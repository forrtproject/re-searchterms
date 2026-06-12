###############################################
# data-raw/10_build_app_data.R                #
# Build app-ready datasets from merged corpus #
###############################################

# Load libraries #
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(purrr)
  library(reticulate)
  library(mclust)
  library(umap)
  library(tidyr)
})

# Specify paths for the input and output files #
IN <- "data/definitions_merged_clean.rds"
OUT_DIR <- "data/app"

# Quick sanity check to check if the input file was read properly
if (!file.exists(IN)) stop("Missing input: ", IN, "\nRun 04_clean_text.R first.")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Load + basic normalisation #
# Keep:
# - term    = source-facing term label
# - concept = canonical concept label
df <- readRDS(IN) %>%
  mutate(
    term = str_squish(term),
    concept = str_squish(concept),
    source = case_when(
      source %in% c("FORRT", "forrt") ~ "FORRT",
      source %in% c("Wiktionary", "wiktionary") ~ "Wiktionary",
      TRUE ~ source
    )
  )

# Definition-level dataset (df_bydefinition) #
# term    = source-facing term
# concept = canonical concept
df_bydefinition <- df %>%
  transmute(
    ID = ID,
    term = term,                 # source-facing term
    concept = concept,           # canonical term
    source = source,
    source_type = source_type,
    retrieval_date = retrieval_date,
    hyperlink = hyperlink,
    definition = dplyr::coalesce(definition_full, definition),
    def_clean = def_clean,
    source_work_title = source_work_title,
    source_book_title = source_book_title,
    source_work_url = source_work_url
  ) %>%
  group_by(concept) %>%
  mutate(def_ID = paste0(
    str_replace_all(str_to_lower(concept), "[^a-z0-9]+", "_"),
    "_",
    row_number()
  )) %>%
  ungroup() %>%
  mutate(
    def_ID = trimws(def_ID),
    term = trimws(term),
    concept = trimws(concept),
    source = trimws(source)
  )

# Drop empty/junk cleaned definitions #
df_bydefinition <- df_bydefinition %>%
  mutate(def_clean = str_squish(def_clean)) %>%
  mutate(def_clean = na_if(def_clean, "")) %>%
  mutate(def_clean = ifelse(def_clean %in% c(".", "NA", "N/A"), NA_character_, def_clean)) %>%
  filter(!is.na(def_clean))

# Embeddings + similarities #
# Embed each cleaned definition, then compute cosine similarity between every pair
# of definitions within a canonical concept.

sentence_transformers <- import("sentence_transformers")
model <- sentence_transformers$SentenceTransformer("all-MiniLM-L6-v2")

embed_one <- function(x) as.numeric(model$encode(x))
cos_sim <- function(v1, v2) {
  sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
}

# Compute per-definition embeddings
df_bydefinition$emb <- lapply(df_bydefinition$def_clean, function(x) {
  if (is.na(x) || !nzchar(x)) return(rep(NA_real_, 384))
  embed_one(x)
})

# Pairwise cosine similarity within each concept (definition-level network)
pairwise_cosine_similarity <- df_bydefinition %>%
  select(concept, def_ID, emb) %>%
  group_by(concept) %>%
  group_modify(~{
    d <- .x
    concept_value <- .y$concept[[1]]
    
    if (nrow(d) < 2) {
      return(tibble(
        def_ID1 = character(),
        def_ID2 = character(),
        cosine_similarity = numeric()
      ))
    }
    
    comb <- t(combn(d$def_ID, 2))
    
    sims <- apply(comb, 1, function(pair) {
      v1 <- d$emb[[match(pair[1], d$def_ID)]]
      v2 <- d$emb[[match(pair[2], d$def_ID)]]
      if (any(is.na(v1)) || any(is.na(v2))) return(NA_real_)
      cos_sim(v1, v2)
    })
    
    tibble(
      def_ID1 = comb[, 1],
      def_ID2 = comb[, 2],
      cosine_similarity = sims
    )
  }) %>%
  ungroup()

# Term/concept metrics (counts + types/tokens + avg similarity) #
# Here df_byterm is really concept-level, but we keep the filename for app compatibility.

term_counts <- df_bydefinition %>%
  group_by(concept) %>%
  summarise(
    total_definitions = n(),
    .groups = "drop"
  )

# Types/tokens from def_clean (fallback to definition if def_clean missing)
text_col <- if ("def_clean" %in% names(df_bydefinition)) "def_clean" else "definition"

term_types_tokens <- df_bydefinition %>%
  transmute(concept, text = .data[[text_col]]) %>%
  mutate(
    text = ifelse(is.na(text), "", text),
    text = str_squish(text)
  ) %>%
  mutate(words = str_split(text, "\\s+")) %>%
  tidyr::unnest(words) %>%
  mutate(
    words = str_to_lower(words),
    words = str_replace_all(words, "^[^a-z0-9]+|[^a-z0-9]+$", "")
  ) %>%
  filter(words != "", !is.na(words)) %>%
  group_by(concept) %>%
  summarise(
    tokens = n(),
    types = n_distinct(words),
    type_to_token_ratio = ifelse(tokens > 0, types / tokens, NA_real_),
    .groups = "drop"
  )

avg_similarity_by_term <- pairwise_cosine_similarity %>%
  group_by(concept) %>%
  summarise(
    avg_similarity = mean(cosine_similarity, na.rm = TRUE),
    .groups = "drop"
  )

df_byterm <- term_counts %>%
  left_join(term_types_tokens, by = "concept") %>%
  left_join(avg_similarity_by_term, by = "concept") %>%
  rename(term = concept)

# Aggregate embeddings per concept (mean over definitions) for clustering/UMAP #
emb_mat <- do.call(rbind, df_bydefinition$emb)
df_with_emb <- bind_cols(df_bydefinition %>% select(concept), as.data.frame(emb_mat))

by_term_emb <- df_with_emb %>%
  group_by(concept) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

X <- as.matrix(by_term_emb %>% select(where(is.numeric)))

# Fix NAs/NaN/Inf in embedding matrix #
X[!is.finite(X)] <- NA_real_

# Drop concepts with all-NA embeddings
keep_rows <- rowSums(!is.na(X)) > 0
message("Concepts before filtering: ", nrow(X))
message("Dropping concepts with no valid embedding dims: ", sum(!keep_rows))

X_clean <- X[keep_rows, , drop = FALSE]
terms_clean <- by_term_emb$concept[keep_rows]

# Impute remaining NA with column means
col_means <- colMeans(X_clean, na.rm = TRUE)
for (j in seq_len(ncol(X_clean))) {
  idx <- is.na(X_clean[, j])
  if (any(idx)) X_clean[idx, j] <- col_means[j]
}

# Update downstream objects to use filtered + imputed matrix
X <- X_clean
by_term_emb <- by_term_emb %>% filter(concept %in% terms_clean)
df_byterm <- df_byterm %>% filter(term %in% terms_clean)

# Cluster concepts (GMM) #
gmm <- Mclust(X, G = 1:10, modelNames = c("EEE", "VII"))
clusters <- as.factor(gmm$classification)

K <- max(as.integer(clusters))
cluster_names <- setNames(paste("Cluster", seq_len(K)), as.character(seq_len(K)))

df_byterm <- df_byterm %>%
  mutate(
    cluster = clusters,
    cluster_name = cluster_names[as.character(cluster)]
  )

# UMAP for display #
um <- umap(X, n_neighbors = 15, min_dist = 0.01)
umap_df <- as.data.frame(um$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")

umap_df <- bind_cols(
  tibble(term = by_term_emb$concept),
  umap_df,
  df_byterm %>% select(term, cluster_name)
)

# Add cluster labels to definition-level data by canonical concept
df_bydefinition <- df_bydefinition %>%
  left_join(
    df_byterm %>%
      select(term, cluster, cluster_name),
    by = c("concept" = "term")
  )

# Cluster keywords #
# Join cluster labels back to definition-level data, then extract common words per cluster
cluster_keywords <- df_bydefinition %>%
  left_join(
    df_byterm %>% select(term, cluster, cluster_name),
    by = c("concept" = "term")
  ) %>%
  select(concept, cluster, cluster_name, def_clean) %>%
  mutate(def_clean = ifelse(is.na(def_clean), "", def_clean)) %>%
  separate_rows(def_clean, sep = "\\s+") %>%
  filter(def_clean != "", !is.na(def_clean)) %>%
  count(cluster, cluster_name, word = def_clean, sort = TRUE) %>%
  group_by(cluster, cluster_name) %>%
  mutate(rank = row_number()) %>%
  ungroup()


# Export app artifacts #
write_csv(df_bydefinition %>% select(-emb), file.path(OUT_DIR, "df_bydefinition.csv"))
write_csv(df_byterm,                          file.path(OUT_DIR, "df_byterm.csv"))
write_csv(pairwise_cosine_similarity,         file.path(OUT_DIR, "pairwise_cosine_similarity.csv"))
write_csv(umap_df,                            file.path(OUT_DIR, "umap_df.csv"))

message("Saved app datasets to: ", OUT_DIR)

# Quick sanity checks #
message("X dims: ", paste(dim(X), collapse = " x "))
print(summary(gmm$classification))

tmp_term <- read.csv(file.path(OUT_DIR, "df_byterm.csv"))
print(table(tmp_term$cluster_name))

tmp_def <- read.csv(file.path(OUT_DIR, "df_bydefinition.csv"))
print(head(tmp_def[, c("concept", "term", "source", "def_ID")]))
