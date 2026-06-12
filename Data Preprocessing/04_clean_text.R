#############################################################
# data-raw/04_clean_text.R                                  #
# Clean text fields for downstream analysis + Shiny app use #
#############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tm)
})

in_path  <- "data/definitions_merged_raw.rds"
out_path <- "data/definitions_merged_clean.rds"

if (!file.exists(in_path)) {
  stop("Missing input: ", in_path, "\nRun 03_merge_sources.R first.")
}

df <- readRDS(in_path)

# Clean text for NLP analyses
preprocess_text <- function(text, remove_stopwords = TRUE) {
  if (is.na(text) || !nzchar(text)) return(NA_character_)
  
  text <- str_replace_all(text, "[\r\n]", " ")
  # text <- str_replace_all(text, regex("^definition\\s*:\\s*", ignore_case = TRUE), "")
  # text <- str_replace_all(text, regex("learn more in:.*$", ignore_case = TRUE), "")
  text <- str_replace_all(text, "\\bet al\\b", "")
  text <- str_replace_all(text, "\\be\\.g\\.\\b", " ")
  text <- str_replace_all(text, "http\\S+", "")
  
  # Keep letters + numbers for statistical / technical terms
  text <- str_replace_all(text, "[^A-Za-z0-9]+", " ")
  
  if (remove_stopwords) {
    tokens <- unlist(str_split(text, "\\s+"))
    tokens <- tokens[nzchar(tokens)]
    tokens <- tokens[!tolower(tokens) %in% stopwords("en")]
    text <- paste(tokens, collapse = " ")
  }
  
  text <- tolower(text)
  text <- str_squish(text)
  
  if (!nzchar(text)) return(NA_character_)
  text
}

# Clean text for UI display only
preprocess_text_full <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NA_character_)
  
  text <- str_replace_all(text, "[\r\n]", " ")
  text <- str_replace_all(text, regex("^definition\\s*:\\s*", ignore_case = TRUE), "")
  text <- str_replace_all(text, regex("learn more in:.*$", ignore_case = TRUE), "")
  text <- str_replace_all(text, "http\\S+", "")
  text <- str_squish(text)
  
  if (!nzchar(text)) return(NA_character_)
  text
}

df2 <- df %>%
  mutate(
    definition_full = vapply(definition, preprocess_text_full, character(1)),
    def_clean = vapply(definition, preprocess_text, character(1), remove_stopwords = TRUE)
  )

saveRDS(df2, out_path)

message("Saved cleaned merged dataset: ", out_path)
message("Rows: ", nrow(df2))
message("Non-missing def_clean: ", sum(!is.na(df2$def_clean)))

d <- readRDS(out_path)
c(
  rows = nrow(d),
  concepts = dplyr::n_distinct(d$concept),
  defs_nonmissing = sum(!is.na(d$definition)),
  defclean_nonmissing = sum(!is.na(d$def_clean))
)
d %>% count(source)
