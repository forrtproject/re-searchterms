##############################################################
# data-raw/01b_ingest_wiktionary.R                           #
# Ingest English definitions from Wiktionary for FORRT terms #
##############################################################

# Load libraries #
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(readr)
})


# Safety check: To ensure that we are at the root repo #

if (basename(getwd()) == "data-raw") setwd("..")


# Inputs: Use FORRT concepts' names as title candidates for retrieving Wiktionary's data #

in_candidates <- c(
  "data/forrt_definitions_canonical.rds",
  "data/forrt_definitions_clean.rds"
)

in_path <- in_candidates[file.exists(in_candidates)][1]
if (is.na(in_path)) {
  stop("Cannot find FORRT dataset. Run FORRT ingestion first.")
}

concepts <- readRDS(in_path) %>%
  distinct(concept) %>%
  pull(concept)


# Output paths: Specify the output paths and output file names #

dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create("raw/wiktionary/cache", recursive = TRUE, showWarnings = FALSE)

out_rds <- "data/wiktionary_definitions_raw.rds"
out_csv <- "data/wiktionary_definitions_raw.csv"

cache_dir <- "raw/wiktionary/cache"

# # To clear the cache if the script was run previously
# unlink("raw/wiktionary/cache", recursive = TRUE, force = TRUE)
# dir.create("raw/wiktionary/cache", recursive = TRUE, showWarnings = FALSE)


##################
# Wiktionary API #
##################

# Define Wiktionary's search API (Here, the "Action API" from Wiktionary.org is used.) #
api <- "https://en.wiktionary.org/w/api.php"


# Helpers #
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# Slugify texts for filenames
slugify <- function(x) {
  x %>%
    str_to_lower() %>% # Convert texts to lowercase
    str_replace_all("[^a-z0-9]+", "-") %>% # Replace non-alphanumerics with "-"
    str_replace_all("(^-|-$)", "") # Trim leading/trailing hyphens
}

# Extract acronym inside parentheses (Example: "Open Science (OS) --> "OS")
extract_acronym <- function(x) {
  m <- str_match(x, "\\(([^\\)]+)\\)")
  if (is.na(m[1,2])) return(NA_character_)
  str_squish(m[1,2])
}

# Generate candidate page titles for Wiktionary. This is to try multiple reasonable variants to improve match success.
candidate_titles <- function(concept) {
  concept <- str_squish(concept)
  
  no_paren <- str_squish(str_remove(concept, "\\s*\\([^\\)]*\\)\\s*"))
  acro <- extract_acronym(concept)
  depunct <- str_squish(str_replace_all(concept, "[\\(\\)\\[\\]\\{\\}:;,_/\\\\]+", " "))
  
  cands <- c(concept, no_paren, acro, depunct)
  cands <- cands[!is.na(cands) & nzchar(cands)]
  unique(cands)
}

# Remove Wiktionary markup from definitions
strip_wiki_markup <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_) # Clean templates, links, italics, HTML, etc.
  
  x <- as.character(x)
  x <- str_replace_all(x, "\\{\\{[^\\}]*\\}\\}", "")
  x <- str_replace_all(x, "\\[\\[([^\\]|]+)\\|([^\\]]+)\\]\\]", "\\2")
  x <- str_replace_all(x, "\\[\\[([^\\]]+)\\]\\]", "\\1")
  x <- str_replace_all(x, "''+", "")
  x <- str_replace_all(x, "<[^>]+>", "")
  str_squish(x)
}

# Extract English definitions from Wiktionary wikitext. Only grab numbered definitions under the English section.
extract_english_definitions <- function(wikitext, max_defs = 10) {
  if (is.na(wikitext) || !nzchar(wikitext)) return(character(0))
  
  # Find English section in a tolerant way (handles spacing/case)
  m <- str_match(wikitext, "(?is)==\\s*English\\s*==\\s*(.*)")
  if (is.na(m[1,2]) || !nzchar(m[1,2])) return(character(0))
  eng <- m[1,2]
  
  # Cut at next top-level language header
  eng <- str_split(eng, "\n==[^=].*==\n", n = 2, simplify = TRUE)[1,1]
  
  lines <- str_split(eng, "\n", simplify = FALSE)[[1]]
  
  defs <- lines %>%
    keep(~ str_starts(.x, "# ")) %>%
    map_chr(~ str_remove(.x, "^#\\s+")) %>%
    keep(~ nzchar(str_squish(.x)))
  
  if (length(defs) == 0) return(character(0))
  
  defs <- defs[seq_len(min(length(defs), max_defs))]
  defs <- map_chr(defs, strip_wiki_markup)
  defs[nzchar(defs)]
}

# Fetch raw wikitext from Wiktionary API
fetch_wikitext <- function(title) {
  resp <- GET(
    api,
    query = list(
      action = "parse",
      page = title,
      prop = "wikitext",
      format = "json",
      redirects = 1
    ),
    user_agent("Re-SearchTerms (FORRT Edition) – research use")
  )
  
  txt <- content(resp, as = "text", encoding = "UTF-8")
  js <- fromJSON(txt, simplifyVector = FALSE)
  
  if (!is.null(js$error)) {
    return(list(ok = FALSE, title = title, wikitext = NA_character_, error = js$error$info))
  }
  
  list(
    ok = TRUE,
    title = js$parse$title %||% title,
    wikitext = js$parse$wikitext$`*`,
    error = NA_character_
  )
}

# Search Wiktionary if direct title lookup fails
wiki_search_titles <- function(query, n = 5) {
  resp <- GET(
    api,
    query = list(
      action = "query",
      list = "search",
      srsearch = query,
      srlimit = n,
      format = "json"
    ),
    user_agent("Re-SearchTerms (FORRT Edition)")
  )
  
  txt <- content(resp, as = "text", encoding = "UTF-8")
  js <- fromJSON(txt, simplifyVector = TRUE)
  
  # Robust guards
  if (is.null(js$query) || is.null(js$query$search)) return(character(0))
  
  s <- js$query$search
  
  # Sometimes 'search' is not a data.frame; handle both cases
  if (is.data.frame(s)) {
    if (nrow(s) == 0) return(character(0))
    return(s$title)
  }
  
  # If it's a list of results
  if (is.list(s) && length(s) == 0) return(character(0))
  if (is.list(s)) {
    titles <- purrr::map_chr(s, ~ .x$title %||% NA_character_)
    return(titles[!is.na(titles) & nzchar(titles)])
  }
  
  character(0)
}

# Resolve Wiktionary definitions for a term/concept. Here, we try candidate titles directly, then fall back to Wiktionary search.
resolve_wiktionary_definitions <- function(concept) {
  
  # 1) Candidate titles
  for (t in candidate_titles(concept)) {
    f <- fetch_wikitext(t)
    if (!f$ok) next
    defs <- extract_english_definitions(f$wikitext)
    if (length(defs) > 0) {
      return(list(ok = TRUE, title = f$title, method = "candidate", defs = defs))
    }
  }
  
  # 2) Search fallback
  for (t in wiki_search_titles(concept)) {
    f <- fetch_wikitext(t)
    if (!f$ok) next
    defs <- extract_english_definitions(f$wikitext)
    if (length(defs) > 0) {
      return(list(ok = TRUE, title = f$title, method = "search", defs = defs))
    }
  }
  
  list(ok = FALSE, title = NA_character_, method = "none", defs = character(0))
}


# Main extraction loop (one concept --> multiple definitions) #

results <- vector("list", length(concepts))

for (i in seq_along(concepts)) {
  concept <- concepts[i]
  slug <- slugify(concept)
  cache_file <- file.path(cache_dir, paste0(slug, ".json"))
  
  message(sprintf("[%d/%d] %s", i, length(concepts), concept))
  
  # Use cached API result if available
  if (file.exists(cache_file)) {
    cached <- fromJSON(read_file(cache_file), simplifyVector = FALSE)
    res <- cached
    res$defs <- as.character(res$defs %||% character(0))
    
  } else {
    res <- resolve_wiktionary_definitions(concept)
    res_to_cache <- res
    res_to_cache$defs <- as.character(res$defs %||% character(0))
    writeLines(toJSON(res_to_cache, auto_unbox = TRUE, pretty = TRUE), cache_file)
    Sys.sleep(0.6)
  }
  
  # Construct output rows
  if (!isTRUE(res$ok) || length(res$defs) == 0) {
    results[[i]] <- tibble(
      concept = concept,
      term = concept,
      definition = NA_character_,
      source = "Wiktionary",
      source_type = "wiktionary_api",
      page_title = NA_character_,
      hyperlink = NA_character_,
      retrieval_date = as.character(Sys.Date()),
      match_method = res$method %||% "none",
      error = "No English definition found"
    )
  } else {
    results[[i]] <- tibble(
      concept = concept,
      term = concept,
      definition = as.character(res$defs),  # Vector => multiple rows
      source = "Wiktionary",
      source_type = "wiktionary_api",
      page_title = res$title,
      hyperlink = paste0("https://en.wiktionary.org/wiki/", URLencode(res$title)),
      retrieval_date = as.character(Sys.Date()),
      match_method = res$method,
      error = NA_character_
    )
  }
}

# Final formatting + export files #
wik_df <- bind_rows(results) %>%
  group_by(concept) %>%
  mutate(ID = paste0("WIKT_", slugify(concept), "_", row_number())) %>%
  ungroup() %>%
  relocate(ID, .before = concept)

saveRDS(wik_df, out_rds)
write_csv(wik_df, out_csv, na = "")

# Print messages for sanity checks
message("Saved:")
message("  ", out_rds)
message("  ", out_csv)
message("Total rows: ", nrow(wik_df))
message("Concepts with ≥1 definition: ",
        dplyr::n_distinct(wik_df$concept[!is.na(wik_df$definition)]))

