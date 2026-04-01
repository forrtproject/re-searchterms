###########################################################
# data-raw/03_merge_sources.R                             #
# Merge FORRT + Wiktionary + IGI into a single raw corpus #
###########################################################

# Load libraries #
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# Lightweight null-coalescing helper #
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# Specify paths of the input and output files #
FORRT_IN <- "data/forrt_definitions_canonical.rds"
WIKT_IN  <- "data/wiktionary_definitions_raw.rds"
IGI_IN   <- "data/igi_manual_clean.rds"

OUT_RDS  <- "data/definitions_merged_raw.rds"
OUT_CSV  <- "data/definitions_merged_raw.csv"

# Sanity checks #
if (!file.exists(FORRT_IN)) stop("Missing: ", FORRT_IN, "\nRun 02_standardise_schema.R first.")
if (!file.exists(WIKT_IN))  stop("Missing: ", WIKT_IN,  "\nRun 01b_ingest_wiktionary.R first.")
if (!file.exists(IGI_IN))   stop("Missing: ", IGI_IN,   "\nRun 01c_ingest_igi.R first.")

dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Load and standardise FORRT definitions #
forrt <- readRDS(FORRT_IN) %>%
  transmute(
    ID = as.character(ID),
    concept = as.character(concept),
    term = as.character(term),
    definition = as.character(definition),
    source = as.character(source),
    source_type = as.character(source_type),
    hyperlink = as.character(hyperlink),
    retrieval_date = as.character(retrieval_date),
    
    related_terms = as.character(related_terms %||% NA_character_),
    references = as.character(references %||% NA_character_),
    drafted_by = as.character(drafted_by %||% NA_character_),
    reviewed_by = as.character(reviewed_by %||% NA_character_),
    
    page_title = NA_character_,
    match_method = NA_character_,
    error = NA_character_,
    
    source_work_title = NA_character_,
    source_book_title = NA_character_,
    source_work_url = NA_character_
  )

# Load and standardise Wiktionary definitions #
wikt <- readRDS(WIKT_IN) %>%
  transmute(
    ID = as.character(ID),
    concept = as.character(concept),
    term = as.character(term),
    definition = as.character(definition),
    source = as.character(source),
    source_type = as.character(source_type),
    hyperlink = as.character(hyperlink),
    retrieval_date = as.character(retrieval_date),
    
    related_terms = NA_character_,
    references = NA_character_,
    drafted_by = NA_character_,
    reviewed_by = NA_character_,
    
    page_title = as.character(page_title),
    match_method = as.character(match_method),
    error = as.character(error),
    
    source_work_title = NA_character_,
    source_book_title = NA_character_,
    source_work_url = NA_character_
  ) %>%
  filter(!is.na(definition), str_squish(definition) != "")

# Load and standardise IGI definitions #
igi <- readRDS(IGI_IN) %>%
  transmute(
    ID = as.character(ID),
    concept = as.character(concept),
    term = as.character(term),
    definition = as.character(definition),
    source = as.character(source),
    source_type = as.character(source_type),
    hyperlink = as.character(hyperlink),
    retrieval_date = as.character(retrieval_date),
    
    related_terms = NA_character_,
    references = NA_character_,
    drafted_by = NA_character_,
    reviewed_by = NA_character_,
    
    page_title = NA_character_,
    match_method = NA_character_,
    error = NA_character_,
    
    source_work_title = as.character(source_work_title),
    source_book_title = as.character(source_book_title),
    source_work_url = as.character(source_work_url)
  ) %>%
  filter(!is.na(definition), str_squish(definition) != "")

# Merge sources into a single corpus #
merged <- bind_rows(forrt, wikt, igi) %>%
  mutate(
    concept = str_squish(concept),
    term = str_squish(term),
    definition = str_squish(definition),
    source = str_squish(source),
    source_type = str_squish(source_type)
  )

# Conservative deduplication #
merged <- merged %>%
  distinct(concept, term, source, definition, .keep_all = TRUE) %>%
  arrange(concept, source, ID)

# Save merged corpus #
saveRDS(merged, OUT_RDS)
write_csv(merged, OUT_CSV, na = "")

message("Saved:")
message("  ", OUT_RDS)
message("  ", OUT_CSV)

message("Rows: ", nrow(merged))
message("Concepts: ", dplyr::n_distinct(merged$concept))

by_source <- merged %>%
  count(source, name = "rows") %>%
  arrange(desc(rows))

message("Rows by source:")
print(by_source)

defs_per_concept <- table(merged$concept)
message("Definitions per concept (min/median/max): ",
        min(defs_per_concept), " / ",
        median(defs_per_concept), " / ",
        max(defs_per_concept))
