# ########################################################
# data-raw/01_ingest_forrt.R                             #
# Ingest FORRT glossary (English) from GitHub repository #
##########################################################

# Loading libraries #
suppressPackageStartupMessages({
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})


# Safety check: To ensure we are at repo root #
if (basename(getwd()) == "data-raw") {
  setwd("..")
}

if (!dir.exists("data-raw")) {
  stop("Please run this script from the re-searchterms-data project root.")
}


# Configuration #

FORRT_EN_DIR <- file.path(
  "raw", "forrt", "forrtproject.github.io",
  "content", "glossary", "english"
)

OUTPUT_RDS <- file.path("data", "forrt_definitions_raw.rds")
OUTPUT_CSV <- file.path("data", "forrt_definitions_raw.csv")

FORRT_BASE_URL <- "https://forrt.org"


# Check that the FORRT English directory exists

if (!dir.exists(FORRT_EN_DIR)) {
  stop(
    "Cannot find FORRT English directory:\n  ",
    normalizePath(FORRT_EN_DIR, winslash = "/", mustWork = FALSE),
    "\n\nExpected structure:\n",
    "re-searchterms-data/\n",
    "  raw/forrt/forrtproject.github.io/content/glossary/english/"
  )
}

dir.create("data", showWarnings = FALSE, recursive = TRUE)


# Helper functions #

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

read_json_md <- function(path) {
  txt <- read_file(path)
  txt <- str_replace(txt, "^\ufeff", "")  # remove BOM if present
  fromJSON(txt)
}

safe_collapse <- function(x, sep = "; ") {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  paste(as.character(x), collapse = sep)
}

make_hyperlink <- function(aliases) {
  if (is.null(aliases) || length(aliases) == 0) return(NA_character_)
  a <- as.character(aliases[[1]])
  if (!nzchar(a)) return(NA_character_)
  if (str_detect(a, "^https?://")) return(a)
  if (!str_starts(a, "/")) a <- paste0("/", a)
  paste0(FORRT_BASE_URL, a)
}


# Ingest FORRT glossary #

files <- list.files(
  FORRT_EN_DIR,
  pattern = "\\.md$",
  full.names = TRUE
)

# Remove index file
files <- files[basename(files) != "_index.md"]

if (length(files) == 0) {
  stop("No FORRT glossary .md files found.")
}

message("Found ", length(files), " FORRT English glossary files.")

parse_one <- function(path) {
  x <- read_json_md(path)
  
  tibble(
    ID             = paste0("FORRT_", str_replace(basename(path), "\\.md$", "")),
    concept        = x$title %||% NA_character_,
    term           = x$title %||% NA_character_,
    definition     = x$definition %||% NA_character_,
    source         = "FORRT",
    source_type    = "forrt_github",
    language       = x$language %||% "english",
    hyperlink      = make_hyperlink(x$aliases),
    related_terms  = safe_collapse(x$related_terms),
    references     = safe_collapse(x$references),
    drafted_by     = safe_collapse(x$drafted_by),
    reviewed_by    = safe_collapse(x$reviewed_by),
    file_name      = basename(path),
    retrieval_date = as.character(Sys.Date())
  )
}

forrt_df <- map_dfr(files, possibly(parse_one, otherwise = NULL))


# Basic cleanup #

forrt_df <- forrt_df %>%
  mutate(
    concept    = str_squish(concept),
    term       = str_squish(term),
    definition = str_squish(definition)
  ) %>%
  filter(!is.na(concept), !is.na(definition))

message("Parsed entries: ", nrow(forrt_df))


# Save outputs #

saveRDS(forrt_df, OUTPUT_RDS)
write_csv(forrt_df, OUTPUT_CSV, na = "")

# Print messages for checking
message("Saved outputs:")
message("  ", OUTPUT_RDS)
message("  ", OUTPUT_CSV)
