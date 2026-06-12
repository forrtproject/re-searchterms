###############################################
# data-raw/01c_ingest_igi.R                   #
# Clean manually collected IGI definitions    #
###############################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

IN  <- "raw/igi/igi_manual.csv"
OUT <- "data/igi_manual_clean.rds"

if (!file.exists(IN)) stop("Missing input file: ", IN)

igi <- read_csv(IN, show_col_types = FALSE)

# helper to create clean IDs from source-facing term labels
make_slug <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

igi_clean <- igi %>%
  mutate(
    concept = str_squish(concept),
    term = str_squish(term),
    source = str_squish(source),
    source_type = str_squish(source_type),
    definition = str_squish(definition),
    chapter_title = str_squish(chapter_title),
    book_title = str_squish(book_title),
    hyperlink = str_squish(hyperlink),
    source_url = str_squish(source_url),
    
    concept = na_if(concept, ""),
    term = na_if(term, ""),
    definition = na_if(definition, ""),
    chapter_title = na_if(chapter_title, ""),
    book_title = na_if(book_title, ""),
    hyperlink = na_if(hyperlink, ""),
    source_url = na_if(source_url, "")
  ) %>%
  # Keep only rows with an actual definition
  filter(!is.na(definition)) %>%
  # If source-facing term is missing, fall back to canonical concept
  mutate(
    term = coalesce(term, concept),
    
    # standardise source labels
    source = case_when(
      source %in% c("InfoScipedia", "IGI InfoScipedia", "IGI InfoSci-Dictionary") ~ "IGI InfoSci-Dictionary",
      TRUE ~ source
    ),
    source_type = "publisher_dictionary",
    
    # convert date to ISO format
    retrieval_date = suppressWarnings(as.Date(retrieval_date, format = "%d/%m/%Y")),
    retrieval_date = as.character(retrieval_date)
  ) %>%
  # create clean IDs from the source-facing term
  mutate(term_slug = make_slug(term)) %>%
  group_by(term_slug) %>%
  mutate(
    ID = paste0("IGI_", term_slug, "_", row_number())
  ) %>%
  ungroup() %>%
  transmute(
    ID = ID,
    term = term,                     # source-facing term
    concept = concept,               # canonical term
    source = source,
    source_type = source_type,
    retrieval_date = retrieval_date,
    hyperlink = hyperlink,
    definition = definition,
    def_clean = definition,          # cleaned further in 04_clean_text.R
    source_work_title = chapter_title,
    source_book_title = book_title,
    source_work_url = source_url
  )

saveRDS(igi_clean, OUT)

message("Saved cleaned IGI data to: ", OUT)
message("Rows kept: ", nrow(igi_clean))
print(head(igi_clean, 10))
