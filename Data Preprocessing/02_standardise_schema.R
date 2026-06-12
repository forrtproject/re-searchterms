######################################################
# data-raw/02_standardise_schema.R                   #
# Standardise FORRT raw data into a canonical schema #
######################################################

# Load libraries #
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# Specify the paths of the input and output files #
in_path  <- "data/forrt_definitions_raw.rds"
out_path <- "data/forrt_definitions_canonical.rds"

# Create dataframe #
df <- readRDS(in_path)

# Canonical columns (align with the scripts of Re-SearchTerms 1.0):
# concept, definition, source, term, ID, hyperlink, references, etc.
df2 <- df %>%
  transmute(
    # Keep historical naming convention
    concept = concept,
    term = term,
    definition = definition,
    
    # Provenance
    source = source,                # e.g., "FORRT"
    source_type = source_type,      # e.g., "forrt_github"
    language = language,
    hyperlink = hyperlink,
    retrieval_date = retrieval_date,
    
    # Metadata (optional but useful)
    related_terms = related_terms,
    references = references,
    drafted_by = drafted_by,
    reviewed_by = reviewed_by,
    
    # Stable IDs
    ID = ID,
    file_name = file_name
  ) %>%
  mutate(
    concept = str_squish(concept),
    term = str_squish(term),
    definition = str_squish(definition)
  )

# Save output #
saveRDS(df2, out_path)
message("Saved canonical dataset: ", out_path)
