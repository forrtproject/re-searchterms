# Re-SearchTerms

***Re-SearchTerms*** is an interactive R Shiny application designed for exploring how research terms in open scholarship, metascience, and research practice are defined across different sources.

The platform enables users to:

* compare definitions across sources
* examine relationships between terms and definitions
* analyse linguistic and semantic patterns in how core research concepts are described

---

## Project Background

*Re-SearchTerms* was originally developed as part of the doctoral dissertation of Anna Yi Leung, which examined conceptual variability and terminological ambiguity in open scholarship and psychological science.

In collaboration with FORRT, the current version represents an expanded and evolving platform that aims to:

* expand the dataset in alignment with the development of the FORRT Glossary
* introduce new analytical features
* support community contributions

---

## App Features

* Definition-level comparison across sources
* Word co-occurrence network visualisation across definitions of terms
* Concept-level network analysis (e.g., cluster graphs, linguistic feature comparisons)

---

## Repository Structure

```
re-searchterms/
│
├── app.R                  # Main Shiny application
│
├── data/                  # Processed datasets used by the app
│   ├── df_bydefinition.csv
│   ├── df_byterm.csv
│   ├── pairwise_cosine_similarity.csv
│   └── umap_df.csv
│
├── data-raw/              # Data pre-processing pipeline scripts
│   ├── 01_ingest_forrt.R
│   ├── 01b_ingest_wiktionary.R
│   ├── 01c_ingest_igi.R
│   ├── 02_standardise_schema.R
│   ├── 03_merge_sources.R
│   ├── 04_clean_text.R
│   └── 10_build_app_data.R
│
├── www/                   # Static assets
│   └── forrt_logo.jpg
│
├── README.md
└── .gitignore            
```

---

## Data Pre-Processing Pipeline

The full data pipeline is documented in `data-raw/`.

Pipeline steps include:

1. Data ingestion (FORRT, Wiktionary, IGI)
2. Schema standardisation
3. Merging across sources
4. Text cleaning and preprocessing
5. Embedding and similarity computation
6. Construction of app-ready datasets

To rebuild the datasets:

```r
source("data-raw/10_build_app_data.R")
```

---

## Data Availability

Raw datasets are not included due to size and/or source restrictions.

To reproduce the dataset:

* Access original sources:

  * FORRT glossary
  * Wiktionary
  * IGI Global: InfoScipedia

* Run scripts in `data-raw/`

Processed datasets used by the app are included in:

```
data
```

---

## Project Personnel

**Project Lead**
Anna Yi Leung (Ludwig-Maximilians-University of Munich, Germany)

**Co-Project Lead**
Dr. Daniel Kristanto (Carl von Ossietzky Universität Oldenburg, Germany)

The project is open to collaboration, and additional contributors will be acknowledged as the platform evolves.

---

## Citation

The development and evaluation of *Re-SearchTerms* has been accepted for publication:

**Leung, A. Y., Kristanto, D., & Schmalz, X. (2026, accepted).** Re-SearchTerms: A Shiny app for exploring terminology variations in psychology and metascience. ***Meta-Psychology***.

Citation details will be updated upon publication.

For a detailed description of the conceptual framework, methods, and development of the app, please refer to our preprint:  
[https://osf.io/preprints/osf/qsp7x_v2]

---

## Acknowledgements

The initial development of *Re-SearchTerms* was supported by the META-REP Priority Programme and German Research Foundation (DFG).

We thank **Dr. Xenia Schmalz** for her contributions to the first edition of the app.

The current version represents an independent and expanded development. We thank Dr. Flavio Azevedo (Director of FORRT) and Dr. Lukas Wallrich (Co-Chair of FORRT) for their technical feedback and for supporting the integration of the project within FORRT.

---

## License

This project is licensed under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license unless stated otherwise.

---

