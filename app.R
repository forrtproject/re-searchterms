######################################
## RE-SEARCHTERMS SHINY APP (FORRT) ##
######################################

# Load libraries
library(shiny)
library(DT)
library(visNetwork)
library(plotly)
library(dplyr)
library(bslib)
library(tidytext)
library(widyr)
library(RColorBrewer)
library(tibble)

# Load datasets 
df_bydefinition <- read.csv("data/df_bydefinition.csv", stringsAsFactors = FALSE)
pairwise_cosine_similarity <- read.csv("data/pairwise_cosine_similarity.csv", stringsAsFactors = FALSE)
df_byterm <- read.csv("data/df_byterm.csv", stringsAsFactors = FALSE)
df_umap <- read.csv("data/umap_df.csv", stringsAsFactors = FALSE)
# 
# # Fix df_umap column names
# df_umap <- df_umap %>%
#   rename(term = `term...1`) %>% 
#   select(-`term...4`)

# Basic hygiene (To standardise column names)
df_bydefinition <- df_bydefinition %>%
  mutate(
    def_ID = trimws(def_ID),
    term   = trimws(term),
    concept = trimws(concept),
    source = trimws(source)
  )

pairwise_cosine_similarity <- pairwise_cosine_similarity %>%
  mutate(
    def_ID1 = trimws(def_ID1),
    def_ID2 = trimws(def_ID2)
  )

####################
## USER INTERFACE ##
####################

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      html, body {
        background-color: #FEFDF6 !important;
        margin: 0;
        padding: 0;
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 14px;
      }
      
      .container-fluid {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }
      
      /* Top brand bar */
      .top-brand-bar {
        background-color: #243C35;
        color: white;
        padding: 10px 20px;
      }
      
      .brand-inner {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 18px;
        flex-wrap: wrap;
      }
      
      .brand-left {
        display: flex;
        align-items: center;
        gap: 14px;
        min-width: 0;
      }
      
      .brand-logo {
        height: 44px;
        width: auto;
        display: block;
      }
      
      .brand-text {
        display: flex;
        flex-direction: column;
        justify-content: center;
        min-width: 0;
      }
      
      .brand-title {
        margin: 0;
        font-size: 22px;
        font-weight: 700;
        line-height: 1.05;
        color: white;
      }
      
      .brand-subtitle {
        margin: 2px 0 0 0;
        font-size: 13px;
        color: rgba(255,255,255,0.9);
        line-height: 1.2;
      }
      
      .brand-right {
        display: flex;
        align-items: center;
        gap: 12px;
        flex-wrap: wrap;
        justify-content: flex-end;
      }
      
      .brand-link {
        color: white !important;
        text-decoration: none !important;
        font-weight: 600;
        font-size: 13px;
        padding: 4px 8px;
        border-radius: 6px;
      }
      
      .brand-link:hover,
      .brand-link:focus {
        color: white !important;
        background-color: rgba(255,255,255,0.10);
        text-decoration: none !important;
      }
      
      .main-content-wrap {
        max-width: 1400px;
        margin: 0 auto;
        padding: 12px 16px 20px 16px;
        background-color: #FEFDF6;
      }
      
      .tab-content,
      .tab-pane {
        background-color: #FEFDF6 !important;
      }
      
      .sidebar-panel-soft {
        background-color: #F4F1E8;
        border: 1px solid #E2DDD0;
        border-radius: 10px;
        padding: 12px;
      }
      
      .tutorial-box {
        margin: 8px 0;
        padding: 8px;
        background-color: #EEF3F0;
        border-left: 4px solid #243C35;
        border-radius: 4px;
      }
      
      .home-card {
        background-color: #F4F1E8;
        border-left: 5px solid #243C35;
        padding: 14px;
        border-radius: 6px;
        min-height: 165px;
      }
      
      .legend-box-orange {
        margin-top: 8px;
        padding: 8px;
        border-left: 4px solid #f39c12;
        background-color: #fff7e6;
        font-size: 12px;
        border-radius: 4px;
      }
      
      .legend-box-green {
        margin-top: 8px;
        padding: 8px;
        border-left: 4px solid #243C35;
        background-color: #EEF3F0;
        font-size: 12px;
        border-radius: 4px;
      }
      
      .shiny-input-container {
        width: 100% !important;
      }
      
      h1 { font-size: 22px; }
      h2 { font-size: 20px; }
      h3 { font-size: 18px; }
      h4 { font-size: 17px; }
      h5 { font-size: 15px; }
      
      p, li, label, .help-block {
        font-size: 13px;
        line-height: 1.4;
      }
      
      /* Dataset table's readability */
      table.dataTable.nowrap th,
      table.dataTable.nowrap td {
        white-space: nowrap !important;
        vertical-align: middle !important;
      }
      
      /* Compact row height */
      table.dataTable.compact tbody th,
      table.dataTable.compact tbody td {
        padding-top: 6px !important;
        padding-bottom: 6px !important;
      }
      
      /* Make the horizontal scrollbar clearly visible */
      .dataTables_wrapper .dataTables_scroll {
        overflow: visible !important;
      }
      
      .dataTables_wrapper .dataTables_scrollBody {
        overflow-x: auto !important;
        overflow-y: auto !important;
        border-bottom: 1px solid #d9d9d9;
      }
      
      /* Style the scrollbar */
      .dataTables_wrapper .dataTables_scrollBody::-webkit-scrollbar {
        height: 10px;
      }
      
      .dataTables_wrapper .dataTables_scrollBody::-webkit-scrollbar-thumb {
        background: #b8b8b8;
        border-radius: 6px;
      }
      
      .dataTables_wrapper .dataTables_scrollBody::-webkit-scrollbar-track {
        background: #ececec;
      }
      
      /* Ensure logo container inherits navbar color */
      .navbar, .navbar * {
        background-color: #243C35 !important;
      }
      
      /* Fix transparent PNG rendering */
      img {
        background: transparent !important;
      }
    "))
  ),
  
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#243C35",
    base_font = font_google("IBM Plex Sans"),
    font_scale = 1.1
  ),
  
  # Top brand bar with all navigation
  tags$div(
    class = "top-brand-bar",
    tags$div(
      class = "brand-inner",
      
      tags$div(
        style = "
    display: flex;
    align-items: center;
    gap: 12px;
  ",
        
        img(
          src = "forrt_logo_inverted.png",
          style = "
      height: 32px;
      width: auto;
      background-color: transparent;
      display: block;
    "
        ),
        
        tags$div(
          h1("Re-SearchTerms", style = "margin: 0; font-size: 22px; color: white;"),
          p("A Living and Expanding Terminology Platform in Collaboration with FORRT",
            style = "margin: 0; font-size: 12px; color: #d9e3dc;")
        )
      ),
      
      tags$div(
        class = "brand-right",
        actionLink("nav_home", "Home", class = "brand-link"),
        actionLink("nav_dataset", "Dataset", class = "brand-link"),
        actionLink("nav_words", "Word-Level", class = "brand-link"),
        actionLink("nav_definitions", "Definitions", class = "brand-link"),
        actionLink("nav_terms", "Terms", class = "brand-link"),
        actionLink("nav_videos", "Videos", class = "brand-link"),
        actionLink("nav_about", "About", class = "brand-link")
      )
    )
  ),
  
  # # Navigation bar under brand bar
  # tags$div(
  #   class = "custom-nav-bar",
  #   tags$div(
  #     class = "custom-nav-inner",
  #     actionLink("nav_home", "Home", class = "custom-nav-link"),
  #     actionLink("nav_dataset", "Dataset", class = "custom-nav-link"),
  #     actionLink("nav_words", "Word-Level Analysis", class = "custom-nav-link"),
  #     actionLink("nav_definitions", "Definition-Level Analysis", class = "custom-nav-link"),
  #     actionLink("nav_terms", "Term-Level Analysis", class = "custom-nav-link")
  #   )
  # ),
  
  # Main content
  div(
    class = "main-content-wrap",
    
    tabsetPanel(
      id = "main_tabs",
      type = "hidden",
      
      # Home
      tabPanel(
        title = "home", value = "home",
        div(
          style = "padding: 10px 10px 30px 10px; max-width: 1200px; margin: auto; background-color: #FEFDF6;",
          
          p(
            tags$em("Re-SearchTerms"), " is an interactive platform for comparing definitions of research concepts across multiple sources, including FORRT, Wiktionary, and academic texts.",
            style = "font-size: 17px; max-width: 950px;"
          ),
          
          p(
            "Use our app to investigate conceptual variation, semantic similarity, and differences in how terms are framed across scholarly communities.",
            style = "font-size: 16px; max-width: 950px; color: #555;"
          ),
          
          tags$hr(),
          
          fluidRow(
            column(
              width = 3,
              div(
                class = "home-card",
                h4("Dataset"),
                p("Browse, filter, and download the full database of definitions and metadata."),
                actionLink("go_dataset", "Open Dataset", style = "font-weight: 600; color: #243C35;")
              )
            ),
            column(
              width = 3,
              div(
                class = "home-card",
                h4("Word-Level Analysis"),
                p("Examine wording patterns and co-occurrence structures across definitions of a concept."),
                actionLink("go_words", "Open Word-Level Analysis", style = "font-weight: 600; color: #243C35;")
              )
            ),
            column(
              width = 3,
              div(
                class = "home-card",
                h4("Definition-Level Analysis"),
                p("Compare individual definitions and inspect semantic similarity across sources."),
                actionLink("go_definitions", "Open Definition-Level Analysis", style = "font-weight: 600; color: #243C35;")
              )
            ),
            column(
              width = 3,
              div(
                class = "home-card",
                h4("Term-Level Analysis"),
                p("Explore relationships between concepts through co-occurrence, clustering, and lexical variation."),
                actionLink("go_terms", "Open Term-Level Analysis", style = "font-weight: 600; color: #243C35;")
              )
            )
          ),
          
          tags$hr(style = "margin-top: 30px;"),
          
          h4("Why use Re-SearchTerms?"),
          tags$ul(
            tags$li("Compare how the same concept is defined across different sources."),
            tags$li("Identify when similar labels refer to different meanings, or different labels refer to similar meanings."),
            tags$li("Inspect semantic similarity between definitions using embedding-based analyses."),
            tags$li("Support careful and transparent terminology choices in research writing.")
          ),
          
          tags$hr(),
          
          h4("Video Tutorials"),
          p("New to the app? Start with our tutorial playlist for guided walkthroughs of each feature."),
          actionLink("go_videos", "Open Video Tutorials", style = "font-weight: 600; color: #243C35; font-size: 16px;")
        )
      ),
      
      # Dataset
      tabPanel(
        title = "dataset", value = "dataset",
        sidebarLayout(
          sidebarPanel(
            class = "sidebar-panel-soft",
            tags$div(
              class = "tutorial-box",
              actionLink("tutorial_dataset", "▶ Watch tutorial for this page",
                         style = "font-weight: 600; color: #243C35;")
            ),
            h4("Filter Dataset"),
            p("Use the filters below to explore the dataset:"),
            tags$ul(
              tags$li("Filter by cluster, term, or source."),
              tags$li("Reset filters to view the entire dataset."),
              tags$li("Download the filtered dataset for offline use.")
            ),
            hr(),
            selectInput("filterCluster", "Cluster Name:", choices = NULL, multiple = TRUE),
            selectInput("filterTerm", "Term:", choices = NULL, multiple = TRUE),
            selectInput("filterSource", "Source:", choices = NULL, multiple = TRUE),
            actionButton("resetFilters", "Reset Filters", icon = icon("redo")),
            br(), br(),
            downloadButton("downloadDatasetFiltered", "Download Filtered Dataset"),
            width = 3
          ),
          mainPanel(
            h4("Dataset Table"),
            DTOutput("dataTable", width = "100%"),
            width = 9
          )
        )
      ),
      
      # Word-Level Analysis
      tabPanel(
        title = "words", value = "words",
        sidebarLayout(
          sidebarPanel(
            class = "sidebar-panel-soft",
            tags$div(
              class = "tutorial-box",
              actionLink("tutorial_words", "▶ Watch tutorial for this page",
                         style = "font-weight: 600; color: #243C35;")
            ),
            width = 3,
            h4("Word-Level Analysis"),
            p("Explore word-level patterns across definitions of a term."),
            tags$ul(
              tags$li("Select a term."),
              tags$li("Adjust the minimum co-occurrence frequency to filter the network."),
              tags$li("If only one definition exists: word-frequency chart; otherwise: co-occurrence network.")
            ),
            br(),
            selectizeInput("selectedTermWord", "Select Term:", choices = NULL, selected = ""),
            sliderInput("frequencyThreshold", "Minimum Co-Occurrence Frequency:", min = 1, max = 20, value = 2),
            helpText(
              "If nothing appears, try lowering the threshold. ",
              "For terms with many words, a lower threshold may create a very dense network and slow down rendering. ",
              "If the network does not load or becomes unresponsive, increase the threshold to simplify the visualisation."
            )
          ),
          mainPanel(
            width = 9,
            h4("Word Co-Occurrence Visualisation"),
            uiOutput("wordCooccurrencePlot", height = "600px")
          )
        )
      ),
      
      # Definition-Level Analysis
      tabPanel(
        title = "definitions", value = "definitions",
        sidebarLayout(
          sidebarPanel(
            class = "sidebar-panel-soft",
            tags$div(
              class = "tutorial-box",
              actionLink("tutorial_definitions", "▶ Watch tutorial for this page",
                         style = "font-weight: 600; color: #243C35;")
            ),
            width = 3,
            h4("Definition-Level Analysis"),
            tags$ul(
              tags$li("Select a term; the network updates automatically."),
              tags$li(HTML("<strong>Edges represent cosine similarity</strong> between definitions based on their embedding vectors.")),
              tags$li("Thicker edges indicate greater similarity in semantic meaning (not just shared words)."),
              tags$li("Click a node to view the full definition."),
              tags$li("Click an edge to compare the two connected definitions in detail.")
            ),
            selectizeInput("selectedDefinitionTerm", "Select Term:", choices = NULL, selected = ""),
            tags$div(
              class = "legend-box-orange",
              tags$strong("Node legend"),
              tags$ul(
                tags$li(HTML("⭐ <strong>Star</strong>: FORRT definitions")),
                tags$li(HTML("🔺 <strong>Triangle</strong>: Wiktionary definitions")),
                tags$li("⚪ Circular nodes: other sources"),
                tags$li("Node colour: cluster membership")
              )
            ),
            tags$div(
              class = "legend-box-green",
              tags$strong("Term note"),
              tags$p(
                tags$strong("Canonical term"), " refers to the concept used to group related definitions in the app."
              ),
              tags$p(
                tags$strong("Source term"), " refers to the exact term name used by the original source for that definition."
              )
            )
          ),
          mainPanel(
            width = 9,
            h4("Definition Relationship Network"),
            visNetworkOutput("definitionNetwork", height = "40vh"),
            h5("Selected Definition Comparison:"),
            uiOutput("edgeComparison"),
            h5("Selected Definition:"),
            textOutput("nodeDefinition"),
            br(),
            strong("Canonical term: "),
            textOutput("nodeConcept"),
            br(),
            strong("Source term: "),
            textOutput("nodeTerm"),
            br(),
            strong("Source: "),
            textOutput("nodeSource"),
            br(),
            strong("Chapter source: "),
            uiOutput("nodeChapterTitle"),
            br(),
            strong("Book source: "),
            uiOutput("nodeBookTitle"),
            br(),
            textOutput("nodeDisplayNote"),
            br(),
            strong("Link: "),
            uiOutput("nodeHyperlink")
          )
        )
      ),
      
      # Term-Level Analysis
      tabPanel(
        title = "terms", value = "terms",
        sidebarLayout(
          sidebarPanel(
            class = "sidebar-panel-soft",
            width = 3,
            h4("Term-Level Analysis"),
            selectInput(
              "termAnalysisMode", "Analysis Mode:",
              choices = c(
                "Word Co-Occurrence Network",
                "Clustering (Sentence Embeddings)",
                "Types & Tokens Analysis"
              )
            ),
            
            conditionalPanel(
              condition = "input.termAnalysisMode == 'Word Co-Occurrence Network'",
              tags$div(
                class = "tutorial-box",
                actionLink("tutorial_term_cooccurrence", "▶ Watch tutorial for this mode",
                           style = "font-weight: 600; color: #243C35;")
              ),
              tags$ul(
                tags$li("Adjust the threshold to filter connections."),
                tags$li("Select a term to highlight.")
              ),
              selectizeInput("selectedTerm", "Highlight Term:", choices = NULL, selected = ""),
              sliderInput("cooccurrenceThreshold", "Minimum Co-Occurrence:", min = 1, max = 50, value = 25)
            ),
            
            conditionalPanel(
              condition = "input.termAnalysisMode == 'Clustering (Sentence Embeddings)'",
              tags$div(
                class = "tutorial-box",
                actionLink("tutorial_term_clustering", "▶ Watch tutorial for this mode",
                           style = "font-weight: 600; color: #243C35;")
              ),
              tags$ul(tags$li("Select a term to highlight on the UMAP plot.")),
              selectizeInput("highlightedTerm", "Highlight Term:", choices = NULL, selected = "")
            ),
            
            conditionalPanel(
              condition = "input.termAnalysisMode == 'Types & Tokens Analysis'",
              tags$div(
                class = "tutorial-box",
                actionLink("tutorial_term_types", "▶ Watch tutorial for this mode",
                           style = "font-weight: 600; color: #243C35;")
              ),
              
              div(
                style = "margin: 10px 0;",
                actionButton(
                  "generateTable", "Generate Table", icon = icon("table"),
                  style = "width: 100%; font-size: 16px; padding: 10px 12px;"
                )
              ),
              
              sliderInput("typesFilter", "Filter by Types:", min = 1, max = 100, value = c(1, 100)),
              sliderInput("tokensFilter", "Filter by Tokens:", min = 1, max = 1000, value = c(1, 1000)),
              sliderInput("ttrFilter", "Filter by Type-to-Token Ratio:", min = 0, max = 1, step = 0.01, value = c(0, 1))
            )
          ),
          
          mainPanel(
            width = 9,
            
            conditionalPanel(
              condition = "input.termAnalysisMode == 'Word Co-Occurrence Network'",
              h4("Word Co-Occurrence Network"),
              visNetworkOutput("termNetwork", height = "60vh"),
              br(),
              uiOutput("tableTitle"),
              DTOutput("cooccurrenceTable", height = "400px")
            ),
            
            conditionalPanel(
              condition = "input.termAnalysisMode == 'Clustering (Sentence Embeddings)'",
              h4("Clustering Visualisation"),
              plotlyOutput("clusteringPlot", height = "400px"),
              uiOutput("closestTermsTableTitle"),
              DTOutput("closestTermsTable", height = "400px")
            ),
            
            conditionalPanel(
              condition = "input.termAnalysisMode == 'Types & Tokens Analysis'",
              h4("Types & Tokens Analysis Visualisation"),
              plotlyOutput("typesTokensPlot", height = "400px"),
              hr(),
              h4("Filtered data table"),
              DTOutput("filteredTable")
            )
          )
        )
      ),
      
      # Video Tutorials
      tabPanel(
        title = "videos", value = "videos",
        div(
          style = "padding: 20px; max-width: 1100px; margin: auto;",
          
          h2("Video Tutorials"),
          
          p(
            "Watch our ", tags$em("Re-SearchTerms"), " video tutorials to learn more about our app's features.",
            "Use the playlist menu in the top-right corner of the video player to browse all available tutorials."
          ),
          
          tags$div(
            style = "
    max-width: 900px;
    margin: 20px auto;
  ",
            tags$iframe(
              width = "100%",
              height = "420",
              src = "https://www.youtube.com/embed/videoseries?list=PLSLpkUCilSvrPyKLTgc8KWtMx7tPiiLVp",
              frameborder = "0",
              allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
              allowfullscreen = NA,
              style = "border-radius: 8px;"
            )
          )
        )
      ),
      
      # About
      tabPanel(
        title = "about", value = "about",
        div(
          style = "padding: 20px; max-width: 900px; margin: auto;",
          
          h4("About ", tags$em("Re-SearchTerms")),
          
          p(
            tags$em("Re-SearchTerms"), " is an interactive Shiny application designed for exploring how research terms in open scholarship, metascience, and research practice are defined across different sources."
          ),
          
          p(
            "The platform enables you to compare definitions, examine relationships between terms and definitions, 
            and analyse patterns in the language used to describe core concepts in research."
          ),
          
          tags$hr(),
          
          h4("From a Research Tool to a Living Platform"),
          
          p(
            tags$em("Re-SearchTerms"), " was originally developed as part of the doctoral dissertation of Anna Yi Leung, 
            which examined conceptual variability and terminological ambiguity in open scholarship and psychological science."
          ),
          
          p(
            "The current version represents an expanded and evolving platform that builds on this initial work. 
            It integrates a larger and more diverse set of definitions, introduces new analytical features 
            (e.g., word-level, definition-level, and embedding-based analyses), and improves the interactivity 
            and accessibility of the original application."
          ),
          
          p(
            "Rather than a static resource, ", tags$em("Re-SearchTerms"), " is being developed as a living and expanding terminology platform. 
            Its goal is to support continuous reflection, discussion, and refinement of how key research concepts are defined and used across disciplines."
          ),
          
          tags$hr(),
          
          h4("Collaboration with FORRT"),
          
          p(
            tags$em("Re-SearchTerms"), " is developed in collaboration with the Framework for Open and Reproducible Research Training (FORRT)."
          ),
          
          p(
            "This app incorporates terminology curated by FORRT (the FORRT Glossary) alongside other sources 
            (e.g., Wiktionary and academic publications), enabling users to explore how definitions vary across communities and contexts."
          ),
          
          p(
            "Ongoing development aims to further expand the dataset, introduce new features, and support community contributions, 
            making the platform a shared resource for researchers, educators, and practitioners. Stay tuned for our newest development!"
          ),
          
          tags$hr(),
          
          h4("Citation"),
          
          p(
            "The development and evaluation of ", tags$em("Re-SearchTerms"), " has been published in ", tags$em("Meta-Psychology"), "."
          ),
          
          p("Publication:"),
          
          p(
            "Anna Yi Leung, Daniel Kristanto, & Xenia Schmalz. (2026, accepted). ",
            tags$a(
              "Re-SearchTerms: A Shiny app for exploring terminology variations in psychology and metascience",
              href = "https://osf.io/preprints/osf/qsp7x_v2",
              target = "_blank"
            ),
            ". ", tags$em("Meta-Psychology"), "."
          ),
          
          tags$hr(),
          
          h4("Project Personnel"),
          
          p(
            strong("Project Lead: "),
            a("Anna Yi Leung", href = "https://annayileung.com", target = "_blank"),
            " (Ludwig-Maximilians-University of Munich, Germany)"
          ),
          
          p(
            strong("Co-Project Lead: "),
            a("Dr. Daniel Kristanto", href = "https://uol.de/psychologie/statistik/daniel-kristanto", target = "_blank"),
            " (Carl von Ossietzky Universität Oldenburg, Germany)"
          ),
          
          p(
            "Our project is open to collaboration, and additional contributors may be acknowledged as the platform evolves."
          ),
          
          tags$hr(),
          
          h4("Acknowledgements"),
          
          p(
            "The initial development of ", tags$em("Re-SearchTerms"),
            " was supported by the ",
            tags$a(
              "META-REP Priority Programme",
              href = "https://www.lmu.de/psy/de/forschung/meta-rep/",
              target = "_blank"
            ),
            " and the German Research Foundation (DFG). ",
            "We thank Dr. Xenia Schmalz for her contributions to the first edition of the app. ",
            "The current version represents an independent, expanded development. We thank Dr. Flavio Azevedo (Director of FORRT) and Dr. Lukas Wallrich (Co-Chair of FORRT) for their technical feedback and for supporting the integration of the project within FORRT."
          )
        )
      )
    )
  )
)

##########
# Server #
##########

server <- function(input, output, session) {
  
  show_tutorial_modal <- function(title_text, video_id) {
    showModal(modalDialog(
      title = title_text,
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$div(
        style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden;",
        tags$iframe(
          src = paste0("https://www.youtube.com/embed/", video_id),
          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: none;",
          allowfullscreen = NA
        )
      )
    ))
  }
  
  # Navigation
  observeEvent(input$nav_home, {
    updateTabsetPanel(session, "main_tabs", selected = "home")
  })
  
  observeEvent(input$nav_dataset, {
    updateTabsetPanel(session, "main_tabs", selected = "dataset")
  })
  
  observeEvent(input$nav_words, {
    updateTabsetPanel(session, "main_tabs", selected = "words")
  })
  
  observeEvent(input$nav_definitions, {
    updateTabsetPanel(session, "main_tabs", selected = "definitions")
  })
  
  observeEvent(input$nav_terms, {
    updateTabsetPanel(session, "main_tabs", selected = "terms")
  })
  
  observeEvent(input$nav_videos, {
    updateTabsetPanel(session, "main_tabs", selected = "videos")
  })
  
  observeEvent(input$nav_about, {
    updateTabsetPanel(session, "main_tabs", selected = "about")
  })
  
  observeEvent(input$go_dataset, {
    updateTabsetPanel(session, "main_tabs", selected = "dataset")
  })
  
  observeEvent(input$go_words, {
    updateTabsetPanel(session, "main_tabs", selected = "words")
  })
  
  observeEvent(input$go_definitions, {
    updateTabsetPanel(session, "main_tabs", selected = "definitions")
  })
  
  observeEvent(input$go_terms, {
    updateTabsetPanel(session, "main_tabs", selected = "terms")
  })
  
  observeEvent(input$go_videos, {
    updateTabsetPanel(session, "main_tabs", selected = "videos")
  })
  
  # Define clusters' colours
  clusters <- sort(unique(df_byterm$cluster_name))
  if (length(clusters) <= 9) {
    pal <- brewer.pal(max(3, length(clusters)), "Set1")[seq_along(clusters)]
  } else {
    pal <- grDevices::hcl.colors(length(clusters), "Set 2")
  }
  cluster_colors <- setNames(pal, clusters)
  
  # Truncate definitions
  truncate_by_source <- function(text, source, n = 90) {
    text <- gsub("\n", " ", text)
    text <- trimws(text)
    
    out <- ifelse(
      is.na(text) | !nzchar(text),
      "",
      text
    )
    
    needs_trunc <- !is.na(source) &
      source == "IGI InfoSci-Dictionary" &
      !is.na(out) &
      nzchar(out) &
      nchar(out) > n
    
    out[needs_trunc] <- paste0(substr(out[needs_trunc], 1, n), "...")
    out
  }
  
  # Truncate definitions from publishers for display
  truncate_igi_only <- function(definition, source, n = 300) {
    if (length(definition) == 0 || is.na(definition[1]) || !nzchar(definition[1])) {
      return("No definition available.")
    }
    
    txt <- trimws(definition[1])
    src <- if (length(source) == 0 || is.na(source[1])) "" else trimws(source[1])
    
    if (identical(src, "IGI InfoSci-Dictionary") && nchar(txt) > n) {
      paste0(substr(txt, 1, n), "...")
    } else {
      txt
    }
  }
  
  #################
  ## DATASET TAB ##
  #################
  
  observeEvent(input$tutorial_dataset, {
    show_tutorial_modal("Dataset Tutorial", "vDkM8qnHB4c")
  })
  
  filtered_data <- reactive({
    dat <- df_bydefinition %>%
      select(-def_clean, -source_type, everything()) %>%
      rename(
        "Definition ID" = def_ID,
        "ID" = ID,
        "Canonical Term" = concept,
        "Source Term" = term,
        "Source" = source,
        "Cluster Name" = cluster_name,
        "Retrieval Date" = retrieval_date,
        "Hyperlink" = hyperlink,
        "Definition" = definition
      )
    
    if (!is.null(input$filterCluster) && length(input$filterCluster) > 0) {
      dat <- dat %>% filter(`Cluster Name` %in% input$filterCluster)
    }
    if (!is.null(input$filterTerm) && length(input$filterTerm) > 0) {
      dat <- dat %>% filter(`Canonical Term` %in% input$filterTerm)
    }
    if (!is.null(input$filterSource) && length(input$filterSource) > 0) {
      dat <- dat %>% filter(`Source` %in% input$filterSource)
    }
    dat
  })
  
  observe({
    updateSelectInput(session, "filterCluster", choices = sort(unique(df_bydefinition$cluster_name)), selected = NULL)
  })
  
  observe({
    tmp <- df_bydefinition
    if (!is.null(input$filterCluster) && length(input$filterCluster) > 0) {
      tmp <- tmp %>% filter(cluster_name %in% input$filterCluster)
    }
    updateSelectInput(session, "filterTerm", choices = sort(unique(tmp$concept)), selected = NULL)
  })
  
  observe({
    tmp <- df_bydefinition
    if (!is.null(input$filterCluster) && length(input$filterCluster) > 0) {
      tmp <- tmp %>% filter(cluster_name %in% input$filterCluster)
    }
    if (!is.null(input$filterTerm) && length(input$filterTerm) > 0) {
      tmp <- tmp %>% filter(term %in% input$filterTerm)
    }
    updateSelectInput(session, "filterSource", choices = sort(unique(tmp$source)), selected = NULL)
  })
  
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "filterCluster", selected = NULL)
    updateSelectInput(session, "filterTerm", selected = NULL)
    updateSelectInput(session, "filterSource", selected = NULL)
  })
  
  output$dataTable <- renderDT({
    datatable(
      filtered_data(),
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        columnDefs = list(
          list(className = "dt-center", targets = "_all"),
          list(width = "140px", targets = 0),  # ID
          list(width = "120px", targets = 1),  # Source Term
          list(width = "120px", targets = 2),  # Canonical Term
          list(width = "100px", targets = 3),  # Source
          list(width = "110px", targets = 4),  # Retrieval Date
          list(width = "220px", targets = 5),  # Hyperlink
          list(width = "500px", targets = 6)   # Definition
        )
      ),
      rownames = FALSE,
      class = "stripe hover compact nowrap"
    )
  })
  
  output$downloadDatasetFiltered <- downloadHandler(
    filename = function() paste0("filtered_dataset_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
  
  #################################
  ## TERM CO-OCCURRENCE NETWORK  ##
  #################################
  
  observeEvent(input$tutorial_term_cooccurrence, {
    show_tutorial_modal("Term-Level: Word Co-Occurrence Network", "7W0Wkz6mQjI")
  })
  
  term_pairs_full <- reactive({
    term_words <- df_bydefinition %>%
      group_by(term) %>%
      summarise(words = list(unique(unlist(strsplit(def_clean, "\\s+")))), .groups = "drop")
    
    term_pairs <- expand.grid(term_words$term, term_words$term, stringsAsFactors = FALSE)
    colnames(term_pairs) <- c("from", "to")
    term_pairs <- term_pairs[term_pairs$from != term_pairs$to, ]
    
    term_pairs <- term_pairs %>%
      rowwise() %>%
      mutate(pair_id = paste(sort(c(from, to)), collapse = "_")) %>%
      distinct(pair_id, .keep_all = TRUE) %>%
      ungroup() %>%
      select(-pair_id)
    
    term_pairs$weight <- mapply(function(x, y) {
      length(intersect(
        term_words$words[[which(term_words$term == x)]],
        term_words$words[[which(term_words$term == y)]]
      ))
    }, term_pairs$from, term_pairs$to)
    
    term_pairs
  })
  
  term_pairs_filtered <- reactive({
    req(input$cooccurrenceThreshold)
    term_pairs_full() %>% filter(weight >= input$cooccurrenceThreshold)
  })
  
  observe({
    cluster_terms <- df_byterm %>%
      filter(!is.na(cluster_name), cluster_name != "", !is.na(term), term != "") %>%
      group_by(cluster_name) %>%
      summarise(terms = list(sort(unique(as.character(term)))), .groups = "drop")
    
    choices_list <- list("(Select a term)" = "")
    for (i in seq_len(nrow(cluster_terms))) {
      choices_list[[cluster_terms$cluster_name[i]]] <- cluster_terms$terms[[i]]
    }
    
    updateSelectizeInput(session, "selectedTerm", choices = choices_list, selected = "", server = TRUE)
    updateSelectizeInput(session, "selectedDefinitionTerm", choices = choices_list, selected = "", server = TRUE)
    updateSelectizeInput(session, "selectedTermWord", choices = choices_list, selected = "", server = TRUE)
    updateSelectizeInput(session, "highlightedTerm", choices = choices_list, selected = "", server = TRUE)
  })
  
  output$termNetwork <- renderVisNetwork({
    edges <- term_pairs_filtered()
    
    nodes <- df_byterm %>%
      select(term, cluster_name) %>%
      distinct() %>%
      rename(id = term) %>%
      mutate(
        label = id,
        group = cluster_name,
        color = cluster_colors[cluster_name],
        size = 15
      )
    
    visNetwork(nodes, edges, width = "100%", height = "800px") %>%
      visNodes(shape = "dot", font = list(size = 18), color = list(highlight = "orange")) %>%
      visEdges(smooth = FALSE, color = list(highlight = "orange")) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE))
  })
  
  observeEvent(input$selectedTerm, {
    req(input$selectedTerm)
    if (input$selectedTerm == "") return()
    
    selected_edges <- term_pairs_full() %>%
      filter(from == input$selectedTerm | to == input$selectedTerm) %>%
      mutate(connected_term = ifelse(from == input$selectedTerm, to, from)) %>%
      filter(!is.na(connected_term) & weight > 0) %>%
      arrange(desc(weight))
    
    # map displayed term labels to canonical concepts
    concept_lookup <- df_bydefinition %>%
      select(term, concept) %>%
      distinct()
    
    cluster_lookup <- df_byterm %>%
      select(concept = term, cluster_name) %>%
      distinct()
    
    cooccurrence_data <- selected_edges %>%
      left_join(concept_lookup, by = c("connected_term" = "term")) %>%
      mutate(concept = ifelse(is.na(concept) | concept == "", connected_term, concept)) %>%
      left_join(cluster_lookup, by = "concept") %>%
      distinct(concept, cluster_name, weight, .keep_all = TRUE) %>%
      select(
        `Canonical Concept` = concept,
        `Cluster Name` = cluster_name,
        `No. of Co-Occurring Words` = weight
      )
    
    visNetworkProxy("termNetwork") %>%
      visSelectNodes(id = input$selectedTerm) %>%
      visFocus(id = input$selectedTerm, scale = 1.5)
    
    output$cooccurrenceTable <- renderDT({
      req(nrow(cooccurrence_data) > 0)
      datatable(
        cooccurrence_data,
        options = list(dom = 't', scrollX = TRUE, scrollY = "400px", paging = FALSE),
        rownames = FALSE
      )
    })
  })
  
  output$tableTitle <- renderUI({
    req(input$selectedTerm)
    if (input$selectedTerm == "") return(NULL)
    h3(paste("Canonical concepts with co-occurring words with", input$selectedTerm, "(frequency in descending order):"))
  })
  
  ######################
  ## CLUSTERING GRAPH ##
  ######################
  
  observeEvent(input$tutorial_term_clustering, {
    show_tutorial_modal("Term-Level: Clustering (Sentence Embeddings)", "Yq9fwFvpJ0Y")
  })
  
  observe({
    df_umap2 <- df_umap %>%
      filter(!is.na(term), term != "", !is.na(cluster_name), cluster_name != "")
    
    dropdown_choices <- split(as.character(df_umap2$term), as.character(df_umap2$cluster_name))
    dropdown_choices <- c(list("(Select a term)" = ""), dropdown_choices)
    
    updateSelectizeInput(session, "highlightedTerm", choices = dropdown_choices, selected = "", server = TRUE)
  })
  
  filtered_umap <- reactive({
    req(input$highlightedTerm)
    df_umap %>% mutate(is_selected = term == input$highlightedTerm)
  })
  
  output$clusteringPlot <- renderPlotly({
    req(filtered_umap())
    d <- filtered_umap()
    
    plot_ly() %>%
      add_trace(
        data = d %>% filter(!is_selected),
        x = ~UMAP1, y = ~UMAP2,
        color = ~cluster_name,
        text = ~paste("Term:", term, "<br>Cluster:", cluster_name),
        hoverinfo = "text",
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.7)
      ) %>%
      add_trace(
        data = d %>% filter(is_selected),
        x = ~UMAP1, y = ~UMAP2,
        color = ~cluster_name,
        text = ~paste("Term:", term, "<br>Cluster:", cluster_name),
        hoverinfo = "text",
        type = "scatter", mode = "markers",
        marker = list(size = 15, opacity = 1, line = list(width = 3, color = "black"))
      ) %>%
      layout(
        title = "Clustering Graph of Terms (Dimensionality Reduced Using UMAP)",
        xaxis = list(title = "UMAP Dimension 1"),
        yaxis = list(title = "UMAP Dimension 2")
      )
  })
  
  output$closestTermsTableTitle <- renderUI({
    if (is.null(input$highlightedTerm) || input$highlightedTerm == "") return(h4("No term selected"))
    h4(paste("Closest terms to:", input$highlightedTerm))
  })
  
  output$closestTermsTable <- renderDT({
    req(input$highlightedTerm)
    if (input$highlightedTerm == "") return(datatable(data.frame()))
    
    # If cosine_similarity exists in df_umap, use it; otherwise just show same-cluster neighbors
    if ("cosine_similarity" %in% names(df_umap)) {
      closest_terms <- df_umap %>%
        filter(term != input$highlightedTerm) %>%
        arrange(desc(cosine_similarity)) %>%
        head(20) %>%
        select(term, cluster_name, cosine_similarity)
    } else {
      this_cluster <- df_umap %>%
        filter(term == input$highlightedTerm) %>%
        pull(cluster_name) %>%
        unique()
      
      closest_terms <- df_umap %>%
        filter(term != input$highlightedTerm, cluster_name %in% this_cluster) %>%
        head(20) %>%
        select(term, cluster_name)
    }
    
    datatable(
      closest_terms,
      options = list(dom = 't', scrollX = TRUE, scrollY = "400px", paging = FALSE),
      rownames = FALSE
    )
  })
  
  ####################
  ## TYPES & TOKENS ##
  ####################
  
  observeEvent(input$tutorial_term_types, {
    show_tutorial_modal("Term-Level: Types & Tokens Analysis", "BJs3ybwKp0M")
  })
  
  observe({
    if (!all(c("types", "tokens", "type_to_token_ratio") %in% names(df_byterm))) return()
    
    updateSliderInput(session, "typesFilter",
                      min = 1, max = max(df_byterm$types, na.rm = TRUE),
                      value = c(1, max(df_byterm$types, na.rm = TRUE))
    )
    updateSliderInput(session, "tokensFilter",
                      min = 1, max = max(df_byterm$tokens, na.rm = TRUE),
                      value = c(1, max(df_byterm$tokens, na.rm = TRUE))
    )
    updateSliderInput(session, "ttrFilter",
                      min = 0, max = max(df_byterm$type_to_token_ratio, na.rm = TRUE),
                      value = c(0, max(df_byterm$type_to_token_ratio, na.rm = TRUE))
    )
  })
  
  filtered3DData <- reactive({
    req(input$typesFilter, input$tokensFilter, input$ttrFilter)
    req(all(c("types", "tokens", "type_to_token_ratio") %in% names(df_byterm)))
    
    df_byterm %>%
      filter(
        types >= input$typesFilter[1], types <= input$typesFilter[2],
        tokens >= input$tokensFilter[1], tokens <= input$tokensFilter[2],
        type_to_token_ratio >= input$ttrFilter[1], type_to_token_ratio <= input$ttrFilter[2]
      )
  })
  
  output$typesTokensPlot <- renderPlotly({
    req(filtered3DData())
    d <- filtered3DData()
    
    plot_ly(
      data = d,
      x = ~tokens, y = ~types, z = ~type_to_token_ratio,
      color = ~cluster_name,
      text = ~paste("Term:", term, "<br>Cluster:", cluster_name),
      hoverinfo = "text",
      type = "scatter3d", mode = "markers",
      marker = list(opacity = 0.8, line = list(width = 1, color = "black"))
    ) %>%
      layout(
        title = list(
          text = "Types, Tokens & Type-to-Token Ratio",
          y = 0.95,
          yanchor = "top"
        ),
        margin = list(
          t = 40,   
          b = 20,
          l = 20,
          r = 20
        ),
        scene = list(
          aspectmode = "cube",
          aspectratio = list(x = 1.2, y = 1.2, z = 0.8),
          xaxis = list(title = "Tokens"),
          yaxis = list(title = "Types"),
          zaxis = list(title = "Type-to-Token Ratio")
        )
      )
  })
  
  observeEvent(input$generateTable, {
    d <- filtered3DData()
    
    output$filteredTable <- renderDT({
      req(nrow(d) > 0)
      datatable(
        d %>% select(term, cluster_name, total_definitions, types, tokens, type_to_token_ratio, avg_similarity),
        options = list(dom = 't', scrollX = TRUE, scrollY = "400px", paging = FALSE),
        rownames = FALSE
      )
    })
  })
  
  ########################
  ## DEFINITION NETWORK ##
  ########################
  
  observeEvent(input$tutorial_definitions, {
    show_tutorial_modal("Definition-Level Analysis Tutorial", "_PvprbwkYQA")
  })
  
  clickedNode <- reactiveVal(NULL)
  
  filteredDefinitions <- reactive({
    req(input$selectedDefinitionTerm)
    if (input$selectedDefinitionTerm == "") return(NULL)
    
    defs_for_term <- df_bydefinition %>%
      filter(concept == input$selectedDefinitionTerm) %>%
      pull(def_ID)
    
    pairwise_filtered <- pairwise_cosine_similarity %>%
      filter(def_ID1 %in% defs_for_term | def_ID2 %in% defs_for_term)
    
    if (nrow(pairwise_filtered) == 0) {
      # single-node fallback
      one_def <- df_bydefinition %>%
        filter(concept == input$selectedDefinitionTerm) %>%
        slice(1) %>%
        select(def_ID, term, cluster_name)
      
      return(list(
        edges = data.frame(from = character(0), to = character(0), weight = numeric(0), width = numeric(0)),
        nodes = data.frame(
          id = one_def$def_ID,
          label = one_def$def_ID,
          title = input$selectedDefinitionTerm,
          cluster_name = one_def$cluster_name,
          color = cluster_colors[one_def$cluster_name],
          stringsAsFactors = FALSE
        )
      ))
    }
    
    truncate_text <- function(x, n = 100) {
      x <- gsub("\n", " ", x)
      x <- trimws(x)
      ifelse(nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
    }
    
    edges <- pairwise_filtered %>%
      filter(!is.na(cosine_similarity)) %>%
      left_join(df_bydefinition, by = c("def_ID1" = "def_ID")) %>%
      rename(def1 = definition, source1 = source, term1 = term) %>%
      left_join(df_bydefinition, by = c("def_ID2" = "def_ID")) %>%
      rename(def2 = definition, source2 = source, term2 = term) %>%
      transmute(
        id = paste0(def_ID1, "___", def_ID2),
        from = def_ID1,
        to = def_ID2,
        weight = cosine_similarity,
        title = paste0(
          "<div style='max-width: 260px; white-space: normal;'>",
          "<b>Cosine similarity:</b> ", round(cosine_similarity, 3), "<br><br>",
          "<b>", def_ID1, " (", source1, "):</b><br>",
          truncate_by_source(def1, source1, 90), "<br><br>",
          "<b>", def_ID2, " (", source2, "):</b><br>",
          truncate_by_source(def2, source2, 90),
          "</div>"
        )
      )
    
    if (nrow(edges) > 0 && max(edges$weight) > min(edges$weight)) {
      edges <- edges %>%
        mutate(width = 2 + (weight - min(weight)) / (max(weight) - min(weight)) * 15)
    } else {
      edges$width <- 2
    }
    
    node_ids <- unique(c(edges$from, edges$to))
    
    nodes <- df_bydefinition %>%
      filter(def_ID %in% c(edges$from, edges$to) | concept == input$selectedDefinitionTerm) %>%
      transmute(
        id = def_ID,
        label = def_ID,
        title = paste0(
          "<b>Canonical term:</b> ", concept, "<br>",
          "<b>Source term:</b> ", term, "<br>",
          "<b>Source:</b> ", source
        ),
        cluster_name = cluster_name,
        source = source,
        is_forrt = tolower(source) == "forrt"
      ) %>%
      mutate(
        color.background = unname(cluster_colors[cluster_name]),
        # give FORRT a strong outline
        color.border = ifelse(is_forrt, "black", "gray"),
        borderWidth = ifelse(is_forrt, 4, 1),
        size = ifelse(is_forrt, 28, 20),
        shape = case_when(
          tolower(source) == "forrt" ~ "star",
          tolower(source) == "wiktionary" ~ "triangle",
          TRUE ~ "dot"
        )
      )
    
    list(edges = edges, nodes = nodes)
  })
  
  output$definitionNetwork <- renderVisNetwork({
    net <- filteredDefinitions()
    req(net)
    
    visNetwork(net$nodes, net$edges, width = "100%", height = "500px") %>%
      visNodes(shape = "dot", size = 20, font = list(size = 16)) %>%
      visEdges(color = list(color = "gray", highlight = "pink"), smooth = TRUE, width = ~width) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
      visInteraction(hover = TRUE, tooltipDelay = 0, dragNodes = FALSE) %>%
      visPhysics(stabilization = TRUE, solver = "forceAtlas2Based") %>%
      visEvents(
        click = "function(params) {
    if (params.nodes.length > 0) {
      Shiny.setInputValue('clicked_node', params.nodes[0], {priority: 'event'});
    }
    if (params.edges.length > 0) {
      Shiny.setInputValue('clicked_edge', params.edges[0], {priority: 'event'});
    }
  }"
      )
  })
  
  observeEvent(input$clicked_node, {
    clickedNode(input$clicked_node)
    if (!is.null(input$clicked_node) && input$clicked_node != "") {
      visNetworkProxy("definitionNetwork") %>%
        visUpdateNodes(nodes = data.frame(id = input$clicked_node, color = "orange"))
    }
  })
  
  output$nodeConcept <- renderText({
    req(clickedNode())
    x <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      pull(concept)
    if (length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) "Unknown" else x[1]
  })
  
  output$nodeTerm <- renderText({
    req(clickedNode())
    x <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      pull(term)
    if (length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) "Unknown" else x[1]
  })
  
  output$nodeDefinition <- renderText({
    req(clickedNode())
    
    row <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      slice(1)
    
    if (nrow(row) == 0) return("No definition available.")
    
    truncate_igi_only(
      definition = row$definition,
      source = row$source,
      n = 300
    )
  })
  
  output$nodeChapterTitle <- renderUI({
    req(clickedNode())
    
    row <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      slice(1)
    
    if (nrow(row) == 0) return(NULL)
    if (!identical(trimws(row$source[1]), "IGI InfoSci-Dictionary")) return(tags$span("Not applicable"))
    
    if (is.na(row$source_work_title[1]) || !nzchar(trimws(row$source_work_title[1]))) {
      return(tags$span("Not available"))
    }
    
    if (!is.na(row$source_work_url[1]) && nzchar(trimws(row$source_work_url[1]))) {
      tags$a(row$source_work_title[1], href = row$source_work_url[1], target = "_blank", rel = "noopener")
    } else {
      tags$span(row$source_work_title[1])
    }
  })
  
  output$nodeBookTitle <- renderUI({
    req(clickedNode())
    
    row <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      slice(1)
    
    if (nrow(row) == 0) return(NULL)
    if (!identical(trimws(row$source[1]), "IGI InfoSci-Dictionary")) return(tags$span("Not applicable"))
    
    if (is.na(row$source_book_title[1]) || !nzchar(trimws(row$source_book_title[1]))) {
      return(tags$span("Not available"))
    }
    
    tags$span(row$source_book_title[1])
  })
  
  output$nodeDisplayNote <- renderText({
    req(clickedNode())
    
    row <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      slice(1)
    
    if (nrow(row) == 0) return("")
    
    if (identical(trimws(row$source[1]), "IGI InfoSci-Dictionary")) {
      "IGI definitions are shown in truncated form here. Please use the source link below for the original entry."
    } else {
      ""
    }
  })
  
  output$nodeSource <- renderText({
    req(clickedNode())
    src <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      pull(source)
    
    if (length(src) == 0 || is.na(src) || !nzchar(src)) return("Unknown")
    src
  })
  
  output$nodeDisplayNote <- renderText({
    req(clickedNode())
    
    row <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      slice(1)
    
    if (nrow(row) == 0) return("")
    
    if (identical(trimws(row$source[1]), "IGI InfoSci-Dictionary")) {
      "IGI definitions are shown in truncated form here. Please use the source link below for the original entry."
    } else {
      ""
    }
  })
  
  output$nodeHyperlink <- renderUI({
    req(clickedNode())
    url <- df_bydefinition %>%
      filter(def_ID == clickedNode()) %>%
      pull(hyperlink)
    
    if (length(url) == 0 || is.na(url) || !nzchar(url)) {
      return(tags$span("No link available"))
    }
    
    tags$div(
      tags$a("Open original source", href = url, target = "_blank", rel = "noopener"),
      tags$br(),
      tags$small(url)
    )
  })
  
  output$edgeComparison <- renderUI({
    req(input$clicked_edge)
    
    ids <- strsplit(input$clicked_edge, "___", fixed = TRUE)[[1]]
    if (length(ids) != 2) return(NULL)
    
    def1_id <- ids[1]
    def2_id <- ids[2]
    
    edge_row <- pairwise_cosine_similarity %>%
      filter(
        (def_ID1 == def1_id & def_ID2 == def2_id) |
          (def_ID1 == def2_id & def_ID2 == def1_id)
      ) %>%
      slice(1)
    
    row1 <- df_bydefinition %>% filter(def_ID == def1_id) %>% slice(1)
    row2 <- df_bydefinition %>% filter(def_ID == def2_id) %>% slice(1)
    
    if (nrow(edge_row) == 0 || nrow(row1) == 0 || nrow(row2) == 0) {
      return(tags$span("No comparison available."))
    }
    
    def1_text <- truncate_igi_only(row1$definition, row1$source, 300)
    def2_text <- truncate_igi_only(row2$definition, row2$source, 300)
    
    div(
      style = "padding: 12px; background-color: #f8f8f4; border: 1px solid #ddd; border-radius: 6px;",
      
      tags$p(tags$strong("Cosine similarity: "), round(edge_row$cosine_similarity[1], 3)),
      
      tags$p(
        tags$strong(def1_id), " (", row1$source[1], "; source term: ", row1$term[1], ")"
      ),
      tags$p(def1_text),
      
      tags$p(
        tags$strong(def2_id), " (", row2$source[1], "; source term: ", row2$term[1], ")"
      ),
      tags$p(def2_text)
    )
  })
  
  ################################
  ## WORD CO-OCCURRENCE (TERM)  ##
  ################################
  
  observeEvent(input$tutorial_words, {
    show_tutorial_modal("Word-Level Analysis Tutorial", "aqb2jiOws8Y")
  })
  
  wordCooccurrence <- reactive({
    req(input$selectedTermWord)
    req(input$frequencyThreshold)
    
    defs <- df_bydefinition %>%
      filter(concept == input$selectedTermWord) %>%
      mutate(def_clean = trimws(as.character(def_clean))) %>%
      filter(!is.na(def_clean) & nzchar(def_clean)) %>%
      transmute(def_id = def_ID, text = def_clean)
    
    # Show message if no usable cleaned definition is available for a given term
    if (nrow(defs) == 0) {
      return(list(type = "empty", data = NULL, msg = "No usable cleaned definition text for this term."))
    }
    
    tokens <- tibble::tibble(def_id = defs$def_id, text = defs$text) %>%
      tidytext::unnest_tokens(word, text)
    
    if (nrow(tokens) == 0) {
      return(list(type = "empty", data = NULL, msg = "No tokens extracted for this term."))
    }
    
    # 1 definition => frequency chart
    if (dplyr::n_distinct(defs$def_id) == 1) {
      freqs <- tokens %>%
        count(word, sort = TRUE) %>%
        slice_head(n = 30)
      return(list(type = "frequency", data = freqs))
    }
    
    # 2+ definitions => co-occurrence network across definitions of the same term
    cooc <- tokens %>%
      widyr::pairwise_count(word, def_id, sort = TRUE) %>%
      filter(n >= input$frequencyThreshold)
    
    if (nrow(cooc) == 0) {
      return(list(type = "empty", data = NULL,
                  msg = "No co-occurrences at this threshold. Try lowering the slider."))
    }
    
    list(type = "network", data = cooc)
  })
  
  output$frequencyPlot <- renderPlotly({
    res <- wordCooccurrence()
    req(res)
    req(res$type == "frequency")
    
    selected_cluster <- df_bydefinition %>%
      filter(concept == input$selectedTermWord) %>%
      pull(cluster_name) %>%
      unique()
    
    bar_color <- if (length(selected_cluster) > 0 && selected_cluster[1] %in% names(cluster_colors)) {
      cluster_colors[selected_cluster[1]]
    } else "gray"
    
    plot_ly(
      data = res$data,
      x = ~reorder(word, -n),
      y = ~n,
      type = "bar",
      marker = list(color = bar_color),
      text = ~paste("Frequency:", n),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Word Frequency for Term:", input$selectedTermWord),
        xaxis = list(title = "Words"),
        yaxis = list(title = "Frequency", tickformat = ",d")
      )
  })
  
  output$cooccurrenceNetwork <- renderVisNetwork({
    res <- wordCooccurrence()
    req(res)
    req(res$type == "network")
    
    coocc <- res$data
    nodes <- tibble(id = unique(c(coocc$item1, coocc$item2))) %>%
      mutate(label = id, size = 10)
    
    selected_cluster <- df_bydefinition %>%
      filter(term == input$selectedTermWord) %>%
      pull(cluster_name) %>%
      unique()
    
    node_color <- if (length(selected_cluster) > 0 && selected_cluster[1] %in% names(cluster_colors)) {
      cluster_colors[selected_cluster[1]]
    } else "gray"
    
    nodes <- nodes %>% mutate(color = node_color)
    
    edges <- coocc %>%
      rename(from = item1, to = item2, width = n) %>%
      mutate(title = paste("Co-occurrence count:", width))
    
    visNetwork(nodes, edges) %>%
      visNodes(shape = "dot", color = list(background = nodes$color, border = "black"), font = list(size = 14)) %>%
      visEdges(smooth = TRUE, color = list(color = "gray", highlight = "orange")) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>%
      visInteraction(navigationButtons = TRUE)
  })
  
  output$wordCooccurrencePlot <- renderUI({
    result <- wordCooccurrence()
    
    if (result$type == "frequency") {
      plotlyOutput("frequencyPlot", height = "600px")
    } else if (result$type == "network") {
      visNetworkOutput("cooccurrenceNetwork", height = "600px")
    } else {
      div(style = "padding: 10px; color: #555;", result$msg)
    }
  })
}

#############
## RUN APP ##
#############
shinyApp(ui = ui, server = server)
