

ui <- navbarPage(
  titlePanel("Tweet Transformer")
  ,
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", label = "Upload File",
                multiple = FALSE,
                accept = c(".json", ".jsonl", ".gz", ".zip")),
      
      shinyjs::useShinyjs(),
      verbatimTextOutput("text"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css"),
      tags$head(tags$style(HTML("
                            #text {
                              font-family: 'Fira Code Retina', monospace;
                              color: red;
                            }
                            ")))

    )
    ,
    mainPanel(
      # "face",
      shiny::htmlOutput("bq")
      ,
    
      DT::dataTableOutput("tweet_DT") %>% shinycssloaders::withSpinner()
    )
    
    
  )
)