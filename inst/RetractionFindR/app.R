library(shiny)
library(retractionfindr)
library(tidyverse)
library(synthesisr)
library(waiter)
options(shiny.maxRequestSize = 30 * 1024^2) # Set max file size to 30MB

ui <- fluidPage(
  useWaiter(),
  titlePanel(
    tagList(
      icon("search", style = "color:aqua"), # Magnifying glass icon
      "RetractionFindR"
    ),
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("ris_file", "Upload .ris file", accept = ".ris"),
      actionButton("check_btn", "Check for Retractions"),
      br(), br(),
      selectInput("file_format", "Select download format:",
        choices = c("CSV" = "csv", "RIS" = "ris"),
        selected = "csv"
      ),
      br(), br(),
      downloadButton("download_retracted", "Download Retracted"),
      downloadButton("download_nonretracted", "Download Non-Retracted")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction",
          h3("Welcome to RetractionFindR"),
          p("The number of retracted scientific publications has been increasing over the years. In 2023, more than 10,000 research papers were retracted, setting a new record (Van Noorden 2023). Some suggest this is only the ‘tip of the iceberg’ in terms of fraudulent research that is published each year. The escalation of papers that are bogus has escalated, in part fueled by the work of ‘paper mills’ (businesses selling authorship on fabricated papers)."),
          p("These present a particular threat to the reliability of systematic reviews, which aim to identify and synthesise all relevant evidence. Including fraudulent research will undermine the credibility of the review."),
          p("Identifying problematic studies is the goal of the INSTPECT-SR tool which aims to support reviewers identifying studies that lack veracity. One requirement of the tool is that reviewers locate retracted studies."),
          p("The methods of handling retracted studies are variable across different databases. A retracted study may remain on a database, and while a note may be added this might not be obvious to the reviewer."),
          p("This tool enables reviewers to readily identify those studies that have been retracted."),
          p("Follow these steps to locate retracted studies!"),
          p("1. Upload a .ris file of studies"),
          p("2. Once upload is complete, choose ‘Check for Retractions’"),p("3. The number of retracted and non-retracted studies will be given"),p("4.These can download can be either .csv or .ris format"), p("The list of retracted and non-retracted studies can both be downloaded"),          p("PLEASE note, the tool draws on the RetractionWatch database of retracted studies. There may be delays in retracted studies being listed on the database. RetractionFindR can support the use of methods of identifying problematic studies by greatly speeding up the task of checking for retracted studies."),
          p("van Noorden R. More than 10,000 research papers were retracted in 2023—a new record. Nature. 2023 Dec;624(7992):479-81")
        ),
        tabPanel("Results",
                 h3("Results"),
                verbatimTextOutput("results"),  
                verbatimTextOutput("summary"),
                 tabsetPanel(
                   tabPanel("Retracted Articles", tableOutput("retracted_table")),
                   tabPanel("Non-Retracted Articles",tableOutput("nonretracted_table"))
                 )
        )
      )
    ))
)

server <- function(input, output, session) {
  results <- reactiveVal(NULL)
  separated <- reactiveVal(NULL)

  
  observeEvent(input$check_btn, {
    w <- Waiter$new(
      html = tags$div(style = "color:#00ffff;", spin_fading_circles()),
      color = "#00000080"  # semi-transparent dark overlay
    )
    w$show()
    on.exit(w$hide())
    
    tryCatch({
      req(input$ris_file)
      
      refs <- synthesisr::read_refs(input$ris_file$datapath)
      out <- check_retracted(refs)
      
      results(out)
      separated(separate_retracted(out$refs))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })

  output$summary <- renderPrint({
    req(results())
    cat("Total references:", results()$n_total, "\n")
    cat("Retracted references:", results()$n_retracted, "\n")
  })

  output$retracted_table <- renderTable({
    req(separated())
    separated()[[1]]
  })

  output$nonretracted_table <- renderTable({
    req(separated())
    separated()[[2]]
  })

  output$download_retracted <- downloadHandler(
    filename = function() {
      if (input$file_format == "ris") {
        "retracted_references.ris"
      } else {
        "retracted_references.csv"
      }
    },
    content = function(file) {
      data <- separated()[[1]]
      if (input$file_format == "ris") {
        synthesisr::write_refs(data, format = "ris", file = file)
      } else {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )


  output$download_nonretracted <- downloadHandler(
    filename = function() {
      if (input$file_format == "ris") {
        "non_retracted_references.ris"
      } else {
        "non_retracted_references.csv"
      }
    },
    content = function(file) {
      data <- separated()[[2]]
      if (input$file_format == "ris") {
        synthesisr::write_refs(data, format = "ris", file = file)
      } else {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
