library(shiny)
library(retractionfindr)
library(tidyverse)
library(synthesisr)

options(shiny.maxRequestSize = 30 * 1024^2) # Set max file size to 30MB

ui <- fluidPage(
  titlePanel("RetractionFindR"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("ris_file", "Upload .ris file", accept = ".ris"),
      actionButton("check_btn", "Check for Retractions"),
      br(), br(),
      downloadButton("download_retracted", "Download Retracted"),
      downloadButton("download_nonretracted", "Download Non-Retracted")
    ),
    
    mainPanel(
      verbatimTextOutput("summary"),
      tabsetPanel(
        tabPanel("Retracted Articles", tableOutput("retracted_table")),
        tabPanel("Non-Retracted Articles", tableOutput("nonretracted_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  results <- reactiveVal(NULL)
  separated <- reactiveVal(NULL)
  
  observeEvent(input$check_btn, {
    req(input$ris_file)
    
    refs <- synthesisr::read_refs(input$ris_file$datapath)
    out <- check_retracted(refs)
    
    results(out)
    separated(separate_retracted(out$refs))
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
    filename = function() "retracted_references.csv",
    content = function(file) {
      write.csv(separated()[[1]], file, row.names = FALSE)
    }
  )
  
  output$download_nonretracted <- downloadHandler(
    filename = function() "nonretracted_references.csv",
    content = function(file) {
      write.csv(separated()[[2]], file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
