library(shiny)
library(waiter)
library(DT)
library(retractionfindr)
library(synthesisr)

options(shiny.maxRequestSize = 30 * 1024^2)

# Load once at startup and share across all user sessions
retraction_db <- load_retraction_data()

cols_to_show <- c("title", "author", "year", "journal", "doi",
                  "is_retracted", "retraction_nature", "retraction_reason")

ui <- fluidPage(
  useWaiter(),
  titlePanel(
    tagList(icon("search", style = "color: steelblue;"), " RetractionFindR")
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("ris_file", "Upload .ris file", accept = ".ris"),
      actionButton("check_btn", "Check for Retractions", class = "btn-primary"),
      br(), br(),
      selectInput("file_format", "Download format:",
        choices = c("RIS" = "ris", "CSV" = "csv"),
        selected = "ris"
      ),
      br(),
      downloadButton("download_retracted", "Download Retracted"),
      br(), br(),
      downloadButton("download_nonretracted", "Download Non-Retracted")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction",
          h3("Welcome to RetractionFindR"),
          p("The number of retracted scientific publications has been increasing over the
            years. In 2023, more than 10,000 research papers were retracted, setting a new
            record (Van Noorden 2023). Some suggest this is only the 'tip of the iceberg'
            in terms of fraudulent research that is published each year. The escalation of
            papers that are bogus has escalated, in part fueled by the work of 'paper mills'
            (businesses selling authorship on fabricated papers)."),
          p("These present a particular threat to the reliability of systematic reviews,
            which aim to identify and synthesise all relevant evidence. Including fraudulent
            research will undermine the credibility of the review."),
          p("Identifying problematic studies is the goal of the INSPECT-SR tool, which aims
            to support reviewers in identifying studies that lack veracity. One requirement
            of the tool is that reviewers locate retracted studies."),
          p("The methods of handling retracted studies are variable across different
            databases. A retracted study may remain on a database, and while a note may be
            added, this might not be obvious to the reviewer."),
          p("This tool enables reviewers to readily identify those studies that have been
            retracted."),
          h4("How to use this tool"),
          tags$ol(
            tags$li("Export your search results as a .ris file from your reference manager
                    or database."),
            tags$li("Upload the .ris file using the panel on the left."),
            tags$li("Click 'Check for Retractions'."),
            tags$li("View the results in the Results tab."),
            tags$li("Download retracted and non-retracted articles in your preferred format.")
          ),
          p(em("Note: This tool draws on the RetractionWatch database. There may be delays
               in retracted studies being listed. RetractionFindR can greatly speed up the
               task of checking for retracted studies, but manual verification of flagged
               articles is recommended.")),
          p("van Noorden R. More than 10,000 research papers were retracted in 2023—a new
            record. ", em("Nature."), " 2023 Dec;624(7992):479-81.")
        ),
        tabPanel("Results",
          h3("Results"),
          uiOutput("summary_text"),
          br(),
          tabsetPanel(
            tabPanel("Retracted Articles",
              br(),
              DT::dataTableOutput("retracted_table")
            ),
            tabPanel("Non-Retracted Articles",
              br(),
              DT::dataTableOutput("nonretracted_table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  checked <- reactiveVal(NULL)
  sep     <- reactiveVal(NULL)

  observeEvent(input$check_btn, {
    w <- Waiter$new(
      html = tags$div(style = "color: #ffffff;", spin_fading_circles()),
      color = "#00000080"
    )
    w$show()
    on.exit(w$hide())

    tryCatch({
      req(input$ris_file)
      refs   <- synthesisr::read_refs(input$ris_file$datapath)
      result <- check_retracted(refs, retraction_data = retraction_db)
      checked(result)
      sep(separate_retracted(result, write_ris = FALSE))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })

  output$summary_text <- renderUI({
    req(checked())
    n <- count_retracted(checked())
    p(strong(paste0(
      n["n_retracted"], " retracted article",
      if (n["n_retracted"] != 1) "s" else "",
      " found out of ", n["n_total"], " total."
    )))
  })

  output$retracted_table <- DT::renderDataTable({
    req(sep())
    df <- sep()$retracted
    df[intersect(cols_to_show, names(df))]
  }, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)

  output$nonretracted_table <- DT::renderDataTable({
    req(sep())
    df <- sep()$non_retracted
    df[intersect(cols_to_show, names(df))]
  }, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)

  output$download_retracted <- downloadHandler(
    filename = function() {
      paste0("retracted_references", if (input$file_format == "ris") ".ris" else ".csv")
    },
    content = function(file) {
      req(sep())
      if (input$file_format == "ris") {
        writeLines(build_ris(as.data.frame(sep()$retracted)), con = file)
      } else {
        write.csv(sep()$retracted, file, row.names = FALSE)
      }
    }
  )

  output$download_nonretracted <- downloadHandler(
    filename = function() {
      paste0("non_retracted_references", if (input$file_format == "ris") ".ris" else ".csv")
    },
    content = function(file) {
      req(sep())
      if (input$file_format == "ris") {
        writeLines(build_ris(as.data.frame(sep()$non_retracted)), con = file)
      } else {
        write.csv(sep()$non_retracted, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
