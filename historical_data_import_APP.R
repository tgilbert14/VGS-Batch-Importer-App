
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyalert)
library(DT)

# Define UI
ui <- fluidPage(
  
  ## general theme
  theme = shinytheme("flatly"),

  titlePanel("Historical Data Importer"),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      ## inputs for side dashboard
      shiny::selectInput(inputId = "Protocol", label = "Select Protocol for Import", choices = c(" "="NULL", "R6 Rogue River - Tally", "R6 Rogue River Nested Freq/GC/Line Intercept"), multiple = F, selected = F),
      ## pop up UI's here after Protocol entry - see server side
      shiny::actionButton(inputId = "create", label = "Batch Import Data", width = "100%")
    ),
    
   
    ## output area of shiny app - box
    shinydashboard::box(solidHeader = T, width = 8,
      tabsetPanel(type = "tabs",
        #header = "Select all relevant options - need server key",
        #fluidRow(
          tabPanel(
            "Status",
            withSpinner(tableOutput("status")
            )
          )#,
          # tabPanel(
          #   "Sites Created",
          #   withSpinner(tableOutput("sitesCreated")
          #   ),
          # )
        #)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## initial NULL Value - keeps spinner from showing at app load
  output$status <- renderTable(NULL)
  
  ## for UI drop downs ---------------------------------------------------------
  observe({
    ## if value is selected ->
    if (input$Protocol != "NULL") {

      ## changes input depending on Protocol selected
      if (input$Protocol == "R6 Rogue River - Tally") {
        p2 <- c("R6 Rogue River Nested Freq/GC/Line Intercept", "No"="NULL")
        filter_keys <- c("Rogue River-Siskiyou National Forests"="USFS R6-RR")
      }
      if (input$Protocol == "R6 Rogue River Nested Freq/GC/Line Intercept") {
        p2 <- c("R6 Rogue River - Tally", "No"="NULL")
        filter_keys <- c("Rogue River-Siskiyou National Forests"="USFS R6-RR")
      }
      
      ## insert selection for protocol #2
      insertUI(selector = "div:has(> #Protocol)",
               where = "afterEnd",
               ui = selectInput(inputId = "Protocol_2", label = "2nd Protocol?",
                                choices = p2,
                                multiple = F, selected = T))
      
      ## insert selection for server Key for parsing data
      insertUI(selector = "div:has(> #Protocol_2)",
               where = "afterEnd",
               ui = selectInput(inputId = "ServerKey", label = "Select Organization",
                                choices = filter_keys,
                                multiple = F, selected = T))
    }
  })
  ## end of UI drop downs ------------------------------------------------------


  observeEvent(input$create, {
    req(input$Protocol)

    ## creating log file to track progress
    sink("www/r_output.txt")
  
    ## pointing to VGS functions for batch data import
    source("Functions/VGS_functions_R.R", local = T)
    source("Functions/historical_data_importer.R", local = T)
    read_import_data(Protocol = input$Protocol, ServerKey = input$ServerKey, Protocol_2 = input$Protocol_2)
    
    ## close connections from local VGS db
    DBI::dbDisconnect(mydb)
    closeAllConnections()
    
    d_process<- reactive({
       data_log_output<- read.table("www/r_output.txt", header = T,
                                   fill = T, check.names = F, row.names = NULL,
                                   na.strings = T)
    })

    
    output$status <- renderTable({
      data_log_output<- d_process()
      req(data_log_output)
      
      status_check<- substring(data_log_output[nrow(data_log_output),], 
                first = nchar(data_log_output[nrow(data_log_output),])-24,
                last = nchar(data_log_output[nrow(data_log_output),]))
      
      ## first one is NULL so needs to check 2nd
      ## if reaches final output in function - sucessful = got past function checks
      if (status_check[2] == "**Batch Import Complete**") {
        shinyalert("Niceee!", "**Batch Import Complete**", type = "success")
      } else ## did not finish going through function for data import
        shinyalert("Oops!", "Something went wrong. Check log table or file", type = "error")
      
      data_log_output
      
    })
    
    # ## link to vgs / chaz voice ui pop ups
    # insertUI(selector = "#create",
    #          where = "afterEnd",
    #          ui = tagList(
    #            tags$a(class="vgsLink", href="https://portal.vgs.arizona.edu/", "VGS Data Portal Link"),
    #            tags$audio(src = "I love the VGS team.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;"),
    #            tags$p("See www/r_output.txt for data log file")
    #          ))
    
    })
}

# Run the application
shinyApp(ui = ui, server = server)
