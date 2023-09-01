## global - libraries
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyalert)
library(DT)

## testing variables to stop app
test_mode=FALSE
#test_mode="protocol"
#test_mode="eventG"
#test_mode="events"
#test_mode="protocol_2"
#test_mode="base" ## stops for site meta_data
#test_mode="data_page1" ## stops before data insert section
#test_mode="wildcard"
#closeAllConnections()
## -----------------------------------------------------------------------------
## Define UI 
ui <- fluidPage(
  
  ## general theme
  theme = shinytheme("flatly"),

  titlePanel("Historical Data Importer"),
  
  sidebarLayout(
    sidebarPanel(
      
      ## inputs for side dashboard
      shiny::selectInput(inputId = "Protocol", label = "Select Protocol for Import",
                         choices = c(" "="NULL",
                                     "USFS R6 Rogue River - Tally",
                                     "USFS R6 Rogue River Standard",
                                     "USFS R4 BTNF Range Monitoring"),
                         multiple = F, selected = F),
      ## pop up UI's here after Protocol entry - see server side
      shiny::actionButton(inputId = "create", label = "Batch Import Data", width = "100%")
    ), ## end of side bar panel
   
    ## output area of shiny app - box
    shinydashboard::box(solidHeader = T, width = 8,
      tabsetPanel(type = "tabs",
                  tabPanel(
                    "Status",
                    withSpinner(tableOutput("status"))
                  ) ## end of tab panal - status
      ) ## end of tab set panal(s)
    ) ## end of box display
  ) ## end of sidebar layout
) ## end of UI
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Define server logic
server <- function(input, output) {
  
  ## initial NULL Value - keeps spinner from showing at app load
  output$status <- renderTable(NULL)
  
  ## for UI drop downs ----
  observe({
    ## if value is selected ->
    if (input$Protocol != "NULL") {

      ## changes input depending on Protocol selected
      if (input$Protocol == "USFS R6 Rogue River - Tally") {
        p2 <- c("USFS R6 Rogue River Standard", "No"="NULL")
        filter_keys <- c("Rogue River-Siskiyou National Forests"="USFS R6-RR")
      }
      if (input$Protocol == "USFS R6 Rogue River Standard") {
        p2 <- c("USFS R6 Rogue River - Tally", "NA"="NULL")
        filter_keys <- c("Rogue River-Siskiyou National Forests"="USFS R6-RR")
      }
      if (input$Protocol == "USFS R4 BTNF Range Monitoring") {
        p2 <- c("No"="NULL")
        filter_keys <- c("Bridger-Teton National Forest"="USFS R4-BT")
      }
      
      ## insert selection for protocol #2
      insertUI(selector = "div:has(> #Protocol)",
               where = "afterEnd",
               ui = selectInput(inputId = "Protocol_2", label = "Select another protocol",
                                choices = p2,
                                multiple = F, selected = T))
      
      ## insert selection for server Key for parsing data
      insertUI(selector = "div:has(> #Protocol_2)",
               where = "afterEnd",
               ui = selectInput(inputId = "ServerKey", label = "Select Organization",
                                choices = filter_keys,
                                multiple = F, selected = T))
      
      updateSelectInput(inputId = "Protocol", label = paste0(input$Protocol, " selected!"))
      removeUI("div:has(> #Protocol)")
      
    }
  })
  ## end of UI drop downs ----

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
    }) ## end of reactive data log
    
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
      
    }) ## end of render table
  }) ## end of observe event
} ## end of server
## -----------------------------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)
