## General Workflow ->
## Run as 'power_mode=TRUE' first to generate species errors that need to be
## fixed. File is generated in 'www' folder as...
##
##
##

## After all fixes have taken place -> Run in 'power_mode=FALSE' which has various
## QA/QC checks. Must run through all batch data at once with no errors to work.


## power mode will push past species insert error but reference them in output.log
#power_mode=TRUE
power_mode=FALSE

## test mode - TRUE to show DEV mode
#test_mode=TRUE
test_mode=FALSE


## set environment path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
app_path<<-getwd()

## global - libraries
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(DT)
library(openxlsx)

## read in xlsx file with USFS shapefile/naming info
pasture_info<<- openxlsx::read.xlsx("www/pasture_data.xlsx")

## -----------------------------------------------------------------------------
## Define UI 
ui <- fluidPage(
  
  ## general theme
  theme = shinytheme("sandstone"),

  titlePanel("Historical Data Importer"),
  
  sidebarLayout(
    sidebarPanel(
      
      ## inputs for side dashboard
      shiny::selectInput(inputId = "Protocol", label = "Select Protocol for Import",
                         choices = c(" "="NULL",
                                     #"USFS R6 Rogue River - Tally",
                                     "USFS R6 Rogue River Standard",
                                     "USFS R4 BTNF Range Monitoring"),
                         multiple = F, selected = F),
      ## pop up UI's here after Protocol entry - see server side
      shiny::actionButton(inputId = "create", label = "Batch Import Data", width = "100%"),
      
      ## if test mode
      if (test_mode == TRUE) {
        ## dev box
        box(width = '100%', solidHeader = F, collapsed = T,
            selectInput(inputId = "devMode",
                        label = "Dev Mode Options",
                        choices = c(FALSE,"allotment","pasture","protocol","eventG","events","protocol_2","base","data_page1","wildcard")
            ))
        ## end of dev box for test mode
      },
      
      ## if power mode
      if (power_mode == TRUE) {
        ## power mode will push through all species errors, will have to go back and
        ## check log for errors! VGS events will be corrupt if errors occur on insert
        box(width = '100%', solidHeader = F, collapsed = T,
            selectInput(inputId = "powMode",
                        label = "Power Mode?",
                        choices = TRUE
            ))
        ## end of power mode
      }
      
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
server <- function(input, output, session) {
  
  ## initial NULL Value - keeps spinner from showing at app load
  output$status <- renderTable(NULL)
  
  ## for UI drop downs ----
  observe({
    ## override test mode if selected
    test_mode<<- input$devMode
    
    ## if value is selected ->
    if (input$Protocol != "NULL") {

      ## changes input depending on Protocol selected
      if (input$Protocol == "USFS R6 Rogue River - Tally") {
        p2 <- c("USFS R6 Rogue River Standard", "No"="NULL")
        filter_keys <- c("Rogue River-Siskiyou National Forests"="USFS R6-RR")
      }
      if (input$Protocol == "USFS R6 Rogue River Standard") {
        #p2 <- c("USFS R6 Rogue River - Tally", "NA"="NULL")
        p2 <- c("No"="NULL")
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
    source(paste0(app_path,"/Functions/VGS_functions_R.R"), local = T)
    source(paste0(app_path,"/Functions/historical_data_importer.R"), local = T)
    read_import_data(Protocol = input$Protocol, ServerKey = input$ServerKey, Protocol_2 = input$Protocol_2)
    
    ## pop up for species errors in VGS
    source(paste0(app_path,"/Functions/species_qaqc_check.R"), local = T)
    
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
      
      ## if in power mode - different message
      if (power_mode == TRUE) {
        shinyalert("Finished (In Power Mode)", "Some data may be currupt, check log for errors", type = "warning")
        print("Finished (In Power Mode): Some data may be currupt, check log for errors")
      }
      
      ## only if not in power mode
      if (power_mode == FALSE) {
        ## first one is NULL so needs to check 2nd
        ## if reaches final output in function - sucessful = got past function checks
        if (status_check[2] == "**Batch Import Complete**") {
          shinyalert("Niceee!", "**Batch Import Complete**", type = "success", closeOnClickOutside = F)
        } else ## did not finish going through function for data import
          shinyalert("Oops!", "Something went wrong. Check log table or file", type = "error", closeOnClickOutside = F)
      }

      
      data_log_output
      
    }) ## end of render table
  }) ## end of observe event
  
  ## on session end -> make sure connections closed
  session$onSessionEnded(function() {
    suppressWarnings(DBI::dbDisconnect(mydb))
    suppressWarnings(closeAllConnections())
  })
} ## end of server
## -----------------------------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)
