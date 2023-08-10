
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(DT)

## function to capture console updates -----------------------------------------
## -----------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  ## links css style sheet in www folder
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="styles.css")),
  ## general theme
  theme = shinytheme("paper"), collapsable = F,

  # Application title
  
  titlePanel("Historical Data Importer"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      ## inputs for side dashboard
      shiny::selectInput(inputId = "Protocol", label = "Select Protocol #1", choices = c("R6 Rogue River - Tally", "R6 Rogue River Nested Freq/GC/Line Intercept", "USFS R4..."), multiple = F, selected = F),
      shiny::selectInput(inputId = "Protocol_2", label = "Select Protocol #2 (if needed)", choices = c("Only 1 Protocol" = "NULL", "R6 Rogue River - Tally", "R6 Rogue River Nested Freq/GC/Line Intercept", "USFS R4..."), multiple = F, selected = F),
      shiny::selectInput(inputId = "ServerKey", label = "Select Relevant Server Key", choices = c("No ServerKey" = "NULL", "USFS R6-RR", "USFS R4-BT", "NRCS AZ"), multiple = F, selected = F),
      ## tag here for button
      shiny::actionButton(inputId = "create", label = "Batch Import Data")
    ),
    
   
    ## output area of shiny app - box
    shinydashboard::box(solidHeader = T,
      width = 8,
      #status = "primary",
      #title = "",
      
      tabsetPanel(type = "pills",
                  
        header = "Select all relevant options - need server key",
        #footer = 'See variable output_list to see what files were processed when complete...',
        
        fluidRow(
          

          tabPanel(
            "Status",
            withSpinner(textOutput("status")
                        # proxy.height = "150px",
                        # image.height = "150px" # ,
                        # image = "rat-72.gif"
            )
          ) # ,
          # tabPanel(
          #   "Import Summary",
          #   withSpinner(DT::dataTableOutput("status2"),
          #               proxy.height = "150px",
          #               image.height = "150px"#,
          #               #image = "rat-72.gif"
          #   )
          # )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## initial NULL Value - keeps spinner from showing at app load
  output$status <- renderText(NULL)
  
  observeEvent(input$create, {
    ## pointing to VGS functions for batch data import
    source("Functions/VGS_functions_R.R", local = T)
    source("Functions/historical_data_importer.R", local = T)
    read_import_data(Protocol = input$Protocol, ServerKey = input$ServerKey, Protocol_2 = input$Protocol_2)
    
    ## close connections from local VGS db
    DBI::dbDisconnect(mydb)
    closeAllConnections()
    
    ## link to vgs / chaz voice ui pop ups
    insertUI(selector = "#create",
             where = "afterEnd",
             ui = tagList(
               tags$a(class="vgsLink", href="https://portal.vgs.arizona.edu/containers/schemaroots/208c50da-de76-4f42-84b5-1b426aa0ed5b", "VGS Data Portal Link"),
               tags$audio(src = "I love the VGS team.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
             ))
    
    })
  
  
  output$status <- renderText({
    req(input$create)
    print(paste0("Data inserted into VGS50.db -> ", Sys.Date()))
    
  })
  
  # output$status2 <- DT::renderDataTable({
  #   output_list<- data()
  #   output_list
  # })
  
}

# Run the application
shinyApp(ui = ui, server = server)
