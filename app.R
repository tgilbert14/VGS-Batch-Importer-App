## General Workflow ->
## Run as 'power_mode=TRUE' first to generate species errors that need to be
## fixed. File is generated in 'www' folder for QA/QC
## Fix errors and run the script again in a clean database in 'power_mode=FALSE'

## After all fixes have taken place -> Run in 'power_mode=FALSE' which has various
## QA/QC checks. Must run through all batch data at once with no errors to work.


## set environment path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
app_path <<- getwd()

## global - libraries
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyalert)
library(tidyverse)
library(shinyjs)
library(DT)
library(openxlsx)
library(uuid)
library(readxl)
library(DBI)
library(RSQLite)
library(stringr)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(sp)
library(rgdal)
# library(rChoiceDialogs)

## SQL local Connection info from R to local VGS5 (VGS50.db)
db_loc <<- "C:/ProgramData/VGSData/VGS50.db"
mydb <<- dbConnect(RSQLite::SQLite(), dbname = db_loc)

## data validation function
source(paste0(app_path, "/Functions/data_validation.R"), local = T)

## power mode will push past species insert errors but reference them in output.log
# power_mode=TRUE
power_mode <- FALSE
## test mode - TRUE to show DEV mode
# test_mode=TRUE
test_mode <- FALSE

## read in xlsx file with USFS shapefile/naming info
pasture_info <<- openxlsx::read.xlsx("www/pasture_data.xlsx")

## -----------------------------------------------------------------------------
## Define UI
ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/sweetalert2@11.10.0/dist/sweetalert2.all.min.js"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/sweetalert2@11.10.0/dist/sweetalert2.min.css"
    ),
    tags$script(src = "confirm.js")
  ),
  br(),
  
  ## general theme
  theme = shinytheme("slate"),
  titlePanel("Historical Data Importer"),
  br(),

  sidebarLayout(
    sidebarPanel(
      checkboxInput("mode", "Power Mode?", value = F),
      checkboxInput("qaqc", "Use Species Replace?", value = F),
      
      ## inputs for side dashboard
      shiny::selectInput(
        inputId = "Protocol",
        label = "Select Protocol for Import",
        choices = c(
          " " = "NULL",
          # "USFS R6 Rogue River - Tally",
          "USFS R6 Rogue River Standard",
          "USFS R4 BTNF Range Monitoring"
        ),
        multiple = F, selected = F
      ),
      ## pop up UI's here after Protocol entry - see server side
      shiny::actionButton(inputId = "create", label = "Batch Import Data", width = "100%"),
      br(),
      #checkboxInput("spec_mode", "Use Special Insert?", value = F),
      #br(),
 
      ## if test mode
      if (test_mode == TRUE) {
        ## dev box
        box(
          width = "100%", solidHeader = F, collapsed = T,
          selectInput(
            inputId = "devMode",
            label = "Dev Mode Options",
            choices = c(FALSE, "allotment", "pasture", "protocol", "eventG", "events", "protocol_2", "base", "data_page1", "wildcard")
          )
        )
        ## end of dev box for test mode
      },
      
      ## if power mode
      if (power_mode == TRUE) {
        ## power mode will push through all species errors, will have to go back and
        ## check log for errors! VGS events will be corrupt if errors occur on insert
        box(
          width = "100%", solidHeader = F, collapsed = T,
          selectInput(
            inputId = "powMode",
            label = "Power Mode?",
            choices = TRUE
          )
        )
        ## end of power mode
      }
    ), ## end of side bar panel
    

    ## output area of shiny app - box
    shinydashboard::box(
      solidHeader = T, width = 8,

      tabsetPanel(
        type = "tabs",
        tabPanel(
          div(style = "display: flex; justify-content: flex-start;",  # This will align the button to the right
              actionButton(inputId = "help", label = "", icon = icon("shrimp"), width = 40)
          ),
          #"Status...",
          withSpinner(tableOutput("status"))
        ) ## end of tab panal - status
      ) ## end of tab set panal(s)
    ) ## end of box display

  ), ## end of sidebar layout
  ## get species count by site after sites merged
  shiny::actionButton(icon = icon("pagelines"), inputId = "species_by_site",
                      label = "Count", width = 80),
  ## check species against USDA plant database for that state
  shiny::actionButton(icon = icon("wand-magic-sparkles"), inputId = "usda_check",
                      label = "Species Check", width = 140),
  ## update syncKey for loctor data after download syncable folders...
  shiny::actionButton(icon = icon("skull"), inputId = "synckey.update",
                      label = "Update Locator SyncKey", width = 200)
  
) ## end of UI
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Define server logic
server <- function(input, output, session) {

  ## help pop up
  observeEvent(input$help, {
    
    # Show a modal when the button is pressed
    shinyalert("App Workflow", "**Run app and select batch import excel files**
    >Use 'Power Mode' to check for errors<
    >Update SpeciesReplace.xlsx with corrected species codes<
    >Re-run app in normal mode with Species Replace on<
    >Clean up folder and merge repeated sites<
    >Download Folder Structure from server and place sites in correct folder<
    >Run 'vgs_name_update.R' script in the Functions folder to create correct names (syncable only)<
    >Sync Data as in pieces until all data is uploaded<",immediate = T,
               imageUrl = "images/anime.png", size = "m", imageWidth = 300, imageHeight = 200
    )
  })
  
  ## initial NULL Value - keeps spinner from showing at app load
  output$status <- renderTable(NULL)
  
  ## for UI drop downs ----
  observe({
    ## check power
    power_mode <<- input$mode
    ## override test mode if selected
    test_mode <<- input$devMode
    
    ## if value is selected ->
    if (input$Protocol != "NULL") {
      ## changes input depending on Protocol selected
      if (input$Protocol == "USFS R6 Rogue River - Tally") {
        p2 <- c("USFS R6 Rogue River Standard", "Not Allowed" = "NULL")
        filter_keys <- c("Rogue River-Siskiyou National Forests" = "USFS R6-RR")
      }
      if (input$Protocol == "USFS R6 Rogue River Standard") {
        # p2 <- c("USFS R6 Rogue River - Tally", "NA"="NULL")
        p2 <- c("Not Allowed" = "NULL")
        filter_keys <- c("Rogue River-Siskiyou National Forests" = "USFS R6-RR")
      }
      if (input$Protocol == "USFS R4 BTNF Range Monitoring") {
        p2 <- c("Not Allowed" = "NULL")
        filter_keys <- c("Bridger-Teton National Forest" = "USFS R4-BT")
      }
      
      ## insert selection for protocol #2
      insertUI(
        selector = "div:has(> #Protocol)",
        where = "afterEnd",
        ui = selectInput(
          inputId = "Protocol_2", label = "Select another protocol",
          choices = p2,
          multiple = F, selected = T
        )
      )
      
      ## insert selection for server Key for parsing data
      insertUI(
        selector = "div:has(> #Protocol_2)",
        where = "afterEnd",
        ui = selectInput(
          inputId = "ServerKey", label = "Select Organization",
          choices = filter_keys,
          multiple = F, selected = T
        )
      )
      
      updateSelectInput(inputId = "Protocol", label = paste0(input$Protocol, " Selected!"))
      removeUI("div:has(> #Protocol)")
    }
  })
  ## end of UI drop downs ----
  
  observeEvent(input$create, {
    req(input$Protocol)
    
    ## creating log file to track progress
    sink("www/r_output.txt")
    
    ## pointing to VGS functions for batch data import
    source(paste0(app_path, "/Functions/VGS_functions_R.R"), local = T)
    source(paste0(app_path, "/Functions/historical_data_importer.R"), local = T)
    read_import_data(Protocol = input$Protocol, ServerKey = input$ServerKey, Protocol_2 = input$Protocol_2)
    
    ## updating attribute table/form settings when needed
    shinyalert("Updating...", "attributes to unhide data", type = "info", immediate = T)
    source(paste0(app_path, "/Functions/attribute_update.R"), local = T)
    Sys.sleep(1)
    shinyalert("Done!", "Attributes Updated for Protocols", type = "success", immediate = T)
    
    ## pop up for species errors in VGS
    shinyalert("Creating...", "QAQC workbook for data checks", type = "info", immediate = T)
    source(paste0(app_path, "/Functions/species_qaqc_check.R"), local = T)
    Sys.sleep(1)
    shinyalert("Done!", "QAQC workbook created and opening...", type = "success", immediate = T)
    
    ## close connections from local VGS db
    DBI::dbDisconnect(mydb)
    closeAllConnections()
    
    d_process <- reactive({
      data_log_output <- read.table("www/r_output.txt",
                                    header = T,
                                    fill = T, check.names = F, row.names = NULL,
                                    na.strings = T
      )
    }) ## end of reactive data log
    
    output$status <- renderTable({
      data_log_output <- d_process()
      req(data_log_output)
      
      status_check <- substring(data_log_output[nrow(data_log_output), ],
                                first = nchar(data_log_output[nrow(data_log_output), ]) - 24,
                                last = nchar(data_log_output[nrow(data_log_output), ])
      )
      
      ## if in power mode - different message
      if (power_mode == TRUE) {
        ## message for output log, pop up species update file
        print("Use 'SpeciesReplace.xlsx' to update species that need to be corrected")
        file.show(paste0(app_path, "/www/SpeciesReplace.xlsx"))
        
        shinyalert("Finished (In Power Mode)", "No data inserted into events, check log & QA/QC reports for errors", type = "warning", immediate = T)
        print("Finished (In Power Mode): No data inserted into events, check log & QA/QC reports for errors")
      }
      
      ## only if not in power mode
      if (power_mode == FALSE) {
        shinyalert("Niceee!", "**Batch Import Complete**",
                   type = "success",
                   closeOnClickOutside = F,
                   immediate = T
        )
      }
      
      data_log_output
    }) ## end of render table
  }) ## end of observe event
  
  ## Count button click
  observeEvent(input$species_by_site, {
    shinyalert(title = "Sites should be merged before you do this!",
               text = "Opening Species Count File...",
               immediate = T, animation = "slide-from-buttom",
               confirmButtonText = "Thanks...",
               imageUrl = "images/plants.png", size = "m", imageWidth = 400, imageHeight = 200
    )
    
    Sys.sleep(1)
    ## Check all species added by Site
    sp_count_2 <- paste0(" SELECT DISTINCT SiteID, SpeciesName, SpeciesQualifier, PK_Species, CommonName, Count(PK_Species)  from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Site ON Site.PK_Site = Event.FK_Site
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'NRCS'
  group by SiteID, PK_Species, SpeciesName, CommonName, SpeciesQualifier
  order by SiteID, SpeciesName, SpeciesQualifier, PK_Species, CommonName")
    
    species_count_2 <- dbGetQuery(mydb, sp_count_2)
    
    write.xlsx(species_count_2, paste0(app_path, "/www/Conflicts/species_count_by_site.xlsx"))
    file.show(paste0(app_path, "/www/Conflicts/species_count_by_site.xlsx"))
  })
  
  ## Species Check click
  observeEvent(input$usda_check, {
    
    plant_files<- list.files("www/sp_lists_USDA/")
    state_names<- substr(plant_files, 0 , nchar(plant_files)-4)
    
    # shinyalert("Select a state",
    #            imageUrl = "images/cowboy2.png", size = "m", imageWidth = 500,
    #            imageHeight = 300, html = TRUE,
    #            text = tagList(selectInput(inputId = "st_pick",
    #                                       label = "This will compare species by state (USDA)",
    #                                       choices = c("",unlist(state_names)), selected = F, multiple = T)),
    #            confirmButtonText = "ok", confirmButtonCol = "#70FF19")
    
    # Show a modal when the button is clicked
    showModal(modalDialog(
      title = "Select a state",
      selectInput(inputId = "st_pick",
                  label = "This will compare species by state (USDA)",
                  choices = c("", state_names), 
                  selected = F, 
                  multiple = T),
      footer = tagList(
        modalButton("Close"),
        actionButton("button", "Generate Report")
      )
    ))
    
    observeEvent(input$button, {
      output$status <- renderText({
        req(nchar(input$st_pick)>0)
        
        ## get species list from database
        vgs_species_list_q <- paste0("SELECT PK_Species,SpeciesName, CommonName from Species where List = 'NRCS'")
        vgs_species_list <- dbGetQuery(mydb, vgs_species_list_q)
        
        # test
        #selected_state<- c("Idaho", "Nevada", "Colorado")
        selected_state<- input$st_pick
        
        ## state selection and file read - only 1 selection
        if (length(selected_state) == 1) {
          
          selected_state_file_path<- paste0(app_path,"/www/sp_lists_USDA/",selected_state,".csv")
          
          st_plant_data<- read_csv(selected_state_file_path)
          ## get species code name
          sp_codes_avaliable_by_state<- as.data.frame(unique(st_plant_data$symbol))
          names(sp_codes_avaliable_by_state) <- "code"
          ## get species code synomns to help catch old codes
          more_sp_codes_avaliable_by_state<- as.data.frame(unique(st_plant_data$synonym_symbol))
          names(more_sp_codes_avaliable_by_state) <- "code"
          
          final_sp_list <- unique(rbind(sp_codes_avaliable_by_state, more_sp_codes_avaliable_by_state)) %>%
            arrange(code)
          
          ## merge w/ vgs list ?

          
        } else { ## if more than 1 state selected
          state_num <- 1
          ## temp df to bind to later
          final_sp_list <- data.frame("code" = "")
          while (state_num < length(selected_state)+1) {
            ## goes through each state
            state<- selected_state[state_num]
            selected_state_file_path<- paste0(app_path,"/www/sp_lists_USDA/",state,".csv")
            
            st_plant_data<- read_csv(selected_state_file_path)
            ## get species code name
            sp_codes_avaliable_by_state<- as.data.frame(unique(st_plant_data$symbol))
            names(sp_codes_avaliable_by_state) <- "code"
            ## get species code synomns to help catch old codes
            more_sp_codes_avaliable_by_state<- as.data.frame(unique(st_plant_data$synonym_symbol))
            names(more_sp_codes_avaliable_by_state) <- "code"
            
            final_sp_list_temp <- unique(rbind(sp_codes_avaliable_by_state, more_sp_codes_avaliable_by_state)) %>%
              arrange(code)
            
            final_sp_list <- rbind(final_sp_list, final_sp_list_temp)
            
            state_num = state_num+1
          }
          
          final_sp_list <- unique(final_sp_list) %>% 
            arrange(code)
        }
        
        
        ## Check all species added
        sp_query <- paste0("SELECT DISTINCT PK_Species, SpeciesName from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'NRCS'")
        
        species_in_db <- dbGetQuery(mydb, sp_query)
        
        sp_in_data <- as.data.frame(unique(species_in_db$PK_Species))
        
        not_in_state <- setdiff(species_in_db$PK_Species,final_sp_list$code)
        
        ## get rid of allowed VGS 2 codes
        updated_not_in<- not_in_state[grep("^2", not_in_state, invert = T)]
        
        updated_not_in<- as.data.frame(updated_not_in)
        names(updated_not_in)[1]<- "PK_Species"
        
        updated_not_in_2<- left_join(updated_not_in, vgs_species_list)
        
        ## if species name is one name -> genus only (Aster)
        ## then check if there are species that start with it 'Aster sp...'
        
        # ## joining to VGS names for codes to see if species match (ASTER)
        # names(sp_codes_avaliable_by_state)[1]<- "PK_Species"
        # spe<- left_join(sp_codes_avaliable_by_state, vgs_species_list)
        # View(spe)
        # ## compare species names
        # setdiff(updated_not_in_2$SpeciesName, spe$SpeciesName)
        # spe$SpeciesName
        
        
        
        ## check genus in db vs if genus is in state
        
        ## ----
        
        
        names(updated_not_in_2)[1]<- paste0("Species not in USDA plant list for selected States")
        write.xlsx(updated_not_in_2, paste0(app_path, "/www/Conflicts/not_in_state.xlsx"))
        file.show(paste0(app_path, "/www/Conflicts/not_in_state.xlsx"))
        
        print(paste0(selected_state," Selected and checked"))
      })
      
    }) ## end of state select observable event

  }) ## end of click species check button
  
  ## Locator update click
  observeEvent(input$synckey.update, {
    ## update locator sync states so they show up - make 1 higher than site SyncState
    shinyalert("Updating...", "Locator SyncKey Updating to Site.Synckey+1", type = "info", immediate = T)
    source(paste0(app_path, "/Functions/locatorSyncState_update.R"), local = T)
    Sys.sleep(2)
    shinyalert("Done!", "Locator SyncKey Updating Complete", type = "success", immediate = T)
  })
  ## End of Locator update button click
  
  ## clear variables on stop
  onStop(function() {
    vars_to_remove <- c(ls()[ls()!=c("nest_freq_ready","power_mode","mydb")]) # all variables except
    rm(list=vars_to_remove, envir=.GlobalEnv)
  })

  ## on session end -> make sure connections closed
  session$onSessionEnded(function() {
    suppressWarnings(DBI::dbDisconnect(mydb))
    suppressWarnings(closeAllConnections())
  })
} ## end of server
## -----------------------------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)
