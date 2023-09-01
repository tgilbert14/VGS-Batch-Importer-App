## Historical Data importer
## set enviorment
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Read in libraries -----------------------------------------------------------
library(uuid)
library(tidyverse)
library(readxl)
library(DBI)
library(RSQLite)
library(stringr)


## [[ 1st function ]]
## READ IMPORT DATA FUNCTION ---------------------------------------------------
## >This function reads in all excel tabs and saves them, sends them to next
##  function(s) for import data process. Primarily gets raw data and passes it
##  on to functions over and over until all data selected is imported...
read_import_data <<- function(Protocol, ServerKey, Protocol_2 = "NULL") {
  db_loc <- "C:/ProgramData/VGSData/VGS50.db"
  mydb <- dbConnect(RSQLite::SQLite(), dbname = db_loc)
  ## Save to parent environment for later
  ServerKey <<- ServerKey
  Protocol <<- Protocol
  Protocol_2 <<- Protocol_2
  output_list<<- data.frame(FileNumber=numeric(),CompletedFileList=character())
  
  ## choosing file to import
  data_file <<- choose.files("Choose Historical Data to import (excel)")
  the_void <- character(0)
  
  if (identical(data_file, the_void)) {
    ## alert for data import start ->
    shinyalert("No Data Selected", "Refresh the page and choose a file!",
               imageUrl = "https://portal.vgs.arizona.edu/Content/Images/VGS_DarkGreen.png",
               imageWidth = 100, imageHeight = 100, type = "error", 
               timer = 2500)
    Sys.sleep(5)
    if (identical(data_file, the_void)) stop(paste0("No Data Selected - Choose a file!"))
  }
  
  ## alert for data import start ->
  shinyalert("...is crunching your data now", "YUM! Please wait for next update.",
             imageUrl = "https://portal.vgs.arizona.edu/Content/Images/VGS_DarkGreen.png",
             imageWidth = 100, imageHeight = 100, type = "success", 
             timer = 2500)
  
  ## reading in sheets to list
  historical_data <<- list()
  ## go through each batch file
  batch_file <<- 1
  while (batch_file < length(data_file) + 1) {
    print(" ")
    #cat("----->")
    
    print(paste0("Moving to File ", batch_file, " : ", data_file[batch_file]))
    
    shinyalert(paste0("Starting File #", batch_file), paste0(data_file[batch_file]),
               type = "success", timer = 2000, showConfirmButton = F)
    
    ## read specific batch file
    active_sheets <- excel_sheets(data_file[batch_file])
    ## don't care about VGS species list tab / ref sheets
    active_sheets <- active_sheets[active_sheets != "VGSDefaultSpeciesList"]
    active_sheets <- active_sheets[active_sheets != "Species Richness"]
    
    # Loop through each sheet and read the data into a data frame
    for (sheet_name in active_sheets) {
      suppressMessages(historical_data[[sheet_name]] <- read_excel(data_file[batch_file], sheet = sheet_name, col_names = F, trim_ws = T))
      print(paste0("Saving ", sheet_name))
    }
    active_sheets <<- active_sheets
    ## go through each sheet 'x'
    x <<- 1
    while (x < length(historical_data) + 1) {
      ## get all info from each excel sheet -> Reading one sheet at a time
      data_import <<- historical_data[[x]]
      # View(data_import)
      
      ## function for key info and data import
      batch_import(historical_raw_data = data_import)
      ## move site to correct folder or create parent folders
      # create_schema()
    }
    
    ## saving completed files for output
    output_list[batch_file,1]<<- batch_file
    output_list[batch_file,2]<<- data_file[[batch_file]]
    
    ## move to next batch file
    batch_file <<- batch_file + 1
    
    
    if (batch_file == length(data_file) + 1) {
      print("**Batch Import Complete**")
    }
  }
  DBI::dbDisconnect(mydb)
  closeAllConnections()
}
## end of read import data function --------------------------------------------


## [[ 2nd function ]]
## BATCH IMPORT DATA FUNCTION --------------------------------------------------
## >This function reads the saved excel sheets and parses data depending on tab
##  name or tab info. Parses data differently depending on ServerKey/organization.
##  Sends parsed data to create site function if relevant, otherwise sources to
##  other files (scrips folder) that parse data and sends to relevant insert 
##  data function based on ServerKey/organization.
batch_import <<- function(historical_raw_data) {
  ## saving globally
  historical_raw_data<<- historical_raw_data

  ## SET KEY DATA --------------------------------------------------------------
  ## Site MetaData by ROW
  
  ## separate key data out by chunks -> alter to find specific keys for batch import
  ## alter the grep "pattern" in quotes to help find correct rows
  
  ## only does this is there is site meta data
  ## if it is the first sheet/tab or it is a specific sheet tab name "Site Metadata",etc...
  if (x == 1 || active_sheets[batch_file] == "Site Metadata" || active_sheets[batch_file] == "SiteData") {
    print(paste0("Reading ", active_sheets[x]))
    print(paste0("Found SiteData info for file ", batch_file, " -> ", active_sheets[x]))
    print(paste0("Identifying keys for ", active_sheets[x]))
    
    if (test_mode == "base") stop("stop here to check out raw data form")
    #View(data_import)
    
    ## finding meta data ->
    if (ServerKey == "USFS R6-RR") {
      source("scripts/USFS R6/R6_RR_site_info.R")
    }
    if (ServerKey == "USFS R4-BT") {
      source("scripts/USFS R4/R4_BT_site_info.R")
    }
    if (ServerKey == "NRCS AZ") {
      source("scripts/NRCS AZ/NRCS_AZ_site_info.R")
    }
    
    ## variable manipulation ---->
    ## making all values NULL if NA
    if (is.na(aspect) || length(aspect) == 0) {
      aspect <- "NULL"
    }
    if (is.na(slope) || length(slope) == 0) {
      slope <- "NULL"
    }
    if (is.na(elevation) || length(elevation) == 0) {
      elevation <- "NULL"
    }
    ## if lat or long missing -> set to "NULL"
    if (is.na(lat) || is.na(long)) {
      lat <- "NULL"
      long <- "NULL"
    }
    ## End of variable manipulation
    
    print(paste0("Inserting ", active_sheets[x]))
    
    ## create site and data event
    create_site(
      ProtocolName = Protocol, ProtocolName_2 = Protocol_2, SiteID = site_name,
      Event_Date = event_date, Elevation = elevation, Slope = slope, Aspect = aspect,
      DDLat = lat, DDLong = long, EventNotes = EventNotes, Notes = site_notes
    )
    
    
    # ## placing site into correct folder schema ---------------------------------
    #
    # ## SITECLASS and SITECLASSLINK(S) -->
    # pasture_folder <- historical_raw_data[grep("pasture",
    #                                            historical_raw_data[[1]], ignore.case = TRUE),][[2]]
    # allotment_folder <- historical_raw_data[grep("allotment",
    #                                              historical_raw_data[[1]], ignore.case = TRUE),][[2]]
    #
    # ## query vgs 5 local db to see if folder schema already exists
    # ## Query to get Schema SiteClass(s)
    # SiteClass_query <- paste0("SELECT quote(PK_SiteClass), ClassName, quote(Ck_ParentClass)from SiteClass")
    # ## table from VGS local .db
    # vgs_siteClass <- dbGetQuery(mydb, SiteClass_query)
    #
    # ## looking for pasture folder
    # #grepl(pasture_folder, vgs_siteClass$ClassName)
    # vgs_siteClass[vgs_siteClass$ClassName == "New Folder",]
    # ## looking for allotment folder with same pasture folder
    #
    
    
    # SiteClassLinks_query <- paste0("SELECT quote(Ck_ParentClass), ClassName from SiteClassLink
    # inner join siteclass on siteclass.PK_SiteClass = siteClassLink.FK_SiteClass")
    # ## table from VGS local .db
    # vgs_siteClassLink <- dbGetQuery(mydb, SiteClassLinks_query)
    
    
    ## end of placing site into correct schema ---------------------------------
  }
  
  ## OTHER TABS ---- other than the first and not "site Meta Data" tabs
  ## if not site meta data of on site metadata page
  ## works for RR so far... ?
  if (x != 1) {
    
    print(paste0("Moving to ", active_sheets[x]))
    
    ## RR key - nested freq - check protocols as well
    if (ServerKey == "USFS R6-RR" && 
        (Protocol == "USFS R6 Rogue River Standard" || Protocol_2 == "USFS R6 Rogue River Standard")) {
      ## inserting freq data
      base::source("scripts/USFS R6/R6_RR_freq.R")
    }
    ## BTNF key
    if (ServerKey == "USFS R4-BT") {
      ## not done yet...
      base::source("scripts/USFS R4/R4_BT.R")
    }
    ## NRCS AZ
    if (ServerKey == "NRCS AZ") {
      ## not done yet...
      base::source("scripts/NRCS AZ/NRCS_AZ.R")
    }
    
  }
  ## move to next tab
  x <<- x + 1
}
## end of batch import data function -------------------------------------------


## [[ 3rd function ]]
## CREATE SITE FUNCTION --------------------------------------------------------
create_site <<- function(SiteID, Notes, ProtocolName, ProtocolName_2, Event_Date, EventNotes = "NULL", DDLat = "NULL", DDLong = "NULL", IsPrimary = 1, Slope = "NULL", Aspect = "NULL", Elevation = "NULL", DateEstablished = "NULL", SyncKey = 33, SyncState = 1, FK_Species_Site = "'SITE_KEY'", FK_Species_SiteStatus = "'SST_ACTIVE'", FK_Species_ElevUnits = "'UNIT_FEET'", FK_Species_Locator = "'LOC_MARKER'", LocatorID = "NULL", L_Description = "NULL", L_Date = "NULL", LocatorElevation = "NULL", L_SyncKey = 33, L_SyncState = 0, DateEnd = "NULL", Bailiwick = "NULL", FK_SiteClass = "NULL") {
  
  # SyncKey <<- 33
  # SyncState <<- 1
  
  # ## For testing ----
  # ProtocolName = Protocol
  # ProtocolName_2 = Protocol_2
  # SiteID = site_name
  # Event_Date = event_date
  # Elevation = elevation
  # Slope = slope
  # Aspect = aspect
  # DDLat = lat
  # DDLong = long
  # EventNotes=EventNotes
  # FK_Species_Site="'SITE_KEY'"
  # FK_Species_SiteStatus="'SST_ACTIVE'"
  # FK_Species_ElevUnits="'UNIT_FEET'"
  # FK_Species_Locator="'LOC_MARKER'"
  # DateEstablished='NULL'
  # SyncKey=33
  # SyncState=1
  # DateEnd="NULL"
  # Bailiwick="NULL"
  # L_Description="NULL"
  # L_Date="NULL"
  # LocatorElevation="NULL"
  # L_SyncKey=33
  # L_SyncState=0
  # FK_SiteClass="NULL"
  # LocatorID="NULL"
  # IsPrimary=1
  # ## End of testing ----
  # print("Starting site insert...")
  
  ## adding quotes if data present
  if (site_notes != "NULL" || is.na(site_notes) || length(site_notes) == 0) {
    site_notes <- paste0("'", site_notes, "'")
  }
  
  # if (DateEstablished != "NULL" || is.na(DateEstablished) || length(DateEstablished) == 0) {
  #   DateEstablished <- paste0("'", DateEstablished, "'")
  # }
  
  ## Guid for site pk
  PK_Site <- GUID()
  ## insert site sql statement
  insert_site <<- paste0("INSERT INTO Site
           (PK_Site
           ,FK_Species_Site
           ,FK_Species_SiteStatus
           ,FK_Species_ElevUnits
           ,SiteID
           ,Notes
           ,Slope
           ,Aspect
           ,Elevation
           ,DateEstablished
           ,SyncKey
           ,SyncState)
     VALUES
           (", PK_Site, ",
            ", FK_Species_Site, ",
            ", FK_Species_SiteStatus, ",
            ", FK_Species_ElevUnits, ",
            '", SiteID, "',
            ", site_notes, ",
            ", Slope, ",
            ", Aspect, ",
            ", Elevation, ",
            ", DateEstablished, ",
            ", SyncKey, ",
            ", SyncState, ")")
  

  ## insert site into site table - in unassigned bin
  dbExecute(mydb, insert_site)
  
  ## update messages for function
  print(paste0("Site created -> ", site_name))
  
  ## Adding locator is exists - both not NULL
  if (DDLat != "NULL" && DDLong != "NULL") {
    ## Guid for locator pk
    PK_Locator <- GUID()
    
    ## formatting ----
    if (L_Description != "NULL") {
      L_Description <- paste0("'", L_Description, "'")
    }
    if (LocatorID != "NULL") {
      LocatorID <- paste0("'", LocatorID, "'")
    }
    if (L_Date != "NULL") {
      L_Date <- paste0("'", L_Date, "'")
      ## format datetime
      L_Date <- as.character(L_Date)
    }
    ## end formatting ----
    
    insert_loc <- paste0("INSERT INTO Locator
  (PK_Locator
    ,FK_Species_Locator
    ,FK_Site
    ,LocatorID
    ,Description
    ,Date
    ,IsPrimary
    ,DDLat
    ,DDLong
    ,LocatorElevation
    ,SyncKey
    ,SyncState)
  VALUES (", PK_Locator, ",", FK_Species_Locator, ",", PK_Site, ",", LocatorID, ",", L_Description, ",", L_Date, ",", IsPrimary, ",", DDLat, ",", DDLong, ",", LocatorElevation, ",", L_SyncKey, ",", L_SyncState, ")")
    
    ## insert locator into locator table
    dbExecute(mydb, insert_loc)
    
    ## update messages for function
    print(paste0("Locator created -> ", site_name))
  }
  ## End of Locator ------------------------------------------------------------
  
  print("Finding Protocol in .db")
  ## Protocols Section
  ## inserting protocol 1----------------------------
  
  PK_Protocol <- GUID()
  print(paste0("Making ", ProtocolName, " for - ", PK_Protocol))
  
  ## query to get correct FK_Type_Protocol from typelist by looking for ProtocolName
  find_protocol <- paste0("Select quote(PK_Type), Attributes from typelist
                          where ListItem LIKE'%", ProtocolName, "%'")
  
  TypeList_info <- DBI::dbGetQuery(mydb, find_protocol)
  
  FK_Type_Protocol <- TypeList_info$`quote(PK_Type)`
  FK_Type_Protocol <- tolower(FK_Type_Protocol)
  
  if (length(FK_Type_Protocol) == 0) {
    shinyalert("Missing Protocol!", paste0("Import protocol ",ProtocolName," into VGS first and try again"), type = "error")
    Sys.sleep(5)
  }
  
  ## check if found protocol - boots out of VGS
  if (length(FK_Type_Protocol) == 0) stop(paste0(ProtocolName, " not in VGS -> import protocol first"))
  
  Attributes_raw <- TypeList_info$Attributes
  ## end inserting protocol 1----------------------------
  
  ## variable manipulation -----------------------------------------------------
  if (EventNotes != "NULL") {
    EventNotes <- paste0("'", EventNotes, "'")
  }
  if (DateEnd != "NULL") {
    DateEnd <- paste0("'", DateEnd, "'")
  }
  if (Bailiwick != "NULL") {
    Bailiwick <- paste0("'", Bailiwick, "'")
  }
  if (FK_SiteClass != "NULL") {
    FK_SiteClass <- paste0("'", FK_SiteClass, "'")
  }
  ## End of variable manipulation
  
  # print("")
  
  insert_protocol <- paste0("INSERT INTO Protocol
  (PK_Protocol
    ,FK_Type_Protocol
    ,Bailiwick
    ,ProtocolName
    ,Date
    ,DateEnd
    ,Notes
    ,SyncKey
    ,SyncState)
  VALUES
  (", PK_Protocol, ",", FK_Type_Protocol, ",", Bailiwick, ",'", ProtocolName, "','", Event_Date, "',", DateEnd, ",", EventNotes, ",", SyncKey, ",", SyncState, ")")
  
  ## log statment
  print(paste0("insert protocol"))
  
  if (test_mode == "protocol") {
    insert_protocol<<-insert_protocol
    sink()
    closeAllConnections()
    print(insert_protocol)
    stop("stop here to review")
  }
  
  ## insert protocol into protocol table
  dbExecute(mydb, insert_protocol)
  
  ## update messages for function
  print(paste0("Creating Protocol Event for ", Event_Date))
  
  ## then create EventGroups and Events-----------------------------------------
  ## need attributes from typelist -> query from protocol insert
  match_formIDs <- gregexpr("\\bFormID\\b", Attributes_raw) # \\b is word boundary
  match_groupNames <- gregexpr("\\bFormName\\b", Attributes_raw)
  match_attributes <- gregexpr("\\bFormAttributesXML\\b", Attributes_raw)
  match_FK_Type_EventGroups <- gregexpr("\\bFK_Type_EventGroup\\b", Attributes_raw)
  match_displayOrders <- gregexpr("\\bDisplayOrder\\b", Attributes_raw)
  # match_isActive<- gregexpr("\\bIsActive\\b", Attributes_raw)
  ## clearing all_forms variable if exists
  suppressWarnings(rm(all_formIDs))
  suppressWarnings(rm(all_groupNames))
  suppressWarnings(rm(all_attributes))
  suppressWarnings(rm(all_FK_Type_EventGroups))
  suppressWarnings(rm(all_displayOrders))
  suppressWarnings(rm(PK_EventGroup_GUIDS))
  ## lists to store variables of attributes
  all_formIDs <- list()
  all_groupNames <- list()
  all_attributes <- list()
  all_FK_Type_EventGroups <- list()
  all_displayOrders <- list()
  PK_EventGroup_GUIDS <- list()
  # all_isActive<- list()
  
  i <- 1
  while (i < length(match_formIDs[[1]]) + 1) {
    
    all_formIDs[i] <- substr(Attributes_raw, match_formIDs[[1]][i] + 7, match_formIDs[[1]][i + 1] - 3)
    all_groupNames[i] <- paste0("'", substr(Attributes_raw, match_groupNames[[1]][i] + 9, match_groupNames[[1]][i + 1] - 3), "'")
    all_attributes[i] <- paste0("'", substr(Attributes_raw, match_attributes[[1]][i] + 18, match_attributes[[1]][i + 1] - 3), "'")
    all_FK_Type_EventGroups[i] <- substr(Attributes_raw, match_FK_Type_EventGroups[[1]][i] + 19, match_FK_Type_EventGroups[[1]][i + 1] - 3)
    all_displayOrders[i] <- as.numeric(substr(Attributes_raw, match_displayOrders[[1]][i] + 13, match_displayOrders[[1]][i + 1] - 3))
    # all_isActive[i]<- substr(Attributes_raw, match_isActive[[1]][i]+9, match_isActive[[1]][i+1]-3)
    ## formatting xlm -->
    all_attributes[i] <- gsub("&lt;", "<", all_attributes[i], fixed = T)
    all_attributes[i] <- gsub("&gt;", ">", all_attributes[i], fixed = T)
    all_attributes[i] <- gsub("[\r\n]", "", all_attributes[i], fixed = T)
    all_attributes[i] <- gsub("amp;", "", all_attributes[i], fixed = T)
    all_attributes[i] <- gsub("'", "", all_attributes[i], fixed = T)

    # all_groupNames[i] <- gsub("&lt;", "<", all_groupNames[i], fixed = T)
    # all_groupNames[i] <- gsub("&gt;", ">", all_groupNames[i], fixed = T)
    # all_groupNames[i] <- gsub("[\r\n]", "", all_groupNames[i], fixed = T)
    # all_groupNames[i] <- gsub("amp;", "", all_groupNames[i], fixed = T)
    # all_groupNames[i] <- gsub("'", "", all_groupNames[i], fixed = T)
    
    
    ## query for protocol correct guid for PK_Protocol
    protocol_check <- paste0("Select quote(PK_Protocol) from Protocol
           where PK_Protocol = ", PK_Protocol)
    
    protocol_pk_check <- dbGetQuery(mydb, protocol_check)
    protocol_pk_check <- tolower(protocol_pk_check)
    substr(protocol_pk_check, 2, 2) <- toupper(substr(protocol_pk_check, 2, 2))
    
    ## GUIDS in attributes are the wrong format ->
    # PK_Protocol=paste0("X'1c988e6a3dea4741b8065235a92b00b0'")
    if (i %in% c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)) { ## if i is odd inset data
      
      PK_EventGroup_GUIDS[i] <- GUID()
      
      insert_eventGroup <- paste0("INSERT INTO EventGroup
           (PK_EventGroup
           ,FK_Type_EventGroup
           ,FK_Protocol
           ,Attributes
           ,GroupName
           ,DisplayOrder
           ,DefaultFormID
           ,SyncKey
           ,SyncState)
     VALUES
           (", PK_EventGroup_GUIDS[[i]], ",", Hex(all_FK_Type_EventGroups[[i]]), ",", protocol_pk_check[[1]], ",'", all_attributes[[i]], "',", all_groupNames[[i]], ",", all_displayOrders[[i]], ",", Hex(all_formIDs[[i]]), ",", SyncKey, ",", SyncState, ")")
      
      ## log statment
      print(paste0("insert event group to protocol"))
      
      if (test_mode == "eventG") {
        insert_eventGroup<<-insert_eventGroup
        sink()
        closeAllConnections()
        print(insert_eventGroup)
        stop("stop here to review")
      }
      
      ## insert eventGroups into eventGroup table
      dbExecute(mydb, insert_eventGroup)
    }
    i <- i + 2
  }
  ## End of EventGroups
  
  ## Insert Events in each EventGroup/Events -----------------------------------
  ## need attributes from typelist -> query from protocol insert
  match_ParentFormIDs <- gregexpr("\\bParentFormID\\b", Attributes_raw)
  match_EventAttributes <- gregexpr("\\bEventAttributesXML\\b", Attributes_raw)
  match_FK_Type_Events <- gregexpr("\\FK_Type_Event\\b", Attributes_raw)
  match_PageNumbers <- gregexpr("\\PageNumber\\b", Attributes_raw)
  match_EntryOrders <- gregexpr("\\EntryOrder\\b", Attributes_raw)
  match_EventNames <- gregexpr("\\EventName\\b", Attributes_raw)
  match_DefaultEventIDs <- gregexpr("\\DefaultEventID\\b", Attributes_raw)
  ## prepping list - removing if exists
  suppressWarnings(rm(all_ParentformIDs))
  suppressWarnings(rm(all_EventAttributes))
  suppressWarnings(rm(all_FK_Type_Events))
  suppressWarnings(rm(all_PageNumbers))
  suppressWarnings(rm(all_EntryOrders))
  suppressWarnings(rm(all_EventNames))
  suppressWarnings(rm(all_DefaultEventIDs))
  ## lists to store variables of attributes
  all_ParentformIDs <- list()
  all_EventAttributes <- list()
  all_FK_Type_Events <- list()
  all_PageNumbers <- list()
  all_EntryOrders <- list()
  all_EventNames <- list()
  all_DefaultEventIDs <- list()
  
  i <- 1
  while (i < length(match_ParentFormIDs[[1]]) + 1) {

    all_EventAttributes[i] <- paste0("'", substr(Attributes_raw, match_EventAttributes[[1]][i] + 19, match_EventAttributes[[1]][i + 1] - 3), "'")
    all_FK_Type_Events[i] <- substr(Attributes_raw, match_FK_Type_Events[[1]][i] + 14, match_FK_Type_Events[[1]][i + 1] - 3)
    all_ParentformIDs[i] <- substr(Attributes_raw, match_ParentFormIDs[[1]][i] + 13, match_ParentFormIDs[[1]][i + 1] - 3)
    all_PageNumbers[i] <- substr(Attributes_raw, match_PageNumbers[[1]][i] + 11, match_PageNumbers[[1]][i + 1] - 3)
    all_EntryOrders[i] <- substr(Attributes_raw, match_EntryOrders[[1]][i] + 11, match_EntryOrders[[1]][i + 1] - 3)
    all_EventNames[i] <- paste0("'", substr(Attributes_raw, match_EventNames[[1]][i] + 10, match_EventNames[[1]][i + 1] - 3), "'")
    all_DefaultEventIDs[i] <- substr(Attributes_raw, match_DefaultEventIDs[[1]][i] + 15, match_DefaultEventIDs[[1]][i + 1] - 3)
    ## formatting xlm
    all_EventAttributes[i] <- gsub("&lt;", "<", all_EventAttributes[i], fixed = T)
    all_EventAttributes[i] <- gsub("&gt;", ">", all_EventAttributes[i], fixed = T)
    all_EventAttributes[i] <- gsub("[\r\n]", "", all_EventAttributes[i], fixed = T)
    all_EventAttributes[i] <- gsub("amp;", "", all_EventAttributes[i], fixed = T)
    all_EventAttributes[i] <- gsub("'", "", all_EventAttributes[i], fixed = T)
    
    # all_EventNames[i] <- gsub("&lt;", "<", all_EventNames[i], fixed = T)
    # all_EventNames[i] <- gsub("&gt;", ">", all_EventNames[i], fixed = T)
    # all_EventNames[i] <- gsub("[\r\n]", "", all_EventNames[i], fixed = T)
    # all_EventNames[i] <- gsub("amp;", "", all_EventNames[i], fixed = T)
    # all_EventNames[i] <- gsub("'", "", all_EventNames[i], fixed = T)
    
    i <- i + 2
  }
  
  ## matching unique PK_Groups with ParentFormID in attributes to connect correctly
  PK_EG <- unique(unlist(PK_EventGroup_GUIDS))
  Parent_Guid <- unique(unlist(all_ParentformIDs))
  ## saving as data frame
  eventGroup_match <- bind_cols(as.data.frame(PK_EG), as.data.frame(Parent_Guid))
  
  ## after inserting site -> query .db
  ## retrieve PK value ---
  site_check <- paste0("Select quote(PK_site) from site
                        where PK_Site = ", PK_Site)
  site_pk_check <- dbGetQuery(mydb, site_check)
  site_pk_check <- tolower(site_pk_check)
  substr(site_pk_check, 2, 2) <- toupper(substr(site_pk_check, 2, 2))

  
  i <- 1
  while (i < length(all_ParentformIDs) + 1) {
    if (i %in% c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)) { ## if i is odd inset data
      
      PK_Event <- GUID()
      
      ## Look in attributes to see what Form is above the event to get correct PK_EventGroup GUID
      ## ParentFormID tells which EventGroup they belong in
      find_position <- grep(all_ParentformIDs[i], eventGroup_match$Parent_Guid)
      FK_EventGroup <- eventGroup_match$PK_EG[find_position]
      
      ## query for protocol correct guid - PK_EventGroup
      eventG_check <- paste0("Select quote(PK_EventGroup) from EventGroup
                        where PK_EventGroup = ", FK_EventGroup)
      eventG_pk_check <- dbGetQuery(mydb, eventG_check)
      eventG_pk_check <- tolower(eventG_pk_check)
      substr(eventG_pk_check, 2, 2) <- toupper(substr(eventG_pk_check, 2, 2))
      
      insert_events <- paste0("INSERT INTO Event
         (PK_Event
         ,FK_Type_Event
         ,FK_Site
         ,FK_SiteClass
         ,FK_EventGroup
         ,EventName
         ,Attributes
         ,PageNumber
         ,EntryOrder
         ,DefaultEventID
         ,SyncKey
         ,SyncState)
   VALUES
         (", PK_Event, ",", Hex(all_FK_Type_Events[i]), ",", site_pk_check[[1]], ",", FK_SiteClass, ",", eventG_pk_check[[1]], ",", all_EventNames[i], ",'", all_EventAttributes[i], "',", all_PageNumbers[i], ",", all_EntryOrders[i], ",", Hex(all_DefaultEventIDs[i]), ",", SyncKey, ",", SyncState, ")")
      
      ## log statment
      print(paste0("insert event to event group"))

      if (test_mode == "events") {
        insert_events<<-insert_events
        sink()
        closeAllConnections()
        print(insert_eventGroup)
        stop("stop here to review")
      }
      
      ## insert eventGroups into eventGroup table
      dbExecute(mydb, insert_events)
    }
    i <- i + 2
  }
  ## End of Events -------------------------------------------------------------
  
  
  ## update messages for function
  print(paste0("EventGroups and Events attached to ", ProtocolName))
  
  ## inserting protocol, eventgroup, and events - 2 ----------------------------
  ## insert another protocol if needed (Example, RR needs Tally & Nested Freq)
  if (Protocol_2 != "NULL") {
    ## repeat as above with new protocol
    PK_Protocol_2 <- GUID()
    print(paste0("Making ", ProtocolName_2, " for - ", PK_Protocol_2))
    ## query to get correct FK_Type_Protocol from typelist by looking for ProtocolName
    find_protocol_2 <- paste0("Select quote(PK_Type), Attributes from typelist
                          where ListItem LIKE'%", ProtocolName_2, "%'")
    
    TypeList_info <- DBI::dbGetQuery(mydb, find_protocol_2)
    
    FK_Type_Protocol <- TypeList_info$`quote(PK_Type)`
    
    if (length(FK_Type_Protocol) == 0) {
      shinyalert("Missing Protocol!", paste0("Import protocol ",ProtocolName_2," into VGS first and try again"), type = "error")
      Sys.sleep(5)
    }
    
    ## check if found protocol
    if (length(FK_Type_Protocol) == 0) stop(paste0(ProtocolName_2, " not in VGS -> import first"))
    
    Attributes_raw <- TypeList_info$Attributes
    
    ## 2nd protocol set same values as 1st
    ## variable manipulation -----------------------------------------------------
    # if (EventNotes !="NULL") {
    #   EventNotes <- paste0("'",EventNotes,"'")
    # }
    # if (DateEnd !="NULL") {
    #   DateEnd <- paste0("'",DateEnd,"'")
    # }
    # if (Bailiwick !="NULL") {
    #   Bailiwick <- paste0("'",Bailiwick,"'")
    # }
    # if (FK_SiteClass !="NULL") {
    #   FK_SiteClass <- paste0("'",FK_SiteClass,"'")
    # }
    ## End of variable manipulation ----
    
    insert_protocol_2 <- paste0("INSERT INTO Protocol
  (PK_Protocol
    ,FK_Type_Protocol
    ,Bailiwick
    ,ProtocolName
    ,Date
    ,DateEnd
    ,Notes
    ,SyncKey
    ,SyncState)
  VALUES
  (", PK_Protocol_2, ",", FK_Type_Protocol, ",", Bailiwick, ",'", ProtocolName_2, "','", Event_Date, "',", DateEnd, ",", EventNotes, ",", SyncKey, ",", SyncState, ")")
    
    ## log statment
    print(paste0("insert protocol"))
    
    if (test_mode == "protocol_2") {
      insert_protocol_2<<-insert_protocol_2
      sink()
      closeAllConnections()
      print(insert_protocol_2)
      stop("stop here to review")
    }
    
    ## insert protocol into protocol table
    dbExecute(mydb, insert_protocol_2)
    
    ## update messages for function
    print(paste0("Creating Protocol Event for ", Event_Date))
    
    ## then create EventGroups ---------------------------------------------------
    ## need attributes from typelist -> query from protocol insert
    match_formIDs <- gregexpr("\\bFormID\\b", Attributes_raw) # \\b is word boundary
    match_groupNames <- gregexpr("\\bFormName\\b", Attributes_raw)
    match_attributes <- gregexpr("\\bFormAttributesXML\\b", Attributes_raw)
    match_FK_Type_EventGroups <- gregexpr("\\bFK_Type_EventGroup\\b", Attributes_raw)
    match_displayOrders <- gregexpr("\\bDisplayOrder\\b", Attributes_raw)
    # match_isActive<- gregexpr("\\bIsActive\\b", Attributes_raw)
    ## clearing all_forms variable if exists
    suppressWarnings(rm(all_formIDs))
    suppressWarnings(rm(all_groupNames))
    suppressWarnings(rm(all_attributes))
    suppressWarnings(rm(all_FK_Type_EventGroups))
    suppressWarnings(rm(all_displayOrders))
    suppressWarnings(rm(PK_EventGroup_GUIDS))
    ## lists to store variables of attributes
    all_formIDs <- list()
    all_groupNames <- list()
    all_attributes <- list()
    all_FK_Type_EventGroups <- list()
    all_displayOrders <- list()
    PK_EventGroup_GUIDS <- list()
    # all_isActive<- list()
    
    i <- 1
    while (i < length(match_formIDs[[1]]) + 1) {
      all_formIDs[i] <- substr(Attributes_raw, match_formIDs[[1]][i] + 7, match_formIDs[[1]][i + 1] - 3)
      all_groupNames[i] <- paste0("'", substr(Attributes_raw, match_groupNames[[1]][i] + 9, match_groupNames[[1]][i + 1] - 3), "'")
      all_attributes[i] <- paste0("'", substr(Attributes_raw, match_attributes[[1]][i] + 18, match_attributes[[1]][i + 1] - 3), "'")
      all_FK_Type_EventGroups[i] <- substr(Attributes_raw, match_FK_Type_EventGroups[[1]][i] + 19, match_FK_Type_EventGroups[[1]][i + 1] - 3)
      all_displayOrders[i] <- as.numeric(substr(Attributes_raw, match_displayOrders[[1]][i] + 13, match_displayOrders[[1]][i + 1] - 3))
      # all_isActive[i]<- substr(Attributes_raw, match_isActive[[1]][i]+9, match_isActive[[1]][i+1]-3)
      ## formatting xlm -->
      all_attributes[i] <- gsub("&lt;", "<", all_attributes[i], fixed = T)
      all_attributes[i] <- gsub("&gt;", ">", all_attributes[i], fixed = T)
      all_attributes[i] <- gsub("[\r\n]", "", all_attributes[i], fixed = T)
      all_attributes[i] <- gsub("amp;", "", all_attributes[i], fixed = T)
      all_attributes[i] <- gsub("'", "", all_attributes[i], fixed = T)
      
      # all_groupNames[i] <- gsub("&lt;", "<", all_groupNames[i], fixed = T)
      # all_groupNames[i] <- gsub("&gt;", ">", all_groupNames[i], fixed = T)
      # all_groupNames[i] <- gsub("[\r\n]", "", all_groupNames[i], fixed = T)
      # all_groupNames[i] <- gsub("amp;", "", all_groupNames[i], fixed = T)
      # all_groupNames[i] <- gsub("'", "", all_groupNames[i], fixed = T)
      
      ## query for protocol correct guid for PK_Protocol
      protocol_check_2 <- paste0("Select quote(PK_Protocol) from Protocol
           where PK_Protocol = ", PK_Protocol_2)
      
      protocol_pk_check_2 <- dbGetQuery(mydb, protocol_check_2)
      protocol_pk_check_2 <- tolower(protocol_pk_check_2)
      substr(protocol_pk_check_2, 2, 2) <- toupper(substr(protocol_pk_check_2, 2, 2))
      
      ## GUIDS in attributes are the wrong format ->
      # PK_Protocol=paste0("X'1c988e6a3dea4741b8065235a92b00b0'")
      if (i %in% c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)) { ## if i is odd inset data
        
        PK_EventGroup_GUIDS[i] <- GUID()
        
        insert_eventGroup <- paste0("INSERT INTO EventGroup
           (PK_EventGroup
           ,FK_Type_EventGroup
           ,FK_Protocol
           ,Attributes
           ,GroupName
           ,DisplayOrder
           ,DefaultFormID
           ,SyncKey
           ,SyncState)
     VALUES
           (", PK_EventGroup_GUIDS[[i]], ",", Hex(all_FK_Type_EventGroups[[i]]), ",", protocol_pk_check_2[[1]], ",'", all_attributes[[i]], "',", all_groupNames[[i]], ",", all_displayOrders[[i]], ",", Hex(all_formIDs[[i]]), ",", SyncKey, ",", SyncState, ")")
        
        ## log statment
        print(paste0("insert event group to protocol"))
        
        ## insert eventGroups into eventGroup table
        dbExecute(mydb, insert_eventGroup)
      }
      i <- i + 2
    }
    ## End of EventGroups --------------------------------------------------------
    
    ## Insert Events in each EventGroup ------------------------------------------
    ## need attributes from typelist -> query from protocol insert
    match_ParentFormIDs <- gregexpr("\\bParentFormID\\b", Attributes_raw)
    match_EventAttributes <- gregexpr("\\bEventAttributesXML\\b", Attributes_raw)
    match_FK_Type_Events <- gregexpr("\\FK_Type_Event\\b", Attributes_raw)
    match_PageNumbers <- gregexpr("\\PageNumber\\b", Attributes_raw)
    match_EntryOrders <- gregexpr("\\EntryOrder\\b", Attributes_raw)
    match_EventNames <- gregexpr("\\EventName\\b", Attributes_raw)
    match_DefaultEventIDs <- gregexpr("\\DefaultEventID\\b", Attributes_raw)
    ## prepping list - removing if exists
    suppressWarnings(rm(all_ParentformIDs))
    suppressWarnings(rm(all_EventAttributes))
    suppressWarnings(rm(all_FK_Type_Events))
    suppressWarnings(rm(all_PageNumbers))
    suppressWarnings(rm(all_EntryOrders))
    suppressWarnings(rm(all_EventNames))
    suppressWarnings(rm(all_DefaultEventIDs))
    ## lists to store variables of attributes
    all_ParentformIDs <- list()
    all_EventAttributes <- list()
    all_FK_Type_Events <- list()
    all_PageNumbers <- list()
    all_EntryOrders <- list()
    all_EventNames <- list()
    all_DefaultEventIDs <- list()
    
    i <- 1
    while (i < length(match_ParentFormIDs[[1]]) + 1) {
      all_EventAttributes[i] <- paste0("'", substr(Attributes_raw, match_EventAttributes[[1]][i] + 19, match_EventAttributes[[1]][i + 1] - 3), "'")
      all_FK_Type_Events[i] <- substr(Attributes_raw, match_FK_Type_Events[[1]][i] + 14, match_FK_Type_Events[[1]][i + 1] - 3)
      all_ParentformIDs[i] <- substr(Attributes_raw, match_ParentFormIDs[[1]][i] + 13, match_ParentFormIDs[[1]][i + 1] - 3)
      all_PageNumbers[i] <- substr(Attributes_raw, match_PageNumbers[[1]][i] + 11, match_PageNumbers[[1]][i + 1] - 3)
      all_EntryOrders[i] <- substr(Attributes_raw, match_EntryOrders[[1]][i] + 11, match_EntryOrders[[1]][i + 1] - 3)
      all_EventNames[i] <- paste0("'", substr(Attributes_raw, match_EventNames[[1]][i] + 10, match_EventNames[[1]][i + 1] - 3), "'")
      all_DefaultEventIDs[i] <- substr(Attributes_raw, match_DefaultEventIDs[[1]][i] + 15, match_DefaultEventIDs[[1]][i + 1] - 3)
      ## formatting xlm
      all_EventAttributes[i] <- gsub("&lt;", "<", all_EventAttributes[i], fixed = T)
      all_EventAttributes[i] <- gsub("&gt;", ">", all_EventAttributes[i], fixed = T)
      all_EventAttributes[i] <- gsub("[\r\n]", "", all_EventAttributes[i], fixed = T)
      all_EventAttributes[i] <- gsub("amp;", "", all_EventAttributes[i], fixed = T)
      all_EventAttributes[i] <- gsub("'", "", all_EventAttributes[i], fixed = T)
      
      # all_EventNames[i] <- gsub("&lt;", "<", all_EventNames[i], fixed = T)
      # all_EventNames[i] <- gsub("&gt;", ">", all_EventNames[i], fixed = T)
      # all_EventNames[i] <- gsub("[\r\n]", "", all_EventNames[i], fixed = T)
      # all_EventNames[i] <- gsub("amp;", "", all_EventNames[i], fixed = T)
      # all_EventNames[i] <- gsub("'", "", all_EventNames[i], fixed = T)

      i <- i + 2
    }
    
    ## matching unique PK_Groups with ParentFormID in attributes to connect correctly
    PK_EG <- unique(unlist(PK_EventGroup_GUIDS))
    Parent_Guid <- unique(unlist(all_ParentformIDs))
    ## saving as data frame
    eventGroup_match <- bind_cols(as.data.frame(PK_EG), as.data.frame(Parent_Guid))
    
    ## after inserting site -> query .db
    ## retrieve PK value ---
    site_check <- paste0("Select quote(PK_site) from site
                        where PK_Site = ", PK_Site)
    site_pk_check <- dbGetQuery(mydb, site_check)
    site_pk_check <- tolower(site_pk_check)
    substr(site_pk_check, 2, 2) <- toupper(substr(site_pk_check, 2, 2))
    
    i <- 1
    while (i < length(all_ParentformIDs) + 1) {
      if (i %in% c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)) { ## if i is odd inset data
        
        PK_Event <- GUID()
        
        ## Look in attributes to see what Form is above the event to get correct PK_EventGroup GUID
        ## ParentFormID tells which EventGroup they belong in
        find_position <- grep(all_ParentformIDs[i], eventGroup_match$Parent_Guid)
        FK_EventGroup <- eventGroup_match$PK_EG[find_position]
        
        ## query for protocol correct guid - PK_EventGroup
        eventG_check <- paste0("Select quote(PK_EventGroup) from EventGroup
                        where PK_EventGroup = ", FK_EventGroup)
        eventG_pk_check <- dbGetQuery(mydb, eventG_check)
        
        eventG_pk_check <- tolower(eventG_pk_check)
        substr(eventG_pk_check, 2, 2) <- toupper(substr(eventG_pk_check, 2, 2))
        
        insert_events <- paste0("INSERT INTO Event
         (PK_Event
         ,FK_Type_Event
         ,FK_Site
         ,FK_SiteClass
         ,FK_EventGroup
         ,EventName
         ,Attributes
         ,PageNumber
         ,EntryOrder
         ,DefaultEventID
         ,SyncKey
         ,SyncState)
   VALUES
         (", PK_Event, ",", Hex(all_FK_Type_Events[i]), ",", site_pk_check[[1]], ",", FK_SiteClass, ",", eventG_pk_check[[1]], ",", all_EventNames[i], ",'", all_EventAttributes[i], "',", all_PageNumbers[i], ",", all_EntryOrders[i], ",", Hex(all_DefaultEventIDs[i]), ",", SyncKey, ",", SyncState, ")")
        
        ## log statment
        print(paste0("insert event to event group"))
        
        ## insert eventGroups into eventGroup table
        dbExecute(mydb, insert_events)
      }
      i <- i + 2
    }
    ## End of Events -----------------------------------------------------------
    ## update messages for function
    print(paste0("EventGroups and Events attached to ", ProtocolName_2))
  }
  ## End of Protocol 2 -----
  
  ## this section takes place only if 1st tab/page of xlsx or labeled as
  ## site meta data
  
  if (test_mode == "data_page1") stop("stop here to check out raw data form")
  
  ## Start of data inserting after protocols created -> gc data on site meta
  ## data page
  ## specific to R6 RR on first page ->
  if (ServerKey == "USFS R6-RR") {
    ## GC insert for RR
    source("scripts/USFS R6/R6_RR_gc.R")
  }
  
}
## end of create site function -------------------------------------------------


## [[ 4th function ]]
## INSERT DATA FUNCTION --------------------------------------------------------
## insert statement to add data - nested frequency
insert_data <<- function(data, FK_Event, method, FK_Species, Transect = "NULL", SampleNumber = "NULL", Element = "NULL", SubElement = "NULL", FieldSymbol, SpeciesQualifier = "NULL", FieldQualifier = "NULL", cParameter = "NULL", cParameter2 = "NULL", cParameter3 = "NULL", nValue = "NULL", nValue2 = "NULL", nValue3 = "NULL", cValue = "NULL", cValue2 = "NULL", cValue3 = "NULL", SyncKey, SyncState) {
  
  ## testing
  # FK_Event=checked_PK_Event
  
  ## Setting base variables to NULL
  ## default to "NULL" for most variables - update later
  # FK_Species="NULL"
  # Transect="NULL"
  # SampleNumber="NULL"
  # Element="NULL"
  # SubElement="NULL"
  # ##Field symbol usually the same as FK_Species
  # #FieldSymbol="NULL"
  # SpeciesQualifier="NULL"
  # FieldQualifier="NULL"
  # cParameter="NULL"
  # cParameter2="NULL"
  # cParameter3="NULL"
  # nValue="NULL"
  # nValue2="NULL"
  # nValue3="NULL"
  # cValue="NULL"
  # cValue2="NULL"
  # cValue3="NULL"
  
  ## setting sync key/states
  # SyncKey <- 33
  # SyncState <- 1
  
  ## get species list from database
  vgs_species_list_q <- paste0("SELECT PK_Species from Species where List = 'NRCS'")
  
  vgs_species_list <- dbGetQuery(mydb, vgs_species_list_q)
  # View(vgs_species_list)
  
  ## If Nested Freq - reset values for insert for that specific method
  if (method == "NF") {
    # data<- nest_freq_ready
    
    ## sample data is col 5:25 (T1-T20) usually
    ## last column is a summary column
    sample_data <- data[5:(ncol(data) - 1)]
    
    d <- 1
    while (d < nrow(data) + 1) {
      
      ## alerts for app stoppages ----------------------------------------------
      ## Species not in VGS .db species list = stop()
      if (length(grep(toupper(data[d, ][[1]]), vgs_species_list$PK_Species, value = T)) == 0) {
        shinyalert("Species Not in VGS!", paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for belt#", Transect), type = "error")
        Sys.sleep(15)
      }
      if (length(grep(toupper(data[d, ][[1]]), vgs_species_list$PK_Species, value = T)) == 0) stop(paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for belt#", Transect))
      
      ## Check length of species qualifier = stop() if over 20 char
      if (nchar(data[d, ][2]) > 20) {
        shinyalert("Species Qualifier too long!", paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (>20) for belt#", Transect," - ",data_file[batch_file]), type = "error")
        Sys.sleep(20)
      }
      if (nchar(data[d, ][2]) > 20) stop(paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (Max 20 char)"))
      ## end of checks to stop app for nested freq -----------------------------
      
      ## for each species - format if Species Qualifier is not null
      if (data[d, ][2] != "NULL") {
        ## get rid of problematic symbols
        data[d, ][2] <- gsub('"', "", data[d, ][2], fixed = T)
        data[d, ][2] <- gsub("'", "", data[d, ][2], fixed = T)
        ## add quotes
        data[d, ][2] <- paste0("'", data[d, ][2], "'")
      }
      
      ## each col / sample (starts at column 5)
      s <- 1
      while (s < ncol(sample_data) + 1) {
        if (!is.na(sample_data[d, s])) {
          PK_Sample <- GUID()
          
          insert_sample <- paste0("INSERT INTO Sample
           (PK_Sample
           ,FK_Event
           ,FK_Species
           ,Transect
           ,SampleNumber
           ,Element
           ,SubElement
           ,FieldSymbol
           ,SpeciesQualifier
           ,FieldQualifier
           ,cParameter
           ,cParameter2
           ,cParameter3
           ,nValue
           ,nValue2
           ,nValue3
           ,cValue
           ,cValue2
           ,cValue3
           ,SyncKey
           ,SyncState)
     VALUES
           (", PK_Sample, ",", FK_Event, ",'", toupper(data[d, ][[1]]), "',", Transect, ",", s, ",", as.numeric(sample_data[d, s]), ",", SubElement, ",'", toupper(data[d, ][[1]]), "',", data[d, ][2], ",", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",1,", nValue2, ",", nValue2, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
          
          ## insert NF data
          dbExecute(mydb, insert_sample)
        }
        ## move to next sample
        s <- s + 1
      }
      
      ## move to next row/species
      d <- d + 1
    }
    
    ## need to mark SYS_NONE if nothing in frame but did it ->
    ## after all species inserted - go back and check for sys_nones
    s <- 1
    while (s < ncol(sample_data) + 1) {
      ## check if data
      is_there_data <- !is.na(sample_data[s])
      ## if at least one true -> don't add anything
      sys_none_check <- grep("TRUE", is_there_data)
      ## if = 0 -> add SYS_NONE to sample# s
      if (length(sys_none_check) == 0) {
        PK_Sample <- GUID()
        
        insert_sample <- paste0("INSERT INTO Sample
           (PK_Sample
           ,FK_Event
           ,FK_Species
           ,Transect
           ,SampleNumber
           ,Element
           ,SubElement
           ,FieldSymbol
           ,SpeciesQualifier
           ,FieldQualifier
           ,cParameter
           ,cParameter2
           ,cParameter3
           ,nValue
           ,nValue2
           ,nValue3
           ,cValue
           ,cValue2
           ,cValue3
           ,SyncKey
           ,SyncState)
     VALUES
           (", PK_Sample, ",", FK_Event, ",'SYS_NONE',", Transect, ",", s, ",1,", SubElement, ",'SYS_NONE',NULL,", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",1,", nValue2, ",", nValue2, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
        
        ## insert NF data
        dbExecute(mydb, insert_sample)
      }
      s <- s + 1
    }
    print("Nested Freq inserted...")
  }
  
  ## If Ground Cover - reset values for insert for that specific method
  if (method == "GC") {
    # data<- gc_ready
    ## setting variables for tally insert
    Transect <- 1
    SampleNumber <- 1
    Element <- 0
    ## Update categories to PK_Species/Fk_Species/Field Symbol
    data$...4[data$...4 == "Litter"] <- "G_LITT"
    data$...4[data$...4 == "Rock"] <- "G_ROCK3"
    data$...4[data$...4 == "Vegetation"] <- "G_VEGE"
    data$...4[data$...4 == "Pavement"] <- "G_$QGJSWR6KN5"
    data$...4[data$...4 == "Moss"] <- "G_MOSS"
    data$...4[data$...4 == "Soil"] <- "G_$YKLSYQARFV"
    
    d <- 1
    while (d < nrow(data) + 1) {
      PK_Sample <- GUID()
      
      insert_sample <- paste0("INSERT INTO Sample
           (PK_Sample
           ,FK_Event
           ,FK_Species
           ,Transect
           ,SampleNumber
           ,Element
           ,SubElement
           ,FieldSymbol
           ,SpeciesQualifier
           ,FieldQualifier
           ,cParameter
           ,cParameter2
           ,cParameter3
           ,nValue
           ,nValue2
           ,nValue3
           ,cValue
           ,cValue2
           ,cValue3
           ,SyncKey
           ,SyncState)
     VALUES
           (", PK_Sample, ",", FK_Event, ",'", data[d, ][[1]], "',", Transect, ",", SampleNumber, ",", Element, ",", SubElement, ",'", data[d, ][[1]], "',", SpeciesQualifier, ",", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",", data[d, ][[2]], ",", nValue2, ",", nValue2, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
      
      ## insert GC data
      dbExecute(mydb, insert_sample)
      d <- d + 1
    }
    
    print("GC inserted...")
  }
}
## end of insert data function -------------------------------------------------





## in progress...
## insert statement to create folder schema for site
create_schema <- function() {
  ## in progress
}



