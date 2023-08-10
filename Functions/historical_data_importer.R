## Historical Data importer

## Create function to create GUID for VGS
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Read in libraries -----------------------------------------------------------
library(uuid)
library(tidyverse)
library(readxl)
library(DBI)
library(RSQLite)
library(stringr)

read_import_data <- function(Protocol, ServerKey, Protocol_2 = "NULL") {
  db_loc <- "C:/ProgramData/VGSData/VGS50.db"
  mydb <- dbConnect(RSQLite::SQLite(), dbname = db_loc)
  ## Save to parent environment for later
  ServerKey <<- ServerKey
  Protocol <<- Protocol
  Protocol_2 <<- Protocol_2
  output_list<<- data.frame(FileNumber=numeric(),CompletedFileList=character())
  
  ## choosing file to import
  data_file <<- choose.files("Choose Historical Data to import (excel)")
  ## reading in sheets to list
  historical_data <<- list()
  ## go through each batch file
  batch_file <<- 1
  while (batch_file < length(data_file) + 1) {
    print(" ")
    #cat("----->")
    
    print(paste0("Starting File ", batch_file, " : ", data_file[batch_file]))
    
    ## read specific batch file
    active_sheets <- excel_sheets(data_file[batch_file])
    ## don't care about VGS species list tab
    active_sheets <- active_sheets[active_sheets != "VGSDefaultSpeciesList"]
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
      batch_import(data_import)
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

## Looks at folder schema to see if need to create or exits already ------------
## step_x represent whatever the 'x' is on from previous function - the 'tab' its on
batch_import <- function(historical_raw_data) {
  ## testing
  # historical_raw_data<- data_import
  # View(historical_raw_data)
  
  ## SET KEY DATA --------------------------------------------------------------
  ## Site MetaData by ROW
  
  ## separate key data out by chunks -> alter to find specific keys for batch import
  ## alter the grep "pattern" in quotes to help find correct rows
  
  ## only does this is there is site meta data
  ## if it is the first sheet/tab or it is a specific sheet tab set by user
  if (x == 1 || active_sheets[batch_file] == "Site Metadata" || active_sheets[batch_file] == "SiteData") {
    print(paste0("Reading ", active_sheets[x]))
    print(paste0("Found SiteData info for file ", batch_file, " -> ", active_sheets[x]))
    print(paste0("Identifying keys for ", active_sheets[x]))
    
    ## SITE TABLE -->
    {
      # site_name <- historical_raw_data[grep("key", historical_raw_data[[1]], ignore.case = TRUE),][[2]]
      site_name <<- historical_raw_data[grep("siteID", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
      event_date <<- historical_raw_data[grep("date", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
      event_date <<- as.Date(as.numeric(event_date), origin = "1899-12-30")
      event_date <<- format(event_date, "%Y-%m-%d")
      event_date <<- paste0(event_date, " 00:00:00")
      ## spelling error for key below -->
      elevation <<- historical_raw_data[grep("elevat", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
      # elevation <- historical_raw_data[grep("elevation", historical_raw_data[[1]], ignore.case = TRUE),][[2]]
      slope <<- historical_raw_data[grep("slope", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
      aspect <<- historical_raw_data[grep("aspect", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
      # site_notes_1 <- historical_raw_data[grep("general", historical_raw_data[[1]], ignore.case = TRUE),][[2]]
      site_notes_1 <- historical_raw_data[grep("site notes", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
      site_notes_1 <- gsub('"', "", site_notes_1, fixed = T)
      site_notes_1 <- gsub("'", "", site_notes_1, fixed = T)
      # site_notes_2 <- historical_raw_data[grep("mlra", historical_raw_data[[1]], ignore.case = TRUE),][[2]]
      site_notes_2 <- historical_raw_data[grep("event note", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
      site_notes_2 <- gsub('"', "", site_notes_2, fixed = T)
      site_notes_2 <- gsub("'", "", site_notes_2, fixed = T)
    }
    
    ## Section if 2 sections of site notes / MLRA notes
    ## NRCS AZ has this
    if (ServerKey == "NRCS AZ") {
      ## Formatting notes ---- change depending on batch data
      ## if only site_notes_1
      if (length(site_notes_1) > 0 && length(site_notes_2) == 0) {
        site_notes <<- site_notes_1
      }
      ## if only site_notes_2
      if (length(site_notes_1) == 0 && length(site_notes_2) > 0) {
        site_notes <<- site_notes_2
      }
      ## if both have notes
      if (length(site_notes_1) > 0 && length(site_notes_2) > 0) {
        site_notes <<- paste0(site_notes, " - ", site_notes_2)
      }
      ## if both NA -> no notes
      if (length(site_notes_1) == 0 && length(site_notes_2) == 0) {
        site_notes <<- "NULL"
      }
      ## set EventNotes to NULL
      EventNotes <<- "NULL"
      ## End of formatting notes if 2 sections of site notes / MLRA notes --------
    }
    
    ## Section if notes separate - site and event notes
    ## NRCS AZ has this
    if (ServerKey == "USFS R6-RR") {
      ## Formatting notes ---- change depending on batch data
      ## if site_notes_1 present
      if (length(site_notes_1) > 0 && !is.na(site_notes_1)) {
        site_notes <<- site_notes_1
      }
      if (length(site_notes_1) == 0 || is.na(site_notes_1)) {
        site_notes <<- "NULL"
      }
      ## event notes
      if (length(site_notes_2) > 0 && !is.na(site_notes_2)) {
        EventNotes <<- site_notes_2
      }
      if (length(site_notes_2) == 0 || is.na(site_notes_2)) {
        EventNotes <<- "NULL"
      }
      ## End of formatting notes
    }
    
    ## variable manipulation -----------------------------------------------------
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
    ## End of variable manipulation ----
    
    ## LOCATOR TABLE -->
    lat <- historical_raw_data[grep("latitude", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
    long <- historical_raw_data[grep("longitude", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
    
    ## if lat or long missing -> set to "NULL"
    if (is.na(lat) || is.na(long)) {
      lat <- "NULL"
      long <- "NULL"
    }
    
    print(paste0("Inserting ", active_sheets[x]))
    
    ## set protocol first
    # ProtocolName = "R6 Rogue River Nested Freq/GC/Line Intercept"
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
  
  ## OTHER TABS ---- other than the first and not siteMetaData
  ## if not site meta data of on site metadata page
  ## works for RR so far... ?
  if (x != 1) {
    print(paste0("Moving to ", active_sheets[x]))
    ## RR key - nested freq
    if (ServerKey == "USFS R6-RR" && (Protocol == "R6 Rogue River Nested Freq/GC/Line Intercept" || Protocol_2 == "R6 Rogue River Nested Freq/GC/Line Intercept")) {
      ## Processing Nested Freq Data (GC on cover page for R6 RR batch)
      
      print(paste0("Parsing Data for Nested Freq..."))
      ## selecting only data with no column names
      nf_data <- data_import[c(3:nrow(data_import)), c(1:ncol(data_import))]
      ## get rid of NA columns with no species listed
      nf_data <- nf_data %>%
        filter(!is.na(nf_data[1]))
      
      belt_num <- as.numeric(substr(active_sheets[x], nchar(active_sheets[x]), nchar(active_sheets[x])))
      
      if (nrow(nf_data) == 0) {
        print(paste0("No Nested Freq for this Belt ", belt_num))
      }
      
      ## only insert if data present
      if (nrow(nf_data) > 0) {
        ## save as data frame
        nf_data <- as.data.frame(nf_data)
        ## row 2 = Qualifiers(SpeciesQ/FieldQ) -> NA should be NULL
        nf_data[, 2][is.na(nf_data[, 2])] <- "NULL"
        ## change all 4's to 0's -> 0 means it is in the largest frame
        nf_data[nf_data == 4] <- 0
        
        ## get rid of rows if have a species but has no hits (sum of zero for freq)
        nest_freq_ready <- nf_data %>% filter(nf_data$...25 != 0)
        # View(nest_freq_ready)
        
        ## Set FK_Event - Query .db for correct PK_Event
        ## complex for no reason - ehhhhhh
        find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
INNER JOIN Site ON Site.PK_Site = Event.FK_Site
where ProtocolName LIKE'%Nested Freq%' AND Protocol.Date = '", event_date, "'
AND SiteID = '", site_name, "'", " AND
EventName = 'Frequency (by quadrat)'")
        
        Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
        checked_PK_Event <- Event_guid_info$`quote(PK_Event)`[1]
        
        # stop("stop here")
        
        ## insert nested freq data
        insert_data(data = nest_freq_ready, method = "NF", FK_Event = checked_PK_Event, Transect = belt_num)
      }
    }
    
    ## BTNF key - nested freq
    if (ServerKey == "USFS R4-BT") {
      ## not done yet...
      # ## look for species label
      # find_sp_data<- grep("Species",historical_raw_data$`Nested Frequency Data`)
      # data<- historical_raw_data[c(find_sp_data+2:nrow(historical_raw_data)),c(2:ncol(historical_raw_data))]
      #
      # nested_freq_data<<- data[c(1:find_gc_data-1),] %>%
      #   filter(!is.na(`Nested Frequency Data`))
      # ## change all NAs to zero??
      # #View(nested_freq_data)
      #
      # find_gc_data<<- grep("Ground Cover",data$`Nested Frequency Data`)
      # gc_data<- data[c(find_gc_data:nrow(data)),] %>%
      #   filter(!is.na(`Nested Frequency Data`))
    }
  }
  ## move to next tab
  x <<- x + 1
}





## insert statement to create folder schema for site
create_schema <- function() {
  ## in progress
}

## insert statement to create site - defaults to local (SyncState=1)
create_site <- function(SiteID, Notes, ProtocolName, ProtocolName_2, Event_Date, EventNotes = "NULL", DDLat = "NULL", DDLong = "NULL", IsPrimary = 1, Slope = "NULL", Aspect = "NULL", Elevation = "NULL", DateEstablished = "NULL", SyncKey = 33, SyncState = 1, FK_Species_Site = "'SITE_KEY'", FK_Species_SiteStatus = "'SST_ACTIVE'", FK_Species_ElevUnits = "'UNIT_FEET'", FK_Species_Locator = "'LOC_MARKER'", LocatorID = "NULL", L_Description = "NULL", L_Date = "NULL", LocatorElevation = "NULL", L_SyncKey = 33, L_SyncState = 0, DateEnd = "NULL", Bailiwick = "NULL", FK_SiteClass = "NULL") {
  
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
  if (DateEstablished != "NULL" || is.na(DateEstablished) || length(DateEstablished) == 0) {
    DateEstablished <- paste0("'", DateEstablished, "'")
  }
  
  
  ## Guid for site pk
  PK_Site <- GUID()
  ## insert site sql statement
  insert_site <- paste0("INSERT INTO Site
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
  print(paste0("Site created -> ", SiteID))
  
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
    print(paste0("Locator created -> ", SiteID))
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
  
  ## check if found protocol
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
    ## formatting xlm
    all_attributes[i] <- gsub("&lt;", "<", all_attributes[i], fixed = T)
    all_attributes[i] <- gsub("&gt;", ">", all_attributes[i], fixed = T)
    all_attributes[i] <- gsub("[\r\n]", "", all_attributes[i], fixed = T)
    
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
           (", PK_EventGroup_GUIDS[[i]], ",", Hex(all_FK_Type_EventGroups[[i]]), ",", protocol_pk_check[[1]], ",", all_attributes[[i]], ",", all_groupNames[[i]], ",", all_displayOrders[[i]], ",", Hex(all_formIDs[[i]]), ",", SyncKey, ",", SyncState, ")")
      
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
         (", PK_Event, ",", Hex(all_FK_Type_Events[i]), ",", site_pk_check[[1]], ",", FK_SiteClass, ",", eventG_pk_check[[1]], ",", all_EventNames[i], ",", all_EventAttributes[i], ",", all_PageNumbers[i], ",", all_EntryOrders[i], ",", Hex(all_DefaultEventIDs[i]), ",", SyncKey, ",", SyncState, ")")
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
      ## formatting xlm
      all_attributes[i] <- gsub("&lt;", "<", all_attributes[i], fixed = T)
      all_attributes[i] <- gsub("&gt;", ">", all_attributes[i], fixed = T)
      all_attributes[i] <- gsub("[\r\n]", "", all_attributes[i], fixed = T)
      
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
           (", PK_EventGroup_GUIDS[[i]], ",", Hex(all_FK_Type_EventGroups[[i]]), ",", protocol_pk_check_2[[1]], ",", all_attributes[[i]], ",", all_groupNames[[i]], ",", all_displayOrders[[i]], ",", Hex(all_formIDs[[i]]), ",", SyncKey, ",", SyncState, ")")
        
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
         (", PK_Event, ",", Hex(all_FK_Type_Events[i]), ",", site_pk_check[[1]], ",", FK_SiteClass, ",", eventG_pk_check[[1]], ",", all_EventNames[i], ",", all_EventAttributes[i], ",", all_PageNumbers[i], ",", all_EntryOrders[i], ",", Hex(all_DefaultEventIDs[i]), ",", SyncKey, ",", SyncState, ")")
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
  
  ## Start of data inserting ---------------------------------------------------
  ## specific to R6 RR
  if (ServerKey == "USFS R6-RR") {
    # stop("stoppeding to check for protocol insert...S")
    
    ## Set FK_Event - Query .db for correct PK_Event
    ## complex for no reason - ehhhhhh - hard code query for EventName
    find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
    INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
    INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
    INNER JOIN Site ON Site.PK_Site = Event.FK_Site
    where ProtocolName LIKE'%Tally%' AND Protocol.Date = '", Event_Date, "' AND
    Protocol.SyncKey = ", SyncKey, " AND SiteID = '", SiteID, "'", "
    AND EventName = 'Point Ground Cover (by tally)'")
    
    Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
    checked_PK_Event <- Event_guid_info$`quote(PK_Event)`[1]
    
    ## GC tally data - done on meta-data page
    print(paste0("Parsing Data for GC Tally Data..."))
    ## saving gc data only with no column titles
    gc_data <- data_import[c(4:9), c(4:5)]
    ## get rid of NA columns with no species listed
    gc_data <- gc_data %>%
      filter(!is.na(gc_data[2]))
    
    ## if no gc data
    if (nrow(gc_data) == 0) {
      print(paste0("No GC data for file ", batch_file))
      ## update event notes to remark no gc data for this event
      update_event_notes <- paste0("Update Protocol
               Set Notes = 'No Ground Cover data for this event'
               Where PK_Protocol=", checked_PK_Event)
      dbExecute(mydb, update_event_notes)
    }
    
    ## only insert if data present
    if (nrow(gc_data) > 0) {
      ## save as data frame
      gc_data <- as.data.frame(gc_data)
      ## change NA's to 0
      gc_data[is.na(gc_data)] <- 0
      ## get rid of rows if have a species but has no hits (sum of zero for freq)
      gc_ready <- gc_data %>% filter(gc_data$...5 != 0)
      # View(gc_ready)
      
      ## insert gc data
      insert_data(data = gc_ready, method = "GC", FK_Event = checked_PK_Event)
    }
  }
  ## End of GC insert
}
## end of create_site() function / tab 1


## insert statement to add data - nested frequency
insert_data <- function(data, FK_Event, method, FK_Species, Transect = "NULL", SampleNumber = "NULL", Element = "NULL", SubElement = "NULL", FieldSymbol, SpeciesQualifier = "NULL", FieldQualifier = "NULL", cParameter = "NULL", cParameter2 = "NULL", cParameter3 = "NULL", nValue = "NULL", nValue2 = "NULL", nValue3 = "NULL", cValue = "NULL", cValue2 = "NULL", cValue3 = "NULL", SyncKey, SyncState) {
  
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
  SyncKey <- 33
  SyncState <- 1
  
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
      ## for each species
      ## check if in vgs species list or stop script
      if (length(grep(toupper(data[d, ][[1]]), vgs_species_list$PK_Species, value = T)) == 0) stop(paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for belt#", Transect))
      ## check length of species qualifier
      if (nchar(data[d, ][2]) > 20) stop(paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (Max 20 char)"))
      
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
} ## end of insert data
