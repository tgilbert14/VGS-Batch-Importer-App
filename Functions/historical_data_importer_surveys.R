## Historical Data importer
## set environment
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## table to save added unique species for QA/QC
# species_added<<- data.frame(sp=character(),qualifier=character(),from=character())

## [[ 1st function ]]
## READ IMPORT DATA FUNCTION ---------------------------------------------------
## >This function reads in all excel tabs and saves them, sends them to next
##  function(s) for import data process. Primarily gets raw data and passes it
##  on to functions over and over until all data selected is imported...
read_import_data <<- function(Protocol, ServerKey) {
  
  ## get species list from database
  vgs_species_list_q <- paste0("SELECT PK_Species from Species where List = 'NRCS'")
  vgs_species_list <<- dbGetQuery(mydb, vgs_species_list_q)
  
  ## get full species list from database
  vgs_species_list_q2 <- paste0("SELECT PK_Species, SpeciesName, CommonName from Species where List = 'NRCS'")
  vgs_species_list_more <<- dbGetQuery(mydb, vgs_species_list_q2)
  
  ## get full species list from database
  vgs_species_list_q3 <- paste0("SELECT PK_Species, NewSynonym from Species where List = 'NRCS'
                                AND NewSynonym IS NOT NULL AND NewSynonym != ''")
  vgs_species_list_least <<- dbGetQuery(mydb, vgs_species_list_q3)
  
  ## Save to parent environment for later
  ServerKey <<- "Survey"
  Protocol <<- Protocol
  output_list <<- data.frame(FileNumber = numeric(), CompletedFileList = character())
  
  ## choosing files to import - built to handle multiple
  data_file <<- choose.files(
    default = "pick excel file(s)",
    multi = T, caption = "Select Historical Batch Data Files(s) to Import"
  )
  
  ## variable to check if user selected anything 'the void'
  the_void <- character(0)
  
  if (identical(data_file, the_void)) {
    ## alert for data import start -> if identical, no data selected
    shinyalert("No Data Selected", "Refresh the page and choose a file!",
               imageUrl = "https://portal.vgs.arizona.edu/Content/Images/VGS_DarkGreen.png",
               imageWidth = 100, imageHeight = 100, type = "error", immediate = T,
               timer = 3500
    )
    ## wait 5 seconds, then stop the script
    Sys.sleep(5)
    stop(paste0("No Data Selected - Choose a file!"))
  }
  
  ## if NOT IN Power Mode
  if (power_mode == FALSE) {
    ## alert for data import start ->
    shinyalert("...is crunching your data now", "Please wait, looking for data keys",
               imageUrl = "https://portal.vgs.arizona.edu/Content/Images/VGS_DarkGreen.png",
               imageWidth = 100, imageHeight = 100, type = "success", closeOnClickOutside = T,
               showConfirmButton = F, immediate = TRUE
    )
  }
  ## if IN Power Mode
  if (power_mode == TRUE) {
    ## alert for data import start ->
    shinyalert("...is running in POWER MODE", "All data will try to be forced into the VGS database",
               imageUrl = "https://portal.vgs.arizona.edu/Content/Images/VGS_DarkGreen.png",
               imageWidth = 100, imageHeight = 100, type = "success", closeOnClickOutside = T,
               showConfirmButton = F, timer = 3500, immediate = T
    )
  }
  
  ## reading in sheets to list
  historical_data <<- list()
  ## go through each batch file starting w/ 1
  batch_file <<- 1
  while (batch_file < length(data_file) + 1) {
    # cat("----->")
    f_name <- paste0(data_file[batch_file])
    file_on <<- basename(f_name)
    
    print(paste0("Moving to File ", batch_file, " : ", file_on))
    
    #print("Setting 'add_to_belt' var to NA")
    ## var to update belt # per file
    #add_to_belt<<- NA ##dont need belts for surveys
    
    #print(paste0(add_to_belt," = add_to_belt"))

    shinyalert(paste0("Working on file #", batch_file, "/", length(data_file)), file_on,
               type = "success", immediate = T, showConfirmButton = T
    )
    
    ## read specific batch file
    active_sheets <- excel_sheets(data_file[batch_file])
    
    # ## don't care about VGS species list tab / ref sheets
    # active_sheets <- active_sheets[active_sheets != "VGSDefaultSpeciesList"]
    # active_sheets <- active_sheets[active_sheets != "Species Richness"]
    # active_sheets <- trimws(active_sheets)
    # 
    # ## reorder active sheets for R4 BT to make sure site is created 1st!
    # if (ServerKey == "USFS R4-BT") {
    #   reorder_sheets <- active_sheets[active_sheets != "LPI All Lines (500 points)"]
    #   reorder_sheets <- reorder_sheets[reorder_sheets != "Line Intercept (All Lines)"]
    #   reorder_sheets <- reorder_sheets[reorder_sheets != "Production- 3 Transects"]
    #   reorder_sheets <- reorder_sheets[reorder_sheets != "Production- 4 Transects"]
    #   
    #   reorder_sheets <- append(reorder_sheets, "LPI All Lines (500 points)")
    #   reorder_sheets <- append(reorder_sheets, "Line Intercept (All Lines)")
    #   reorder_sheets <- append(reorder_sheets, "Production- 3 Transects")
    #   reorder_sheets <- append(reorder_sheets, "Production- 4 Transects")
    #   active_sheets <- reorder_sheets
    # }
    
    # Loop through each sheet and read the data into a data frame
    for (sheet_name in active_sheets) {
      suppressMessages(historical_data[[sheet_name]] <- read_excel(data_file[batch_file], sheet = sheet_name, col_names = F, trim_ws = T))
      print(paste0("Saving ", sheet_name))
    }
    
    active_sheets <<- active_sheets
    ## go through each sheet 'x'
    x <<- 1
    
    while (x < (length(active_sheets) + 1)) {
      ## get all info from each excel sheet -> Reading one sheet at a time
      data_import <<- historical_data[[x]]
      
      ## messages for testing
      print(paste("Length of this data tabs =",length(active_sheets)))
      print(paste("x =",x))
      #print(head(data_import))
      
      ## function for key info and data import
      batch_import(historical_raw_data = data_import)
      ## move to next tab
      x <<- x + 1
      
      ## move site to correct folder or create parent folders
      print("next excel tab...")
    }
    
    print("next batch file...")
    ## clearing variables so not reused...
    suppressWarnings(rm(hi))
    suppressWarnings(rm(hi2))
    suppressWarnings(rm(whole_num_check))
    suppressWarnings(rm(whole_num_check_confirm))
    suppressWarnings(rm(tot_num_belts))
    suppressWarnings(rm(sum_per_gc))
    suppressWarnings(rm(Sum_of_nesteds))
    suppressWarnings(rm(trim_sheet_name))
    suppressWarnings(rm(site_name))
    suppressWarnings(rm(belt_num))
    suppressWarnings(rm(belt_numbers))
    suppressWarnings(rm(belts))
    suppressWarnings(rm(checked_PK_Event_gc))
    suppressWarnings(rm(checked_PK_Event_nf))
    suppressWarnings(rm(col_as_num))
    suppressWarnings(rm(cols.num))
    suppressWarnings(rm(Element_raw))
    suppressWarnings(rm(elevation))
    suppressWarnings(rm(event_date))
    suppressWarnings(rm(find_event_guid))
    suppressWarnings(rm(first_belt))
    suppressWarnings(rm(first_SampleNumber_raw))
    suppressWarnings(rm(insert_site))
    suppressWarnings(rm(is_whole_number))
    suppressWarnings(rm(lat))
    suppressWarnings(rm(long))
    suppressWarnings(rm(new))
    suppressWarnings(rm(rounded_hits))
    suppressWarnings(rm(SampleNumber_raw))
    suppressWarnings(rm(site_notes))
    suppressWarnings(rm(site_notes_1))
    suppressWarnings(rm(site_notes_2))
    suppressWarnings(rm(site_PK))
    suppressWarnings(rm(slope))
    suppressWarnings(rm(event_PK))
    
    ## saving completed files for output
    #output_list[batch_file, 1] <<- batch_file
    #output_list[batch_file, 2] <<- data_file[[batch_file]]

    ## move to next batch file
    batch_file <<- batch_file + 1
    
    
    if (batch_file == length(data_file) + 1) {
      print("**Batch Import Complete**")
    }
  }
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
  historical_raw_data <<- historical_raw_data
  
  ## SET KEY DATA --------------------------------------------------------------
  ## Site MetaData by ROW
  
  ## this should mark a tab/sheet that has the metadata (site info) to create site
  if (x == 1) { ## for now site data set to run for 1st tab only!
    SiteCheck <- TRUE
  } else {
    SiteCheck <- FALSE
  }
  
  
  ## THIS SECTION IS FOR META-DATA/FOLDER CREATION/SITE CREATION/LOCATORS -->
  if (SiteCheck == TRUE) {
    print(paste0("Reading ", active_sheets[x]))
    print(paste0("Found SiteData info for file ", batch_file, " -> ", active_sheets[x]))
    print(paste0("Identifying keys for ", active_sheets[x]))
    
    ## reset variables for folder schema
    suppressWarnings(list(
      rm(to_create_rd),
      rm(to_create_forest),
      rm(to_create_allotment),
      rm(to_create_pasture),
      rm(pasture),
      rm(allotment),
      rm(forest),
      rm(ranger_district),
      rm(pasture),
      rm(allotment),
      rm(forest),
      rm(ranger_district)
    ))
    
    ## finding meta data ->
    if (ServerKey == "Survey") {
      source("scripts/Surveys/site_info.R")
    }
    # if (ServerKey == "USFS R4-BT") {
    #   source("scripts/USFS R4/R4_BT_site_info.R")
    # }
    # if (ServerKey == "NRCS AZ") {
    #   source("scripts/NRCS AZ/NRCS_AZ_site_info.R")
    # }
    
    ## variable manipulation ---->
    ##update to account for QAQC checks for new excel version
    elevation<<- elevation[elevation!="pass-value numeric or left blank"]
    slope<<- slope[slope!="pass-value numeric or left blank"]
    aspect<<- aspect[aspect!="pass-value numeric or left blank"]
    
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
    
    ## get rid of problematic characters for site name
    site_name <<- gsub("'", "", site_name, fixed = T)
    site_name <<- gsub("&", "-", site_name, fixed = T)
    
    print(paste0("Inserting data from ", active_sheets[x]," for ",site_name))

    suppressWarnings(create_site(
      ProtocolName = Protocol, SiteID = site_name,
      Event_Date = event_date, Elevation = elevation, Slope = slope, Aspect = aspect,
      DDLat = lat, DDLong = long, EventNotes = EventNotes, Notes = site_notes
    ))
  }
  
  ## OTHER TABS ---- other than the first and not "site Meta Data" tabs
  ## if not site meta data of on site metadata page
  if (x != 1) {
    
    print(paste0("Moving to/checking ", active_sheets[x]))
    
    ## RR key - nested freq - check protocols as well
    if (ServerKey == "Survey") {
      #base::source("scripts/USFS R6/R6_RR_freq.R")
      base::source("scripts/Surveys/inq_surveys.R")
    } else {
      stop("Missing ServerKey for Surveys")
    }

    print(paste0("Finished ", active_sheets[x]))
    
  }
  
  ## reset this var
  rm(historical_raw_data)
}
## end of batch import data function -------------------------------------------


## [[ 3rd function ]]
## CREATE SITE FUNCTION --------------------------------------------------------
create_site <<- function(SiteID, Notes, ProtocolName, Event_Date, EventNotes = "NULL", DDLat = "NULL", DDLong = "NULL", IsPrimary = 1, Slope = "NULL", Aspect = "NULL", Elevation = "NULL", DateEstablished = "NULL", SyncKey = 33, SyncState = 1, FK_Species_Site = "'SITE_KEY'", FK_Species_SiteStatus = "'SST_ACTIVE'", FK_Species_ElevUnits = "'UNIT_FEET'", FK_Species_Locator = "'LOC_MARKER'", LocatorID = "NULL", L_Description = "NULL", L_Date = "NULL", LocatorElevation = "NULL", L_SyncKey = 33, L_SyncState = 1, DateEnd = "NULL", Bailiwick = "SITELOG", FK_SiteClass = "NULL") {
  ## adding quotes if data present
  if (site_notes != "NULL" || is.na(site_notes) || length(site_notes) == 0) {
    site_notes <- paste0("'", site_notes, "'")
  }
  
  ## extract num from slope/aspect/elevation if has value
  if (Slope != "NULL") {
    digit.slope <- as.numeric(gsub("[^0-9]", "", Slope))
    Slope <- digit.slope
  }
  if (Aspect != "NULL") {
    digit.aspect <- as.numeric(gsub("[^0-9]", "", Aspect))
    Aspect <- digit.aspect
  }
  if (Elevation != "NULL") {
    digit.elevation <- as.numeric(gsub("[^0-9]", "", Elevation))
    Elevation <- digit.elevation
  }
  
  if ((is.na(as.numeric(Aspect))&&Aspect!="NULL")  || (is.na(as.numeric(Slope))&&Slope!="NULL") || (is.na(as.numeric(Elevation))&&Elevation!="NULL"))  {
    print("Error! Aspect, Slope or Elevation contain an unexpected symbol")
    stop("Error! Aspect, Slope or Elevation contain an unexpected symbol")
  }
  
  ## Guid for site pk
  PK_Site <- GUID()
  ## save for another function - siteClassLink
  site_PK <<- PK_Site
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
  
  ## update messages for function
  print(paste0("Creating Folder Schema for Site"))
  
  # ## query site to get PK_Site for insert -->
  # site_check_q<- paste0("Select quote(PK_Site) from Site
  # Where SiteID = '",site_name,"'")
  #
  # ## execute query
  # site_pk<- dbGetQuery(mydb, site_check_q)
  # ## PK_Site for siteClassLink insert
  # site_PK<- site_pk$`quote(PK_Site)`
  
  ## query VGS db to see folders that are there already
  siteClassCheck <- paste0("Select quote(PK_SiteClass), quote(CK_ParentClass), ClassName from SiteClass")
  
  folder_check <- dbGetQuery(mydb, siteClassCheck)
  
  ## creating folders up to 4 layered schema, largest folder 1st 'Parent Folder'-->
  ## if it does not have nothing found and exists = then insert/create folder
  
  ## variables to make folder check easier
  to_create_rd <- (!grepl("Nothing found called", ranger_district) && nchar(ranger_district) > 0 &&
                     !grepl(paste0(ranger_district, " was found in the last column, there is nothing to the right of it!"), ranger_district) &&
                     !grepl(paste0(ranger_district, " was found in the last column, there is nothing to the left of it!"), ranger_district))
  
  to_create_forest <- (!grepl("Nothing found called", forest) && nchar(forest) > 0 &&
                         !grepl(paste0(forest, " was found in the last column, there is nothing to the right of it!"), forest) &&
                         !grepl(paste0(forest, " was found in the last column, there is nothing to the left of it!"), forest))
  
  to_create_allotment <- (!grepl("Nothing found called", allotment) && nchar(allotment) > 0 &&
                            !grepl(paste0(allotment, " was found in the last column, there is nothing to the right of it!"), allotment) &&
                            !grepl(paste0(allotment, " was found in the last column, there is nothing to the left of it!"), allotment))
  
  to_create_pasture <- (!grepl("Nothing found called", pasture) && nchar(pasture) > 0 &&
                          !grepl(paste0(pasture, " was found in the last column, there is nothing to the right of it!"), pasture) &&
                          !grepl(paste0(pasture, " was found in the last column, there is nothing to the left of it!"), pasture))
  
  ## formatting names/labels with more info
  ## then if user added already get rid of it and trim white space
  pasture <- paste0(pasture, " Pasture")
  pasture <- str_to_title(pasture)
  pasture <- gsub("(Pasture).*\\1", "\\1", pasture)
  pasture <- trimws(pasture)
  
  allotment <- paste0(allotment, " Allotment")
  allotment <- str_to_title(allotment)
  allotment <- gsub("(Allotment).*\\1", "\\1", allotment)
  allotment <- trimws(allotment)
  
  ranger_district <- paste0(ranger_district, " Ranger District")
  ranger_district <- str_to_title(ranger_district)
  ranger_district <- gsub("(Ranger District).*\\1", "\\1", ranger_district)
  ranger_district <- trimws(ranger_district)
  
  forest <- paste0(forest, " National Forest")
  forest <- str_to_title(forest)
  forest <- gsub("(National Forest).*\\1", "\\1", forest)
  forest <- trimws(forest)
  
  ## get rid of problematic characters for SQL
  pasture <- gsub("'", "", pasture, fixed = T)
  pasture <- gsub("&", "-", pasture, fixed = T)
  
  allotment <- gsub("'", "", allotment, fixed = T)
  allotment <- gsub("&", "-", allotment, fixed = T)
  
  ranger_district <- gsub("'", "", ranger_district, fixed = T)
  ranger_district <- gsub("&", "-", ranger_district, fixed = T)
  
  forest <- gsub("'", "", forest, fixed = T)
  forest <- gsub("&", "-", forest, fixed = T)
  
  
  ## creates folder under "local" folder - Default to Local
  parent <- paste0("X'11111111111111111111111111111111'")
  
  ## FOREST --------------------------------------------------------------------
  ## if value exists
  if (to_create_forest == TRUE) {
    ## check if folder exits already
    check <- folder_check %>%
      filter(ClassName == paste0(forest))
    ## if it does not exist - then create
    if (nrow(check) == 0) {
      ## update messages for function
      print(paste0("Creating Forest Folder"))
      create_schema(ClassName = paste0("'", forest, "'"), CK_ParentClass = parent, SitePK = "NULL")
    } ## if exists then leave as it...
  } else {
    (print(paste0("Forest Folder Already Exists")))
  }
  ## if blank
  if (to_create_forest == FALSE) {
    forest <- "Unknown Forest"
    ## update messages for function
    print(paste0("Creating Unknown Forest Folder"))
    create_schema(ClassName = paste0("'", forest, "'"), CK_ParentClass = parent, SitePK = "NULL")
  }
  ## END OF FOREST -------------------------------------------------------------
  
  ## query db for new siteClass folders
  folder_check <- dbGetQuery(mydb, siteClassCheck)
  
  ## filter to parent
  check_forest <- folder_check %>%
    filter(ClassName == paste0(forest))
  ## assign parent to current folder
  parent <- check_forest$`quote(PK_SiteClass)`
  
  ## RANGER DISTRICT------------------------------------------------------------
  ## if value exists (TRUE)
  if (to_create_rd == TRUE) {
    ## check if folder exits
    check <- folder_check %>%
      filter(ClassName == paste0(ranger_district))
    ## filter parent has correct parent folder (ranger district)
    check_2 <- check %>%
      filter(`quote(CK_ParentClass)` == check_forest$`quote(PK_SiteClass)`)
    ## if it does not exist - then create
    if (nrow(check_2) == 0) {
      ## update messages for function
      print(paste0("Creating RD Folder"))
      create_schema(ClassName = paste0("'", ranger_district, "'"), CK_ParentClass = parent, SitePK = "NULL")
    } else {
      (print(paste0("RD Folder Already Exists")))
    }
  }
  
  ## if blank
  if (to_create_rd == FALSE) {
    ## check if folder exits
    forest <- "Unknown RD"
    ## check if folder exits
    check_u <- folder_check %>%
      filter(ClassName == paste0(ranger_district))
    ## filter parent has correct parent folder (ranger district)
    check_u2 <- check_u %>%
      filter(`quote(CK_ParentClass)` == check_forest$`quote(PK_SiteClass)`)
    ## only create if does not exist
    if (nrow(check_u2) == 0) {
      ## update messages for function
      print(paste0("Creating Unknown RD Folder"))
      create_schema(ClassName = paste0("'", ranger_district, "'"), CK_ParentClass = parent, SitePK = "NULL")
    }
  }
  ## END OF RANGER DISTRICT ----------------------------------------------------
  
  ## query db for new siteClass folders
  folder_check <- dbGetQuery(mydb, siteClassCheck)
  ## parent
  check_f <- folder_check %>%
    filter(ClassName == paste0(ranger_district))
  ## filter parent has correct parent folder (ranger district)
  check_f2 <- check_f %>%
    filter(`quote(CK_ParentClass)` == check_forest$`quote(PK_SiteClass)`)
  
  ## assign parent to current folder
  parent <- check_f2$`quote(PK_SiteClass)`
  
  # if (test_mode == "allotment") {
  #   #check_rd<<- check_rd
  #   parent<<- parent
  #   check<<- check
  #   forest<<- forest
  #   allotment<<- allotment
  #   folder_check<<- folder_check
  #   stop("check allotment site insert...")
  # }
  
  
  ## ALLOTMENT -----------------------------------------------------------------
  ## allotment if value
  if (to_create_allotment == TRUE) {
    ## check if folder exits already
    check <- folder_check %>%
      filter(ClassName == paste0(allotment))
    ## now check if there is a folder, it has the same parent/filter to correct one
    check_2 <- check %>%
      filter(`quote(CK_ParentClass)` == check_f2$`quote(PK_SiteClass)`)
    ## if it does not exist - then create
    if (nrow(check_2) == 0) {
      ## update messages for function
      print(paste0("Creating Allotment Folder"))
      create_schema(ClassName = paste0("'", allotment, "'"), CK_ParentClass = parent, SitePK = "NULL")
    } else {
      (print(paste0("Allotment Folder Already Exists")))
    }
  }
  ## allotment if blank
  if (to_create_allotment == FALSE) {
    allotment <- "Unknown Allotment"
    ## check if folder exits already
    check_u <- folder_check %>%
      filter(ClassName == paste0(allotment))
    ## now check if there is a folder, it has the same parent/filter to correct one
    check_u2 <- check_u %>%
      filter(`quote(CK_ParentClass)` == check_f2$`quote(PK_SiteClass)`)
    ## only create if does not exist
    if (nrow(check_u2) == 0) {
      ## update messages for function
      print(paste0("Creating Unknown Allotment Folder"))
      create_schema(ClassName = paste0("'", allotment, "'"), CK_ParentClass = parent, SitePK = "NULL")
    }
  }
  ## END OF ALLOTMENT ----------------------------------------------------------
  
  ## query db for new siteClass folders
  folder_check <- dbGetQuery(mydb, siteClassCheck)
  ## filter to parent
  check <- folder_check %>%
    filter(ClassName == paste0(allotment))
  ## filter parent has correct parent folder (allotment)
  check_2 <- check %>%
    filter(`quote(CK_ParentClass)` == check_f2$`quote(PK_SiteClass)`)
  
  ## assign parent to current folder
  parent <- check_2$`quote(PK_SiteClass)`
  
  # if (test_mode == "pasture") {
  #   #check_rd<<- check_rd
  #   parent<<- parent
  #   check<<- check
  #   forest<<- forest
  #   allotment<<- allotment
  #   ranger_district<<- ranger_district
  #   folder_check<<- folder_check
  #   stop("check pasture site insert...")
  # }
  
  ## PASTURE -------------------------------------------------------------------
  ## pasture if value
  if (to_create_pasture == TRUE) {
    ## check if folder exits already
    check <- folder_check %>%
      filter(ClassName == paste0(pasture))
    ## check if parent PK matches parent of current folder
    check_2 <- check %>%
      filter(`quote(CK_ParentClass)` == check_2$`quote(PK_SiteClass)`)
    ## if it does not exist - then create
    if (nrow(check_2) == 0) {
      ## update messages for function
      print(paste0("Creating Pasture Folder and placing site"))
      create_schema(ClassName = paste0("'", pasture, "'"), CK_ParentClass = parent, SitePK = site_PK, PK_SiteClass = check_2$`quote(PK_SiteClass)`)
      ## if parent folder exists already, still need to put if correct folder
    } else {
      print(paste0("Pasture Folder Already Exists"))
      print(paste0("Placing Site in Folder"))
      update_schema(SitePK = site_PK, PK_SiteClass = check_2$`quote(PK_SiteClass)`)
    }
  }
  ## pasture if blank
  if (to_create_pasture == FALSE) {
    pasture <- "Unknown Pasture"
    check_u <- folder_check %>%
      filter(ClassName == paste0(pasture))
    ## check if parent PK matches parent of current folder
    check_u2 <- check_u %>%
      filter(`quote(CK_ParentClass)` == check_2$`quote(PK_SiteClass)`)
    ## if it does not exist - then create
    if (nrow(check_u2) == 0) {
      ## update messages for function
      print(paste0("Creating Unkown Pasture Folder and placing site"))
      create_schema(ClassName = paste0("'", pasture, "'"), CK_ParentClass = parent, SitePK = site_PK, PK_SiteClass = check_u2$`quote(PK_SiteClass)`)
      ## if parent folder exists already, still need to put if correct folder
    } else {
      print(paste0("Placing Site in Folder"))
      update_schema(SitePK = site_PK, PK_SiteClass = check_u2$`quote(PK_SiteClass)`)
    }
  }
  ## END OF PASTURE ------------------------------------------------------------
  ## end of folder creation
  
  print("Finding Protocol in .db")
  ## Protocols Section
  ## inserting protocol 1 -->
  
  PK_Protocol <- GUID()
  #ProtocolName <- "NRCS IIRH V5 Evaluation" ## for testing
  print(paste0("Making ", ProtocolName, " for - ", PK_Protocol))
  
  ## query to get correct FK_Type_Protocol from typelist by looking for ProtocolName
  find_protocol <- paste0("Select quote(PK_Type), Attributes from typelist
                          where ListItem LIKE'%", ProtocolName, "%'")
  
  TypeList_info <- DBI::dbGetQuery(mydb, find_protocol)
  
  FK_Type_Protocol <- TypeList_info$`quote(PK_Type)`
  FK_Type_Protocol <- tolower(FK_Type_Protocol)
  
  if (length(FK_Type_Protocol) == 0) {
    shinyalert("Missing Protocol!", paste0("Import protocol ", ProtocolName, " into VGS first and try again"),
               type = "error", immediate = T
    )
    Sys.sleep(5)
  }
  
  ## check if found protocol - boots out of VGS
  ## print message
  if (length(FK_Type_Protocol) == 0) print(paste0(ProtocolName, " not in VGS -> import protocol first"))
  ## stop app
  if (length(FK_Type_Protocol) == 0) stop(paste0(ProtocolName, " not in VGS -> import protocol first"))
  if (length(FK_Type_Protocol) > 1) stop(paste0(ProtocolName, " found twice in database!"))
  
  Attributes_raw <- TypeList_info$Attributes
  
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
  
  ## log statement
  print(paste0("insert protocol"))
  ## insert protocol into protocol table
  dbExecute(mydb, insert_protocol)
  
  ## update messages for function
  print(paste0("Creating Protocol Event for ", Event_Date))
  ## get FK_Type for EventGroup
  FK_TypeEventGroup.SurveyLog <- "x'7a13360586dd1e46bed5e27493805f0d'"
  FK_TypeEventGroup.SurveySite <- "x'5bcb216decfbe548969996e934004458'"
  FK_TypeEventGroup.SurveyFolder <- "x'57460d081dd9e742a3885a21ce6f6d67'"
  
  ## Survey in Log
  if (input$s.type == "Log") {
    fk_type_EG <- FK_TypeEventGroup.SurveyLog
    ## Survey in Site Description
  } else if (input$s.type == "Site") {
    fk_type_EG <- FK_TypeEventGroup.SurveySite
    ## Survey in Folder Description
  } else if (input$s.type == "Folder") {
    fk_type_EG <- FK_TypeEventGroup.SurveyFolder
    stop("Surveys for folders not implemented yet...")
    ## Type not found
  } else {
    stop(paste0("No EventGroup.FK_Type found for this Survey: ",ProtocolName))
  }
  
  ## insert EventGroup
  PK_EventGroup_GUID <- GUID()
  
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
           (", PK_EventGroup_GUID, ",", fk_type_EG, ",", PK_Protocol, ",NULL,'", ProtocolName, "',NULL,NULL,", SyncKey, ",", SyncState, ")")
  
  ## log statement
  print(paste0("insert event group to protocol"))
  ## insert eventGroups into eventGroup table
  dbExecute(mydb, insert_eventGroup)
  
  ## insert Event
  PK_Event <- GUID()
  ## save for inq insert
  event_PK <<- PK_Event
  
  ## Same for all 3 survey types
  fk_type_Event <- Hex("{7D2BD56D-CEF3-46A0-8982-6DEFC970F77B}")
  
  event_attributes <- Attributes_raw
  
  ## clean the attributes for problematic characters
  event_attributes <- gsub("'","",event_attributes)
  
  insert_event <- paste0("INSERT INTO Event
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
         (", PK_Event, ",", fk_type_Event, ",", PK_Site, ",", FK_SiteClass, ",", PK_EventGroup_GUID, ",'", ProtocolName, "','", event_attributes, "',NULL,NULL,NULL,", SyncKey, ",", SyncState, ")")

  ## log statement
  print(paste0("insert event to event group"))
  
  ## insert eventGroups into eventGroup table
  dbExecute(mydb, insert_event)
  
  
  
  ## get attributes before trim
  inq_att.pos.raw <- gregexpr("\\bAttributes\\b", Attributes_raw)
  inq_comment.pos.raw <- gregexpr("\\bComment\\b", Attributes_raw)
  
  ## insert Inquiry Questions, find into in event.attributes
  Attributes_trimmed<- gsub('"',"",Attributes_raw)
  inq_pk.pos <- gregexpr("\\bPK_Question\\b", Attributes_trimmed)
  inq_ck.pos <- gregexpr("\\bCK_Question\\b", Attributes_trimmed)
  inq_id.pos <- gregexpr("\\bInquiryId\\b", Attributes_trimmed)
  inq_fkItem.pos <- gregexpr("\\bFK_Items\\b", Attributes_trimmed)
  inq_item.pos <- gregexpr("\\bItems\\b", Attributes_trimmed)
  inq_text.pos <- gregexpr("\\bInquiryText\\b", Attributes_trimmed)
  inq_att.pos <- gregexpr("\\bAttributes\\b", Attributes_trimmed)
  inq_comment.pos <- gregexpr("\\bComment\\b", Attributes_trimmed)
  inq_tip.pos <- gregexpr("\\bTip\\b", Attributes_trimmed)
  inq_isExpanded.pos <- gregexpr("\\bIsExpanded\\b", Attributes_trimmed)
  inq_tempType.pos <- gregexpr("\\bTemplateType\\b", Attributes_trimmed)
  inq_sumType.pos <- gregexpr("\\bSummaryType\\b", Attributes_trimmed)
  
  ## run through inquiry to make new pk/ck connections
  ## or will have unique constraint fail
  pk_inq_list <- list() ## list of pk's
  pos=1
  while (pos < length(inq_id.pos[[1]])+1) {
    pk_inq_list[pos] <- paste0(substr(Attributes_trimmed, inq_pk.pos[[1]][pos]+12, inq_pk.pos[[1]][pos]+47))
    pos=pos+1
  }
  pk_inq_list_unique <<-  unique(pk_inq_list)
  
  ## go through each pk and update attributes
  pos=1
  Attributes_trimmed.newPKS <- Attributes_trimmed
  while(pos < length(pk_inq_list_unique)+1) {
    ## sub old guild with a new guid where found in attributes
    Attributes_trimmed.newPKS <- gsub(pk_inq_list_unique[[pos]], UUIDgenerate(), Attributes_trimmed.newPKS)
    pos=pos+1
  }
  
  pos=1
  while (pos < length(inq_id.pos[[1]])+1) {
    inquiry.pk <- paste0(substr(Attributes_trimmed.newPKS, inq_pk.pos[[1]][pos]+12, inq_pk.pos[[1]][pos]+47))
    inquiry.ck <- paste0(substr(Attributes_trimmed.newPKS, inq_ck.pos[[1]][pos]+12, inq_id.pos[[1]][pos]-2))
    inquiry.id <- paste0("'", substr(Attributes_trimmed.newPKS, inq_id.pos[[1]][pos]+10, inq_id.pos[[1]][pos]+19),"'")
    inquiry.fkItem <- paste0(substr(Attributes_trimmed.newPKS, inq_fkItem.pos[[1]][pos]+9, inq_item.pos[[1]][pos]-2))
    inquiry.text <- paste0("'", substr(Attributes_trimmed.newPKS, inq_text.pos[[1]][pos]+12, inq_att.pos[[1]][pos]-2),"'")
    inquiry.tip <- paste0("'", substr(Attributes_trimmed.newPKS, inq_tip.pos[[1]][pos]+4, inq_isExpanded.pos[[1]][pos]-2),"'")
    #inquiry.att <- paste0("'", substr(Attributes_trimmed.newPKS, inq_att.pos[[1]][pos]+11, inq_comment.pos[[1]][pos]-2),"'")
    inq.att.raw <- paste0("'", substr(Attributes_raw, inq_att.pos.raw[[1]][pos]+12, inq_comment.pos.raw[[1]][pos]-3),"'")
    
    ## updating NULLs when possible
    if (inquiry.ck == "null") {
      inquiry.ck <- 'NULL'
    } else {
      inquiry.ck <- Hex(inquiry.ck)
    }
      
    if (inquiry.fkItem == "null") {
      inquiry.fkItem <- 'NULL'
    } else {
      inquiry.fkItem <- Hex(inquiry.fkItem)
    }

    if (inquiry.tip == "null") {
      inquiry.tip <- 'NULL'
    }
    
    insert_inq <- paste0("INSERT INTO Inquiry
           (PK_Inquiry
           ,FK_Event
           ,FK_SpList
           ,CK_Parent
           ,InquiryID
           ,Attributes
           ,InquiryText
           ,Comment
           ,Tip
           ,SyncKey
           ,SyncState)
     VALUES
           (",Hex(inquiry.pk), ",",PK_Event, ",",inquiry.fkItem,",",inquiry.ck,",",inquiry.id, ",",inq.att.raw, ",",inquiry.text, ",NULL,NULL,", SyncKey,",",SyncState,")")
    
    ## log statment
    print(paste0("insert inquiry questions: ",pos))
    print(paste0("pk_inquiry: ",inquiry.pk))
    print(paste0("ck_parent: ",inquiry.ck))
    print(paste0("fk_spList: ",inquiry.fkItem))
    dbExecute(mydb, insert_inq)
    
    pos=pos+1
  }
  
  
  
  

  ## clearing all_forms variable if exists
  suppressWarnings(rm(all_formIDs))
  suppressWarnings(rm(all_groupNames))
  suppressWarnings(rm(all_attributes))
  suppressWarnings(rm(all_FK_Type_EventGroups))
  suppressWarnings(rm(all_displayOrders))
  suppressWarnings(rm(PK_EventGroup_GUIDS))

  ## End of Events -------------------------------------------------------------
  
  
  
  
  
  
  ## update messages for function
  print(paste0("EventGroups and Events attached to ", ProtocolName))
  ## End of protocol 1
  
  ## this section takes place only if 1st tab/page of xlsx or labeled as
  ## site meta data
  
  # if (test_mode == "data_page1") stop("stop here to check out raw data form")
  
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
  
  
  stop("pausing in insert_data()... historical_data_importer_surveys.R")
  
  
  ## If Nested Freq - reset values for insert for that specific method
  if (method == "NF") {
    # data<- nest_freq_ready
    # method <- "NF"
    # FK_Event <- checked_PK_Event
    # Transect <- belt_num
    # SampleNumber <- "NULL"
    # Element <- "NULL"
    # SubElement <- "NULL"
    # SpeciesQualifier <- "NULL"
    # FieldQualifier <- "NULL"
    # cParameter <- "NULL"
    # cParameter2 <- "NULL"
    # cParameter3 <- "NULL"
    # nValue <- "NULL"
    # nValue2 <- "NULL"
    # nValue3 <- "NULL"
    # cValue <- "NULL"
    # cValue2 <- "NULL"
    # cValue3 <- "NULL"
    # SyncKey <- 33
    # SyncState <- 1
    # power_mode <- FALSE
    
    print(paste("Inserting NF data..."))
    
    ## data validation check function
    data_quality_data_frame(data)
    
    ## Species replace file if box checked
    if (input$qaqc == TRUE) {
      print("Reading in SpReplace File...")
      ## if does not exist, read file in
      if (!exists("sp_replace_file")) {
        sp_replace_file <- openxlsx::read.xlsx("www/SpeciesReplace.xlsx")
        ## check state of species replace file
        if (length(sp_replace_file$OldCode) != length(sp_replace_file$NewCode)) {
          print("Incomplete SpReplace file, rows don't match...")
          stop("Incomplete SpReplace file, rows don't match...")
        }
      }
      
      print("Checking if sp need to be replaced for NF...")
      
      k <- 1
      ## trying to account for different data sheets (diff col depending on forest)
      if (ServerKey == "USFS R4-BT") {
        ## using data$...2
        while (k < nrow(sp_replace_file) + 1) {
          
          #print(paste("checking",sp_replace_file$OldCode[k]))
          
          data$...2 <- sub(
            pattern = paste0("^", sp_replace_file$OldCode[k], "$"),
            replacement = paste0(sp_replace_file$NewCode[k]),
            x = data$...2
          )
          k <- k + 1
        }
      } else {
        ## using diff col for diff clients
        while (k < nrow(sp_replace_file) + 1) {
          data$...1 <- sub(
            pattern = paste0("^", sp_replace_file$OldCode[k], "$"),
            replacement = paste0(sp_replace_file$NewCode[k]),
            x = data$...1
          )
          k <- k + 1
        }
      }
      ## end of is/else...
      print("Sp Replaced if found...")
    }
    ## End of qaqc species replace file
    
    ## last column is a summary column (for most)
    sample_data <- data[5:(ncol(data) - 1)]
    
    d <- 1
    while (d < nrow(data) + 1) {
      ## Error Checking ----
      ## message for data log for species in VGS check
      if (length(grep(paste0("^", toupper(data[d, ][[1]]), "$"), vgs_species_list$PK_Species, value = T)) == 0) print(paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for NF belt#", Transect, " - ", file_on))
      ## message for data log for species qualifier length
      if (nchar(data[d, ][2]) > 20) print(paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (Max 20 char) for belt#", Transect, " - ", file_on))
      
      ## stop app if not in Power Mode ->
      if (power_mode == FALSE) {
        ## alerts for app stoppages
        ## Species not in VGS .db species list = stop()
        if (length(grep(paste0("^", toupper(data[d, ][[1]]), "$"), vgs_species_list$PK_Species, value = T)) == 0) {
          ## message for data log for species in VGS check
          print(paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for belt#", Transect, " - ", file_on))
          ## alert for species
          shinyalert("Species Not in VGS!", paste0(
            "Species: ", toupper(data[d, ][[1]]),
            " not in VGS db for belt#", Transect, " - ", file_on
          ),
          type = "error", immediate = T
          )
          Sys.sleep(15)
        }
        
        if (length(grep(paste0("^", toupper(data[d, ][[1]]), "$"), vgs_species_list$PK_Species, value = T)) == 0) stop(paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for NF belt#", Transect, " - ", file_on))
        ## Check length of species qualifier = stop() if over 20 char
        ## print message for qualifier error
        if (nchar(data[d, ][2]) > 20) print(paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (Max 20 char) for belt#", Transect, " - ", file_on))
        ## pop up warning
        if (nchar(data[d, ][2]) > 20) {
          shinyalert("Species Qualifier too long!", paste0(
            "Species: ", toupper(data[d, ][[1]]),
            " Qualifier is too long (>20) for belt#",
            Transect, " - ", file_on
          ),
          type = "error",
          immediate = T
          )
          Sys.sleep(20)
        }
        ## stop app
        if (nchar(data[d, ][2]) > 20) stop(paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (Max 20 char) for belt#", Transect, " - ", file_on))
      } ## end of if in power mode
      ## End of Error checking ----
      
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
        PK_Sample <- GUID()
        
        ## trim white space for numbers
        Element <- as.numeric(str_trim(sample_data[d, s]))
        
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
           (", PK_Sample, ",", FK_Event, ",'", toupper(data[d, ][[1]]), "',", Transect, ",", s, ",", Element, ",", SubElement, ",'", toupper(data[d, ][[1]]), "',", data[d, ][2], ",", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",1,", nValue2, ",", nValue3, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
        
        if (!is.na(sample_data[d, s])) {
          
          ## only insert if not in power mode
          if (power_mode == "FALSE") {
            ## for troubleshootin
            #print(paste("Inserting NF code",toupper(data[d, ][[1]]),"on Transect",Transect,"- Sample",s,"- Element",Element,"-",SubElement))
            ## insert NF data
            dbExecute(mydb, insert_sample)
          }
          
        }
        ## move to next sample
        s <- s + 1
      }
      
      ## move to next row/species
      d <- d + 1
    }
    
    ## not inserting None's for now -> more common to have incomplete data than
    ## none's, need to add to excel sheet a way to select 'None'
    
    # ## after all species inserted - go back and check for sys_none
    # s <- 1
    # while (s < ncol(sample_data) + 1) {
    #   ## check if data
    #   is_there_data <- !is.na(sample_data[s])
    #   ## if at least one true -> don't add anything
    #   sys_none_check <- grep("TRUE", is_there_data)
    #   ## if = 0 -> add SYS_NONE to sample# s
    #   if (length(sys_none_check) == 0) {
    #     PK_Sample <- GUID()
    #     
    #     insert_sample <- paste0("INSERT INTO Sample
    #        (PK_Sample
    #        ,FK_Event
    #        ,FK_Species
    #        ,Transect
    #        ,SampleNumber
    #        ,Element
    #        ,SubElement
    #        ,FieldSymbol
    #        ,SpeciesQualifier
    #        ,FieldQualifier
    #        ,cParameter
    #        ,cParameter2
    #        ,cParameter3
    #        ,nValue
    #        ,nValue2
    #        ,nValue3
    #        ,cValue
    #        ,cValue2
    #        ,cValue3
    #        ,SyncKey
    #        ,SyncState)
    #  VALUES
    #        (", PK_Sample, ",", FK_Event, ",'SYS_NONE',", Transect, ",", s, ",1,", SubElement, ",'SYS_NONE',NULL,", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",1,", nValue2, ",", nValue3, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
    #     
    #     ## only insert if not in power mode
    #     if (power_mode == "FALSE") {
    #       ## insert NF data
    #       dbExecute(mydb, insert_sample)
    #     }
    #     
    #   }
    #   
    #   s <- s + 1
    # }
    
    print("Nested Freq inserted...")
  }
  
  ## If Ground Cover - reset values for insert for that specific method
  if (method == "GC-tally") {
    # data<- gc_ready
    
    ## setting variables for tally insert
    Transect <- 1
    SampleNumber <- 1
    Element <- 0
    
    d <- 1
    while (d < nrow(data) + 1) {
      PK_Sample <- GUID()
      
      ## trim white space for numbers
      nValue <- as.numeric(str_trim(data[d, ][[2]]))
      
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
           (", PK_Sample, ",", FK_Event, ",'", data[d, ][[1]], "',", Transect, ",", SampleNumber, ",", Element, ",", SubElement, ",'", data[d, ][[1]], "',", SpeciesQualifier, ",", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",", nValue, ",", nValue2, ",", nValue3, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
      
      ## does not insert if POWER MODE
      if (power_mode == "FALSE") {
        ## insert GC data
        dbExecute(mydb, insert_sample)
      }
      d <- d + 1
    }
    
    print("GC tally inserted...")
  }
  
  ## If Ground Cover - reset values for insert for that specific method
  if (method == "GC") {
    # data<- hi2
    
    ## pulling list of GC species, e.g., ('G_ROCK3','G_$QH0J18RPQ5', etc.)
    
    ## randomize ground cover list
    gc_random <- sample(hi2)
    
    d <- 1
    while (d < length(data) + 1) {
      PK_Sample <- GUID()
      
      if (power_mode == "FALSE") {
        if (is.na(Transect[d]) || is.na(SampleNumber[d]) || is.na(Element[d]) || is.na(gc_random[d])) {
          print("GC insert error - NA's exist -> too many GC points?")
          
          shinyalert("GC Insert Error", "NA's exist, too many GC points?",
                     type = "error",
                     confirmButtonCol = T, confirmButtonText = "Fix me :(",
                     immediate = T
          )
          Sys.sleep(10)
          ## stop app if not in Power Mode ->
          if (power_mode == FALSE) {
            stop(paste0("GC insert error for ", site_name, " - ", belt_num))
          }
          
        }
      }

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
           (", PK_Sample, ",", FK_Event, ",'", gc_random[d], "',", Transect[d], ",", SampleNumber[d], ",", Element[d], ",", SubElement, ",'", gc_random[d], "',", SpeciesQualifier, ",", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",1,", nValue2, ",", nValue3, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
      
      ## does not insert if POWER MODE
      ## had to add to create report otherwise app crashes
      if (power_mode == "FALSE") {
        ## insert GC data
        dbExecute(mydb, insert_sample)
      }

      d <- d + 1
    }
    
    print("GC inserted...")
  }
  
  ## not fully tested... in progress
  ## If LPI - reset values for insert for that specific method
  if (method == "LPI") {
    # data<- temp_lpi
    
    ## pulling data frame w/ Transect, Sample, Species, and SubElement
    
    ## data validation check function
    data_quality_data_frame(data)
    
    ## Species replace file if box checked
    if (input$qaqc == TRUE) {
      
      ## if does not exist, read file in
      if (!exists("sp_replace_file")) {
        sp_replace_file <- openxlsx::read.xlsx("www/SpeciesReplace.xlsx")
        sp_replace_file <<- sp_replace_file
      }
      
      print("Checking if sp need to be replaced for LPI...")
      
      k <- 1
      ## trying to account for different data sheets (diff col depending on forest)
      #if (ServerKey == "USFS R4-BT") {
      ## always runs for now...
      if (0==0) {
        ## using data$...2
        while (k < nrow(sp_replace_file) + 1) {
          
          data$Species <- sub(
            pattern = paste0("^", sp_replace_file$OldCode[k], "$"),
            replacement = paste0(sp_replace_file$NewCode[k]),
            x = data$Species
          )
          k <- k + 1
        }
      } #else {
      #   ## leave here in case need to update?
      #   while (k < nrow(sp_replace_file) + 1) {
      #     data$...1 <- sub(
      #       pattern = paste0("^", sp_replace_file$OldCode[k], "$"),
      #       replacement = paste0(sp_replace_file$NewCode[k]),
      #       x = data$...1
      #     )
      #     k <- k + 1
      #   }
      # }
      ## end of is/else...
    }
    ## End of qaqc species replace file
    
    ## filter to not check for gc - species only (GC are char 13)
    sp_check <- data %>%
      filter(nchar(Species) != 13)
    
    sp_check_sp <- unique(sp_check$Species)
    
    sp_num <- 1
    while (sp_num < length(sp_check_sp) + 1) {
      
      if (length(grep(paste0("^", toupper(sp_check_sp[sp_num]), "$"), vgs_species_list$PK_Species, value = T)) == 0) {
        ## message for sp_check log for species in VGS check
        print(paste0("Species: ", toupper(sp_check_sp[sp_num]), " not in VGS db for LPI - ", file_on))
      }
      
      ## stop app if not in Power Mode
      if (power_mode == FALSE) {
        ## alerts for app stoppages --------------------------------------------
        ## Species code not in VGS .db species list = stop()
        if (length(grep(paste0("^", toupper(sp_check_sp[sp_num]), "$"), vgs_species_list$PK_Species, value = T)) == 0) {
          
          ## alert for species
          shinyalert("Species Not in VGS!", paste0(
            "Species: ", toupper(sp_check_sp[sp_num]),
            " not in VGS db for LPI - ", file_on
          ), type = "error", immediate = T)
          ## pause and then stop app
          Sys.sleep(15)
          stop(paste0("Species: ", toupper(sp_check_sp[sp_num]), " not in VGS db for LPI - ", file_on))
        }
        ## end of checks to stop app for species
      } ## end of if in power mode
      sp_num <- sp_num + 1
    }
    
    
    d <- 1
    while (d < nrow(data) + 1) {
      
      ## Surface Cover / 'S' options -> Specific to Protocols
      ## if species can be a gc or canopy and if before a basal hit - add 'S'
      
      ## for USFS R4 BTNF - 'L' and 'WL'
      if ((ServerKey == "USFS R4-BT") &&
          (data$Species[d] == "G_$8UEFABAVX9" || data$Species[d] == "G_$R4RTL5LFIN")) {
        cParameter <- paste0("'Surface'")
      } else {
        cParameter <- paste0("NULL")
      }
      # View(data)
      
      ## add ground cover check?? make sure species in sheet is basal?
      
      ## also need to track what is being entered for QA/QC after like Freq...
      
      ## each col / sample insert
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
           (", PK_Sample, ",", FK_Event, ",'", data$Species[d], "',", data$Transect[d], ",", data$Sample[d], ",1,", data$SubElement[d], ",'", data$Species[d], "',", SpeciesQualifier, ",", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",1,", nValue2, ",", nValue3, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
      
      ## only insert if not in power mode
      if (power_mode == "FALSE") {
        
        ## for troubleshootin
        #print(paste("Inserting NF code",toupper(data[d, ][[1]]),"on Transect",Transect,"- Sample",s,"- Element",Element,"-",SubElement))
        
        ## insert LPI data
        dbExecute(mydb, insert_sample)
      }
      
      ## move to next row/species
      d <- d + 1
    }
    
    print("LPI data inserted...")
  }
  
  ## Not tested or updated yet -> pasted from other method...
  ## If Line Intercept...
  if (method == "LI") {
    # data<- temp_lpi
    
    ## need to find FK_event for height just in case you need ->
    ## this is for R6! Need to adjust for each protocol...
    if (ServerKey == "USFS R6-RR") {
      
      term <- "Standard"
      eventName <- "Height (feet)"
      
      find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
INNER JOIN Site ON Site.PK_Site = Event.FK_Site
where ProtocolName LIKE '%",term,"%' AND Protocol.Date = '",event_date, "'
AND SiteID = '", site_name, "'", " AND
EventName = '",eventName,"'")
      
      Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
      FK_Event_height <- Event_guid_info$`quote(PK_Event)`[1]
    }
    
    ## checking columns have correct names - if not, correct
    if (names(data)[1] != "Belt" && data[1,1] == "Belt") {
      ## making 1st row (labels) the col headers
      colnames(data) <- data[1, ]
      data <- data[-1, ]
    }
    
    ## update all NA's to NULLs fgor SQL insert
    data <- data %>%
      mutate_all(~ replace(., is.na(.), "NULL"))
    
    d <- 1
    while (d < nrow(data) + 1) {
      
      ## Error Checking ----
      ## message for data log for species in VGS check
      if (length(grep(paste0("^", toupper(data[d, ][[2]]), "$"), vgs_species_list$PK_Species, value = T)) == 0) print(paste0("Species: ", toupper(data[d, ][[2]]), " not in VGS db for NF belt#", Transect, " - ", file_on))
      ## message for data log for species qualifier length
      if (nchar(data[d, ][3]) > 20) print(paste0("Species: ", toupper(data[d, ][[3]]), " Qualifier is too long (Max 20 char) for belt#", Transect, " - ", file_on))
      
      ## stop app if not in Power Mode ->
      if (power_mode == FALSE) {
        ## alerts for app stoppages
        ## Species not in VGS .db species list = stop()
        if (length(grep(paste0("^", toupper(data[d, ][[2]]), "$"), vgs_species_list$PK_Species, value = T)) == 0) {
          ## message for data log for species in VGS check
          print(paste0("Species: ", toupper(data[d, ][[2]]), " not in VGS db for belt#", Transect, " - ", file_on))
          ## alert for species
          shinyalert("Species Not in VGS!", paste0(
            "Species: ", toupper(data[d, ][[2]]),
            " not in VGS db for belt#", Transect, " - ", file_on
          ),
          type = "error", immediate = T
          )
          Sys.sleep(15)
        }
        
        ## species check
        if (length(grep(paste0("^", toupper(data[d, ][[2]]), "$"), vgs_species_list$PK_Species, value = T)) == 0) stop(paste0("Species: ", toupper(data[d, ][[2]]), " not in VGS db for NF belt#", Transect, " - ", file_on))
        ## Check length of species qualifier = stop() if over 20 char
        ## print message for qualifier error
        if (nchar(data[d, ][3]) > 20) print(paste0("Species: ", toupper(data[d, ][[3]]), " Qualifier is too long (Max 20 char) for belt#", Transect, " - ", file_on))
        ## pop up warning
        if (nchar(data[d, ][3]) > 20) {
          shinyalert("Species Qualifier too long!", paste0(
            "Species: ", toupper(data[d, ][[2]]),
            " Qualifier is too long (>20) for belt#",
            Transect, " - ", file_on
          ),
          type = "error",
          immediate = T
          )
          Sys.sleep(20)
        }
        ## stop app
        if (nchar(data[d, ][3]) > 20) stop(paste0("Species: ", toupper(data[d, ][[3]]), " Qualifier is too long (Max 20 char) for belt#", Transect, " - ", file_on))
      } ## end of if in power mode
      ## End of Error checking ----
      
      ## for each species - format if Species Qualifier is not null
      if (data[d, ][3] != "NULL") {
        ## get rid of problematic symbols
        data[d, ][3] <- gsub('"', "", data[d, ][3], fixed = T)
        data[d, ][3] <- gsub("'", "", data[d, ][3], fixed = T)
        ## add quotes
        data[d, ][3] <- paste0("'", data[d, ][3], "'")
      }
      
      ## each col / sample insert
      PK_Sample <- GUID()
  
      ## element is incremental, so using 'd' in loop but adding leading zeros
      ## only supports 3 digits of entries
      if (nchar(d)==1) {
        li.element <- paste0("00000",d)
      } else if (nchar(d)==2) {
        li.element <- paste0("0000",d)
      } else if (nchar(d)==3) {
        li.element <- paste0("000",d)
      }
      
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
           (", PK_Sample, ",", FK_Event, ",'", toupper(data$Code[d]), "',", trimws(data$Belt[d]), ",1,'", li.element, "',NULL,'", toupper(data$Code[d]), "',", trimws(data$Qualifier[d]), ",", FieldQualifier, "," ,cParameter, ",", cParameter2, "," ,cParameter3, "," ,trimws(round(as.numeric(data$Start[d]),2)), "," ,trimws(round(as.numeric(data$Stop[d]),2)), "," ,trimws(round(as.numeric(data$Gap[d]),2)), "," ,cValue, "," ,cValue2, "," ,cValue3, "," ,SyncKey, "," ,SyncState, ")")

      ## only insert if not in power mode
      if (power_mode == "FALSE") {
        
        ## for troubleshootin
        #print(paste("Inserting NF code",toupper(data[d, ][[2]]),"on Transect",Transect,"- Sample",s,"- Element",Element,"-",SubElement))
        
        ## insert LI data
        dbExecute(mydb, insert_sample)
        
        ## if height present, need another insert statement ->
        if (data$Height[d]>0 && data$Height[d] != "NULL") {
          #print(paste0("Height found for LI, Species: ",toupper(data$Code[d])))
          
          ## each col / sample insert
          PK_Sample.h <- GUID()

          insert_sample.h <- paste0("INSERT INTO Sample
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
           (", PK_Sample.h, ",", FK_Event_height, ",'", toupper(data$Code[d]), "',", trimws(data$Belt[d]), ",1,'", li.element, "',NULL,'", toupper(data$Code[d]), "',", trimws(data$Qualifier[d]), ",", FieldQualifier, "," ,cParameter, ",", cParameter2, "," ,cParameter3, "," ,trimws(round(as.numeric(data$Height[d]),2)), "," ,nValue2, "," ,nValue3, "," ,cValue, "," ,cValue2, "," ,cValue3, "," ,SyncKey, "," ,SyncState, ")")

          ## insert LI-height data
          dbExecute(mydb, insert_sample.h)
        }
        
      } ## end of power mode for inserts
      
      ## move to next row/species
      d <- d + 1
    }
    
    ## get Transect, Sample, Species, Element, nValue, nValue2, nValue3
    
    print("LI data inserted... ")
  }
  
  ## If clipping Production...
  
  ## ...
}
## end of insert data function -------------------------------------------------


## insert statement to create folder schema for site and place site in folder
create_schema <- function(ClassName, CK_ParentClass = "NULL", SitePK, PK_SiteClass = "NULL", FK_Species_SiteClass = "NULL", ClassID = "NULL", Description = "NULL", SyncKey = 33, SyncState = 1) {
  PK_SiteClass <- GUID()
  
  insert_siteClass <- paste0("INSERT INTO siteClass
           (PK_SiteClass,
           PK_SiteClass,
           FK_Species_SiteClass,
           CK_ParentClass,
           ClassID,
           ClassName,
           Description,
           SyncKey,
           SyncState)
     VALUES
           (", PK_SiteClass, ",", PK_SiteClass, ",", FK_Species_SiteClass, ",", CK_ParentClass, ",", ClassID, ",", ClassName, ",", Description, ",", SyncKey, ",", SyncState, ")")
  
  ## insert siteClass
  dbExecute(mydb, insert_siteClass)
  
  ## then insert siteClassLinks
  if (SitePK != "NULL") {
    PK_SiteClassLink <- GUID()
    
    insert_siteClassLink <- paste0("INSERT INTO siteClassLink
           (PK_SiteClassLink,
           FK_Site,
           FK_SiteClass,
           SyncKey,
           SyncState)
     VALUES
           (", PK_SiteClassLink, ",", SitePK, ",", PK_SiteClass, ",", SyncKey, ",", SyncState, ")")
    
    ## insert siteClass
    dbExecute(mydb, insert_siteClassLink)
  }
}


## update schema only
update_schema <- function(SitePK, PK_SiteClass, SyncKey = 33, SyncState = 1) {
  ## then insert siteClassLinks
  if (SitePK != "NULL") {
    PK_SiteClassLink <- GUID()
    
    insert_siteClassLink <- paste0("INSERT INTO siteClassLink
           (PK_SiteClassLink,
           FK_Site,
           FK_SiteClass,
           SyncKey,
           SyncState)
     VALUES
           (", PK_SiteClassLink, ",", SitePK, ",", PK_SiteClass, ",", SyncKey, ",", SyncState, ")")
    
    ## insert siteClass
    dbExecute(mydb, insert_siteClassLink)
  }
}
