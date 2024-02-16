db_loc <<- "C:/ProgramData/VGSData/VGS50.db"
mydb <<- dbConnect(RSQLite::SQLite(), dbname = db_loc)

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
  
  ## data validation check function
  data_quality_data_frame(data)
  
  ## last column is a summary column
  sample_data <- data[5:(ncol(data) - 1)]
  
  d <- 1
  while (d < nrow(data) + 1) {
    
    ## Error Checking ----
    ## message for data log for species in VGS check
    if (length(grep(toupper(data[d, ][[1]]), vgs_species_list$PK_Species, value = T)) == 0) print(paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for NF belt#", Transect," - ",file_on))
    ## message for data log for species qualifier length
    if (nchar(data[d, ][2]) > 20) print(paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (Max 20 char) for belt#", Transect," - ",file_on))
    
    ## stop app if not in Power Mode ->
    if (power_mode == FALSE) {
      ## alerts for app stoppages
      ## Species not in VGS .db species list = stop()
      if (length(grep(toupper(data[d, ][[1]]), vgs_species_list$PK_Species, value = T)) == 0) {
        ## message for data log for species in VGS check
        print(paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for belt#", Transect," - ",file_on))
        ## alert for species
        shinyalert("Species Not in VGS!", paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for belt#", Transect," - ",file_on), type = "error")
        Sys.sleep(15)
      }
      
      if (length(grep(toupper(data[d, ][[1]]), vgs_species_list$PK_Species, value = T)) == 0) stop(paste0("Species: ", toupper(data[d, ][[1]]), " not in VGS db for NF belt#", Transect," - ",file_on))
      ## Check length of species qualifier = stop() if over 20 char
      ## print message for qualifier error
      if (nchar(data[d, ][2]) > 20) print(paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (Max 20 char) for belt#", Transect," - ",file_on))
      ## pop up warning
      if (nchar(data[d, ][2]) > 20) {
        shinyalert("Species Qualifier too long!", paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (>20) for belt#", Transect," - ",file_on), type = "error")
        Sys.sleep(20)
      }
      ## stop app
      if (nchar(data[d, ][2]) > 20) stop(paste0("Species: ", toupper(data[d, ][[1]]), " Qualifier is too long (Max 20 char) for belt#", Transect," - ",file_on))
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
      Element<- as.numeric(str_trim(sample_data[d, s]))
      
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
           (", PK_Sample, ",", FK_Event, ",'", toupper(data[d, ][[1]]), "',", Transect, ",", s, ",", Element, ",", SubElement, ",'", toupper(data[d, ][[1]]), "',", data[d, ][2], ",", FieldQualifier, ",", cParameter, ",", cParameter2, ",", cParameter3, ",1,", nValue2, ",", nValue2, ",", cValue, ",", cValue2, ",", cValue3, ",", SyncKey, ",", SyncState, ")")
      
      if (!is.na(sample_data[d, s])) {
        ## insert NF data
        dbExecute(mydb, insert_sample)
      }
      ## move to next sample
      s <- s + 1
    }
    
    ## move to next row/species
    d <- d + 1
  }
  
  print("Nested Freq inserted...")
}

View(data)

suppressWarnings(DBI::dbDisconnect(mydb))
suppressWarnings(closeAllConnections())