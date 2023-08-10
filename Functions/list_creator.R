## Creating list function -- >

## Create function to create GUID for VGS
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Read in libraries -----------------------------------------------------------
library(uuid)
library(tidyverse)
library(readxl)
library(DBI)
library(RSQLite)
library(stringr)

## create_list() function -> works for OT and GC -> Not hierarchical yet...
create_list <- function(listName = "vgs_list", spFilterType = "OT", description = "NULL", definition = "NULL", locked = 0, active = 1, IsHierarchical = 0, SyncKey = 33, SyncState = 4, CK_BestGuess = "NULL", NewSynonym = "NULL", Family = "NULL", Habit = "NULL", Duration = "NULL", Nativity = "NULL", Description_sp = "NULL", CK_ParentSpecies = "NULL", Qualifier = "NULL", SurrogateValue = "NULL", IsDefault = 0, Weight = "NULL") {
  ## adding quotes if data is not NULL for specific variables
  
  ## variable manipulation -----------------------------------------------------
  if (description != "NULL") {
    description <- paste0("'", description, "'")
  }
  if (definition != "NULL") {
    definition <- paste0("'", definition, "'")
  }
  if (CK_BestGuess != "NULL") {
    CK_BestGuess <- paste0("'", CK_BestGuess, "'")
  }
  if (NewSynonym != "NULL") {
    NewSynonym <- paste0("'", NewSynonym, "'")
  }
  if (Family != "NULL") {
    Family <- paste0("'", Family, "'")
  }
  if (Habit != "NULL") {
    Habit <- paste0("'", Habit, "'")
  }
  if (Duration != "NULL") {
    Duration <- paste0("'", Duration, "'")
  }
  if (Nativity != "NULL") {
    Nativity <- paste0("'", Nativity, "'")
  }
  if (Description_sp != "NULL") {
    Description_sp <- paste0("'", Description_sp, "'")
  }
  if (CK_ParentSpecies != "NULL") {
    CK_ParentSpecies <- paste0("'", CK_ParentSpecies, "'")
  }
  if (Qualifier != "NULL") {
    Qualifier <- paste0("'", Qualifier, "'")
  }
  if (SurrogateValue != "NULL") {
    SurrogateValue <- paste0("'", SurrogateValue, "'")
  }
  ## End of variable manipulation ----------------------------------------------
  
  ## ID GUID for Creating List -------------------------------------------------
  Pk_SpList <- GUID(type = "pk")
  ##
  if (spFilterType == "OT") { ## Other List
    FK_SubType <- "X'919499aa3e33e0419852dea440864466'"
    spFilter <- "OT"
  }
  if (spFilterType == "GC") { ## Ground Cover List
    FK_SubType <- "X'ef7c1c387c858346bacc4457813c6ec7'"
    spFilter <- "GC"
  }
  insert_list <- paste0(
    "INSERT INTO SpList(
    PK_SpList,FK_SubType,ListName,Description,SpFilter,Definition,Locked,Active,
    IsHierarchical,SyncKey,SyncState) VALUES(",
    Pk_SpList, ",", FK_SubType, ",'", listName, "',", description, ",'",
    spFilter, "',", definition, ",", locked, ",", active, ",",
    IsHierarchical, ",", SyncKey, ",", SyncState, ")"
  )
  ## insert species into species table
  dbExecute(mydb, insert_list)
  ## End of creating list ------------------------------------------------------
  
  ## select file
  data_file <- choose.files("Choose .csv with 2 cloumns (no labels)", multi = F)
  ## read file
  list_data <- read_csv(data_file, col_names = F, skip_empty_rows = T, show_col_types = FALSE)
  
  ## adjusting Filter and species GUIDs depending on type of list
  if (spFilter == "OT") { ## Other List
    list <- "OT"
    pre_guid <- "OT"
  }
  if (spFilter == "GC") { ## Ground Cover List
    list <- "GC"
    pre_guid <- "G"
  }
  
  ## ID GUID for species insert ------------------------------------------------
  Pk_Species <- GUID(type = "sp", number_of_GUIDS = nrow(list_data))
  Pk_Species <- paste0(pre_guid, Pk_Species)
  
  ## ID GUID for species links insert ------------------------------------------
  Pk_SpListLink <- GUID(type = "pk", number_of_GUIDS = nrow(list_data))
  
  i <- 1
  while (i < nrow(list_data) + 1) {
    query_db <- paste0("Select PK_Species, CommonName, SpeciesName from Species
inner join SpListLink on SpListLink.FK_Species = Species.PK_Species
inner join SpList on SpList.PK_SpList = SpListLink.FK_SpList
where ListName = '", listName, "'")
    
    ## query db to check if exists before inserting
    list_species <- dbGetQuery(mydb, query_db)
    
    species_check <- list_species %>%
      filter(CommonName == list_data[i, ][[1]])
    species_check <- list_species %>%
      filter(SpeciesName == list_data[i, ][[2]])
    
    species_check <- unique(species_check)
    
    ## if species does not exist then insert/continue
    if (nrow(species_check) == 0) {
      ## common name is 1st column, species name is 2nd
      insert_sp <- paste0(
        "INSERT INTO Species(
    PK_Species,CK_BestGuess,CommonName,SpeciesName,List,NewSynonym,Family,Habit,
    Duration,Nativity,Description,SyncKey,SyncState) VALUES('",
        Pk_Species[i], ",", CK_BestGuess, ",'", list_data[i, ][[1]], "','",
        list_data[i, ][[2]], "','", list, "',", NewSynonym, ",", Family, ",",
        Habit, ",", Duration, ",", Nativity, ",", Description_sp, ",", SyncKey,
        ",", SyncState, ")"
      )
      ## insert species into species table
      dbExecute(mydb, insert_sp)
      
      ## update list_data to attack PK_Species
      suppressWarnings(list_data$PK_Species[i] <- substr(Pk_Species[i], 1, nchar(Pk_Species[i]) - 1))
      ## End of species insert -------------------------------------------------
      
      insert_sp_link <- paste0(
        "INSERT INTO spListLink(
    PK_SpListLink, FK_SpList, FK_Species, CK_ParentSpecies, Qualifier,
    SurrogateValue, IsDefault, Weight, SyncKey,SyncState) VALUES(",
        Pk_SpListLink[i], ",", Pk_SpList, ",'", Pk_Species[i], ",", CK_ParentSpecies, ",",
        Qualifier, ",", SurrogateValue, ",", IsDefault, ",", Weight, ",",
        SyncKey, ",", SyncState, ")"
      )
      ## link species to list
      dbExecute(mydb, insert_sp_link)
    }
    
    
    ## if species exists then use it instead of making a new one... use species_check
    if (nrow(species_check) == 1) {
      
      ## update list_data to attach PK_Species
      suppressWarnings(list_data$PK_Species[i] <- substr(species_check$PK_Species[[1]], 1, nchar(species_check$PK_Species[[1]])))
      
      ## End of species insert -------------------------------------------------
      
      #   ## use found PK_Species in VGS instead of creating one
      #   insert_sp_link <- paste0("INSERT INTO spListLink(
      # PK_SpListLink, FK_SpList, FK_Species, CK_ParentSpecies, Qualifier,
      # SurrogateValue, IsDefault, Weight, SyncKey,SyncState) VALUES(",
      #                            Pk_SpListLink[i],",",Pk_SpList,",'",species_check$PK_Species[[1]],"',",CK_ParentSpecies,",",
      #                            Qualifier,",",SurrogateValue,",",IsDefault,",",Weight,",",
      #                            SyncKey,",",SyncState,")")
      #   ## link species to list
      #   dbExecute(mydb, insert_sp_link)
    }
    
    ## go to next row/species in list
    i <- i + 1
  } ## End of species insert links ---------------------------------------------
  
  # Pk_SpList <<- Pk_SpList
  
  # stop("stop here for now to test... before H")
  ## testting
  # list_data<- check
  # listName<- "list4"
  # spFilter<- "OT"
  # SyncKey=33
  # SyncState=4
  # CK_BestGuess="NULL"
  # NewSynonym="NULL"
  # Family="NULL"
  # Habit="NULL"
  # Duration="NULL"
  # Nativity="NULL"
  # Description_sp="NULL"
  # CK_ParentSpecies="NULL"
  # Qualifier="NULL"
  # SurrogateValue="NULL"
  # IsDefault=0
  # Weight="NULL"
  
  # list_data_b<<- list_data
  
  ## if H list
  if (IsHierarchical == 1) {
    
    ## update list IsH to 1 at some point ----
    IsHierarchical <- 0
    ## Need to attach to questions
    ## ---------------------------------------
    
    original_list_length <- ncol(list_data)
    
    m_over <- 2 ## starting to add species from other columns (move_over)
    while (m_over < original_list_length - 1) {
      
      ## adjusting Filter and species GUIDs depending on type of list
      if (spFilter == "OT") { ## Other List
        list <- "OT"
        pre_guid <- "OT"
      }
      if (spFilter == "GC") { ## Ground Cover List
        list <- "GC"
        pre_guid <- "G"
      }
      
      # ## ID GUID for species insert ------------------------------------------
      Pk_Species <- GUID(type = "sp", number_of_GUIDS = nrow(list_data))
      Pk_Species <- paste0(pre_guid, Pk_Species)
      ## adding new species to species table - then linking them to list w/Parent
      i <- 1
      while (i < nrow(list_data) + 1) {
        query_db <- paste0("Select PK_Species, CommonName, SpeciesName from Species
inner join SpListLink on SpListLink.FK_Species = Species.PK_Species
inner join SpList on SpList.PK_SpList = SpListLink.FK_SpList
where ListName = '", listName, "'")
        
        ## query db to check if exists before inserting
        list_species <- dbGetQuery(mydb, query_db)
        
        species_check <- list_species %>%
          filter(CommonName == list_data[i, ][[1 + m_over]])
        species_check <- list_species %>%
          filter(SpeciesName == list_data[i, ][[2 + m_over]])
        
        species_check <- unique(species_check)
        
        ## Only insert if not NA - checking SpeciesName
        if (!is.na(list_data[i, ][[2 + m_over]])) {
          
          ## if species does not exist then insert/continue
          if (nrow(species_check) == 0) {
            ## common name is 1st column, species name is 2nd
            insert_sp <- paste0(
              "INSERT INTO Species(
    PK_Species,CK_BestGuess,CommonName,SpeciesName,List,NewSynonym,Family,Habit,
    Duration,Nativity,Description,SyncKey,SyncState) VALUES('",
              Pk_Species[i], ",", CK_BestGuess, ",'", list_data[i, ][[1 + m_over]], "','",
              list_data[i, ][[2 + m_over]], "','", list, "',", NewSynonym, ",", Family, ",",
              Habit, ",", Duration, ",", Nativity, ",", Description_sp, ",", SyncKey,
              ",", SyncState, ")"
            )
            ## insert species into species table
            dbExecute(mydb, insert_sp)
            
            if (m_over == 2) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_2[i] <- substr(Pk_Species[i], 1, nchar(Pk_Species[i]) - 1))
            }
            if (m_over == 4) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_4[i] <- substr(Pk_Species[i], 1, nchar(Pk_Species[i]) - 1))
            }
            if (m_over == 6) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_6[i] <- substr(Pk_Species[i], 1, nchar(Pk_Species[i]) - 1))
            }
            if (m_over == 8) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_8[i] <- substr(Pk_Species[i], 1, nchar(Pk_Species[i]) - 1))
            }
            if (m_over == 10) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_10[i] <- substr(Pk_Species[i], 1, nchar(Pk_Species[i]) - 1))
            }
            if (m_over == 12) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_12[i] <- substr(Pk_Species[i], 1, nchar(Pk_Species[i]) - 1))
            }
            ## End of species insert ---------------------------------------------
            
            ## ID GUID for species links insert ----------------------------------
            Pk_SpListLink <- GUID(type = "pk", number_of_GUIDS = nrow(list_data))
            
            ## always looks at the last column (pk gets added each time loop happens)
            CK_ParentSpecies <- paste0("'", list_data[i, ][ncol(list_data) - 1], "'")
            
            insert_sp_link <- paste0(
              "INSERT INTO spListLink(
    PK_SpListLink, FK_SpList, FK_Species, CK_ParentSpecies, Qualifier,
    SurrogateValue, IsDefault, Weight, SyncKey,SyncState) VALUES(",
              Pk_SpListLink[i], ",", Pk_SpList, ",'", Pk_Species[i], ",", CK_ParentSpecies, ",",
              Qualifier, ",", SurrogateValue, ",", IsDefault, ",", Weight, ",",
              SyncKey, ",", SyncState, ")"
            )
            ## link species to list
            dbExecute(mydb, insert_sp_link)
          }
          
          if (nrow(species_check) > 1) stop(paste0("Found duplicate species '", species_check[[3]], "' - '", species_check[[2]], "' not sure which one to use : Col ", m_over))
          
          ## if species exists then use it instead of making a new one... use species_check
          if (nrow(species_check) == 1) {
            
            ## update list_data to attack PK_Species
            # suppressWarnings(list_data$PK_Species[i] <- substr(species_check$PK_Species[[1]],1,nchar(species_check$PK_Species[[1]])))
            
            if (m_over == 2) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_2[i] <- substr(species_check$PK_Species[[1]], 1, nchar(species_check$PK_Species[[1]])))
            }
            if (m_over == 4) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_4[i] <- substr(species_check$PK_Species[[1]], 1, nchar(species_check$PK_Species[[1]])))
            }
            if (m_over == 6) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_6[i] <- substr(species_check$PK_Species[[1]], 1, nchar(species_check$PK_Species[[1]])))
            }
            if (m_over == 8) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_8[i] <- substr(species_check$PK_Species[[1]], 1, nchar(species_check$PK_Species[[1]])))
            }
            if (m_over == 10) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_10[i] <- substr(species_check$PK_Species[[1]], 1, nchar(species_check$PK_Species[[1]])))
            }
            if (m_over == 12) {
              ## update list_data to attack PK_Species
              suppressWarnings(list_data$PK_Species_12[i] <- substr(species_check$PK_Species[[1]], 1, nchar(species_check$PK_Species[[1]])))
            }
            ## End of species insert ---------------------------------------------
            
            # ## always looks at the last column (pk gets added each time loop happens)
            # CK_ParentSpecies <- paste0("'",list_data[i,][ncol(list_data)-1],"'")
            
            #         insert_sp_link <- paste0("INSERT INTO spListLink(
            # PK_SpListLink, FK_SpList, FK_Species, CK_ParentSpecies, Qualifier,
            # SurrogateValue, IsDefault, Weight, SyncKey,SyncState) VALUES(",
            #                                  Pk_SpListLink[i],",",Pk_SpList,",'",species_check$PK_Species[[1]],"',",CK_ParentSpecies,",",
            #                                  Qualifier,",",SurrogateValue,",",IsDefault,",",Weight,",",
            #                                  SyncKey,",",SyncState,")")
            #         ## link species to list
            #         dbExecute(mydb, insert_sp_link)
          }
        }
        
        ## if NA - still need to fill in value for table
        if (is.na(list_data[i, ][[2 + m_over]])) {
          if (m_over == 2) {
            ## update list_data to attack PK_Species
            suppressWarnings(list_data$PK_Species_2[i] <- NA)
          }
          if (m_over == 4) {
            ## update list_data to attack PK_Species
            suppressWarnings(list_data$PK_Species_4[i] <- NA)
          }
          if (m_over == 6) {
            ## update list_data to attack PK_Species
            suppressWarnings(list_data$PK_Species_6[i] <- NA)
          }
          if (m_over == 8) {
            ## update list_data to attack PK_Species
            suppressWarnings(list_data$PK_Species_8[i] <- NA)
          }
          if (m_over == 10) {
            ## update list_data to attack PK_Species
            suppressWarnings(list_data$PK_Species_10[i] <- NA)
          }
          if (m_over == 12) {
            ## update list_data to attack PK_Species
            suppressWarnings(list_data$PK_Species_12[i] <- NA)
          }
        }
        
        ## moving on to next row/species in list
        i <- i + 1
      } ## End of species insert links -----------------------------------------
      ## moving to next set columns / parent-children species
      m_over <- m_over + 2
      
      # list_data<<- list_data
    }
  }
}
## End of List creation --------------------------------------------------------
