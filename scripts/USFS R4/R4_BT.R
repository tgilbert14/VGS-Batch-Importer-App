## for LPI tab
if (length(grep("LPI", active_sheets[x]))==1) {
  ## LPI data insert -----------------------------------------------------------
  print(paste0("Parsing Data for LPI..."))
  
  ## searching for specific term for specific protocol
  term <- "BTNF Range Monitoring"
  
  ## Set FK_Event - Query .db for correct PK_Event
  ## complex for no reason - ehhhhhh
  find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
INNER JOIN Site ON Site.PK_Site = Event.FK_Site
where ProtocolName LIKE '%",term,"%' AND Protocol.Date = '",event_date, "'
AND SiteID = '", site_name, "'", " AND
EventName = 'Point Intercept'")
  
  Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
  checked_PK_Event <- Event_guid_info$`quote(PK_Event)`[1]
  
  ## getting pk_species info from LPI lists being used
  LPI_query<- paste0("select DISTINCT species.* from spList
inner join splistlink on splistlink.FK_SpList = splist.PK_SpList
inner join species on species.PK_Species = splistlink.FK_Species
where PK_SpList = X'D25E4ABD60BCAF46BD9BCAEA6F588634'
or PK_SpList = X'6FEFA193D85DD34D87420EB29542C77C'
or PK_SpList = X'8A75470901B59842957B100987C214F7'")
  ## query to VGS
  LPI_list_info <- DBI::dbGetQuery(mydb, LPI_query)
  
  LPI_list<- LPI_list_info %>% 
    select(PK_Species, CommonName, SpeciesName)
  
  ## formatting data
  ## find relevant column first
  search_col<- grep("Pt.",historical_raw_data)
  ## search that column for start of data
  find_data_start<- grep("Pt.",historical_raw_data[[search_col]])
  data_raw<- historical_raw_data[find_data_start:nrow(historical_raw_data),]
  
  lpi_data_0<- data_raw %>% 
    filter(!is.na(data_raw[1]))
  ## lpi data only
  lpi_data_1<- lpi_data_0[1:7]
  lpi_data_2<- lpi_data_1[3:nrow(lpi_data_1),]
  lpi_data<- lpi_data_2 %>% 
    filter(!is.na(...2))
  ## needs code or something to exist
  lpi_data<- lpi_data_2 %>% 
    filter(!is.na(...2))

  ## for each line get info
  fill_me<- data.frame(Transect=character(),Sample=numeric(),Species=character(),SubElement=numeric())
  ## place holder for new table data
  new_position=1
  
  ## go through each row
  me=1
  while (me < nrow(lpi_data)+1) {
    ## looking at one row at a time
    data_r<- lpi_data[me,]
    data<- data_r[!is.na(data_r)]
    ## upper case
    data<- toupper(data)
    ## get rid of 'NONE' for top layer - don't need to store data
    data<- data[data != "NONE"]
    
    ## transect for this row of data
    if (me > 0 && me < 101) {
      transect <- 1
    }
    if (me > 100 && me < 201) {
      transect <- 2
    }
    if (me > 200 && me < 301) {
      transect <- 3
    }
    if (me > 300 && me < 401) {
      transect <- 4
    }
    if (me > 400 && me < 501) {
      transect <- 5
    }
    ## stop if too much data
    if (me > 500) {
      stop("Are there more than 5 transects of data?? - Help!")
    }
    ## sample # for this row of data
    sample<- as.numeric(data[[1]])
    
    ## start with first species (ignoring last entry - basal/gc hit)
    each_sp=2

    while (each_sp < length(data)) {
      ## update data table for insert (row for each insert)
      fill_me[new_position,1]<- transect
      fill_me[new_position,2]<- sample
      fill_me[new_position,3]<- data[each_sp]
      fill_me[new_position,4]<- (each_sp-1)
      ## move on to next one
      each_sp=each_sp+1
      new_position=new_position+1
    }
    ## set basal/gc entry after all species done
    fill_me[new_position,1]<- transect
    fill_me[new_position,2]<- sample
    fill_me[new_position,3]<- data[each_sp]
    fill_me[new_position,4]<- (each_sp-1)
    new_position=new_position+1
    
    ## moving to next row of data
    me=me+1
  }
  temp_lpi<- fill_me
  #View(fill_me)
  
  w=1
  while(w < nrow(LPI_list)+1) {
    ## updating names to GUIDS (PK)
    temp_lpi$Species[temp_lpi$Species == paste0(LPI_list$CommonName[w])] <- paste0(LPI_list$PK_Species[w])
    w=w+1
  }
  
  insert_data(data = temp_lpi, method = "LPI", FK_Event = checked_PK_Event, SyncKey = 33, SyncState = 1)
}

## for Line Intercept tab
if (length(grep("Line Intercept", active_sheets[x]))==1) {
  ## Line Intercept data insert ------------------------------------------------
  print(paste0("Parsing Data for Line Intercept..."))
  
  #View(historical_raw_data)
  #historical_raw_data
  ## look for species label
  #find_label(search_term = "Symbol",data = historical_raw_data, location="below")

#   find_sp_data_col<- grep("Symbol",historical_raw_data)
#   raw_data<- historical_raw_data[,find_sp_data_col]
#   ## start of freq data
#   start_freq<- grep("Symbol",raw_data[[1]])
#   ## start of ground cover data
#   start_gc<- grep("Ground Cover",raw_data[[1]])
#   
#   freq_data_raw<- historical_raw_data[c((start_freq+2):start_gc-1),c(find_sp_data_col:ncol(historical_raw_data))]
#   
#   nf_data<- freq_data_raw %>% 
#     filter(!is.na(freq_data_raw[1]))
#   
#   trim_sheet_name<- trimws(active_sheets[x])
#   #(substr(active_sheets[x], nchar(active_sheets[x]), nchar(active_sheets[x])))
#   belt_num <- as.numeric(substr(trim_sheet_name, nchar(trim_sheet_name), nchar(trim_sheet_name)))
#   
#   if (nrow(nf_data) == 0) {
#     print(paste0("No Nested Freq for this Belt ", belt_num))
#   }
#   #View(nf_data)
#   
#   ## only insert if data present
#   if (nrow(nf_data) > 0) {
#     ## save as data frame
#     nf_data <- as.data.frame(nf_data)
#     ## row 2 = Qualifiers(SpeciesQ/FieldQ) -> NA should be NULL
#     nf_data[, 2][is.na(nf_data[, 2])] <- "NULL"
#     ## change all 0's to NA (specific to this data)
#     nf_data[nf_data == 0] <- NA
#     ## then change all 4's to 0's -> 0 means it is in the largest frame
#     nf_data[nf_data == 4] <- 0
#     
#     ## not doing this...
#     ## get rid of rows if have a species but has no hits (sum of zero for freq)
#     ##nest_freq_ready <- nf_data %>% filter(nf_data$...25 != 0)
#     #View(nest_freq_ready)
#     
#     ## inserting columns for Qualifier/species/common name
#     nf_data_and_q<- add_column(nf_data, qualifier = "NULL", .after = 1)
#     ## adding blank common names
#     nf_data_and_common<- add_column(nf_data_and_q, commonName = "", .after = 3)
#     
#     ## go through each row and update species name and common name
#     move<- 1
#     while (move < nrow(nf_data_and_common)+1) {
#       ## reset names for if statments
#       new_sp_name<- NA
#       new_common_name<- NA
#       new_code_name<- NA
#       
#       ## find row based on USDA code
#       find<- grep(paste0("^",nf_data_and_common[[1]][move],"$"),vgs_species_list_more$PK_Species)
#       
#       ## if could not find by code, try search by species column
#       if (length(find) == 0) {
#         find<- grep(paste0("^",nf_data_and_common[[3]][move],"$"),vgs_species_list_more$SpeciesName)
#         ## if still nothing... check if species column in wrong column
#         if (length(find) == 0) {
#           find<- grep(paste0("^",nf_data_and_common[[1]][move],"$"),vgs_species_list_more$SpeciesName)
#           ## if true then need special table update
#           if (length(find) > 0) {
#             ## updated names from VGS .db
#             new_sp_name<- vgs_species_list_more[find,]$SpeciesName
#             new_common_name<- vgs_species_list_more[find,]$CommonName
#             new_code_name<- vgs_species_list_more[find,]$PK_Species
#           }
#           ## if still nothing - does not exist
#           if (length(find) == 0) {
#             ## stop/error/alert if species not found
#             print(paste0(nf_data_and_common[[1]][move]," - ",nf_data_and_common[[3]][move]," not found in VGS!"))
#             new_sp_name<- "No species name found"
#             new_common_name<- "No common name found"
#           }
#         } else {
#           ## updated names from VGS .db
#           new_sp_name<- vgs_species_list_more[find,]$SpeciesName
#           new_common_name<- vgs_species_list_more[find,]$CommonName
#         }
#         
#       }
#       ## if is na -> use original 'find' for code search
#       if (is.na(new_sp_name)  || length(is.na(new_sp_name))==0) {
#         ## updated names from VGS .db
#         new_sp_name<- vgs_species_list_more[find,]$SpeciesName
#         new_common_name<- vgs_species_list_more[find,]$CommonName
#       }
#       ## check if code needs update
#       if (!is.na(new_code_name)) {
#         nf_data_and_common[[1]][move] <- new_code_name
#       }
#       
#       ## update data table with them
#       ## species name / common name column replace
#       nf_data_and_common[[3]][move] <- new_sp_name
#       nf_data_and_common[[4]][move] <- new_common_name
#       
#       ## move through each species
#       move=move+1
#     }
#     
#     nest_freq_ready<- nf_data_and_common
#     #View(nest_freq_ready)
#     
#     ## searching for specific term for specific protocol
#     term <- "BTNF Range Monitoring"
#     
#     ## Set FK_Event - Query .db for correct PK_Event
#     ## complex for no reason - ehhhhhh
#     find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
# INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
# INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
# INNER JOIN Site ON Site.PK_Site = Event.FK_Site
# where ProtocolName LIKE '%",term,"%' AND Protocol.Date = '",event_date, "'
# AND SiteID = '", site_name, "'", " AND
# EventName = 'Frequency (by quadrat)'")
#     
#     Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
#     checked_PK_Event <- Event_guid_info$`quote(PK_Event)`[1]
#     
#     ## insert nested freq data
#     insert_data(data = nest_freq_ready, method = "NF", FK_Event = checked_PK_Event, Transect = belt_num, SyncKey = 33, SyncState = 1)
#   }
#   
#   
#   
#   
#   
  print("LI not compatible.. not inserted.")
}

## for ground cover and nested frequency tab
if (length(grep("Nested Frequency", active_sheets[x]))==1) {
  ## Freq data insert ----------------------------------------------------------
  print(paste0("Parsing Data for Nested Freq..."))
  
  ## look for species label
  #find_label(search_term = "Symbol",data = historical_raw_data, location="below")
  
  find_sp_data_col<- grep("Symbol",historical_raw_data)
  raw_data<- historical_raw_data[,find_sp_data_col]
  ## start of freq data
  start_freq<- grep("Symbol",raw_data[[1]])
  ## start of ground cover data
  start_gc<- grep("Ground Cover",raw_data[[1]])
  
  freq_data_raw<- historical_raw_data[c((start_freq+2):start_gc-1),c(find_sp_data_col:ncol(historical_raw_data))]

  nf_data<- freq_data_raw %>% 
    filter(!is.na(freq_data_raw[1]))
  
  if (data_quality_data_frame(nf_data) != TRUE) {
    stop("data is not a data frame...")
  }
  ## weird "<NA>" values?
  #nf_data[] <- lapply(nf_data, function(x) sub("\\b<NA>\\b", NA, x))
  
  trim_sheet_name<- trimws(active_sheets[x])
  #(substr(active_sheets[x], nchar(active_sheets[x]), nchar(active_sheets[x])))
  belt_num <- as.numeric(substr(trim_sheet_name, nchar(trim_sheet_name), nchar(trim_sheet_name)))
  
  if (nrow(nf_data) == 0) {
    print(paste0("No Nested Freq for this Belt ", belt_num))
  }
  #View(nf_data)

  ## only insert if data present
  if (nrow(nf_data) > 0) {
    ## save as data frame
    nf_data <- as.data.frame(nf_data)
    ## row 2 = Qualifiers(SpeciesQ/FieldQ) -> NA should be NULL
    #nf_data[, 2][is.na(nf_data[, 2])] <- "NULL"
    ## change all 0's to NA (specific to this data)
    nf_data[nf_data == 0] <- NA
    ## then change all 4's to 0's -> 0 means it is in the largest frame
    nf_data[nf_data == 4] <- 0
    
    ## not doing this...
    ## get rid of rows if have a species but has no hits (sum of zero for freq)
    ##nest_freq_ready <- nf_data %>% filter(nf_data$...25 != 0)
 
    ## making all codes upper case
    nf_data[,1]<- toupper(nf_data[,1])
    
    ## inserting columns for Qualifier/species/common name
    nf_data_and_q<- add_column(nf_data, qualifier = "NULL", .after = 1)
    ## adding blank common names
    nf_data_and_common<- add_column(nf_data_and_q, commonName = "", .after = 3)
    
    ## go through each row and update species name and common name
    move<- 1
    while (move < nrow(nf_data_and_common)+1) {
      ## reset names for if statements
      new_sp_name<- NA
      new_common_name<- NA
      new_code_name<- NA
      
      ## find row based on USDA code
      find<- grep(paste0("^",nf_data_and_common[[1]][move],"$"),vgs_species_list_more$PK_Species)
      
      ## if could not find by code, try search by species column
      if (length(find) == 0) {
        find<- grep(paste0("^",nf_data_and_common[[3]][move],"$"),vgs_species_list_more$SpeciesName)
        ## if still nothing... check if species column in wrong column
        if (length(find) == 0) {
          find<- grep(paste0("^",nf_data_and_common[[1]][move],"$"),vgs_species_list_more$SpeciesName)
          ## if true then need special table update
          if (length(find) > 0) {
            ## updated names from VGS .db
            new_sp_name<- vgs_species_list_more[find,]$SpeciesName
            new_common_name<- vgs_species_list_more[find,]$CommonName
            new_code_name<- vgs_species_list_more[find,]$PK_Species
          }
          ## if still nothing - does not exist
          if (length(find) == 0) {
            ## stop/error/alert if species not found
            print(paste0(nf_data_and_common[[1]][move]," - ",nf_data_and_common[[3]][move]," not found in VGS!"))
            new_sp_name<- "No species name found"
            new_common_name<- "No common name found"
          }
        } else {
          ## updated names from VGS .db
          new_sp_name<- vgs_species_list_more[find,]$SpeciesName
          new_common_name<- vgs_species_list_more[find,]$CommonName
          }
        
      }
      ## if is na -> use original 'find' for code search
      if (is.na(new_sp_name)  || length(is.na(new_sp_name))==0) {
        ## updated names from VGS .db
        new_sp_name<- vgs_species_list_more[find,]$SpeciesName
        new_common_name<- vgs_species_list_more[find,]$CommonName
      }
      ## check if code needs update
      if (!is.na(new_code_name)) {
        nf_data_and_common[[1]][move] <- new_code_name
      }
      
      ## update data table with them
      ## species name / common name column replace
      nf_data_and_common[[3]][move] <- new_sp_name
      nf_data_and_common[[4]][move] <- new_common_name
      
      ## move through each species
      move=move+1
    }
    
    nest_freq_ready<- nf_data_and_common
    #View(nest_freq_ready)

    ## searching for specific term for specific protocol
    term <- "BTNF Range Monitoring"
    
    ## Set FK_Event - Query .db for correct PK_Event
    ## complex for no reason - ehhhhhh
    find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
INNER JOIN Site ON Site.PK_Site = Event.FK_Site
where ProtocolName LIKE '%",term,"%' AND Protocol.Date = '",event_date, "'
AND SiteID = '", site_name, "'", " AND
EventName = 'Frequency (by quadrat)'")
    
    Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
    checked_PK_Event <- Event_guid_info$`quote(PK_Event)`[1]
    
    ## insert nested freq data
    insert_data(data = nest_freq_ready, method = "NF", FK_Event = checked_PK_Event, Transect = belt_num, SyncKey = 33, SyncState = 1)
  }
  
  ## GC tally data USFS R4 -----------------------------------------------------
  
  print(paste0("Parsing Data for GC Data..."))
  
  gc_data<- historical_raw_data[c(start_gc:nrow(historical_raw_data)),
                                c(find_sp_data_col:ncol(historical_raw_data))]
  
  #View(gc_data)
  
  ## only 1 row of data per transect
  gc_d<- gc_data %>% 
    select(...2,...4)
  ## getting rid of NA's
  temp_gc<- gc_d %>% 
    mutate(number = ifelse(is.na(gc_d$...4), 0, gc_d$...4))
  temp_gc<- temp_gc %>% 
    select(...2, number)
  
  ## updating names to GUIDS (PK)
  temp_gc$...2[temp_gc$...2 == "Vegetation"] <- "G_$QH0J18RPQ5"
  temp_gc$...2[temp_gc$...2 == "Bare Soil"] <- "G_$BFZ5XCBCA2"
  temp_gc$...2[temp_gc$...2 == "Litter"] <- "G_LITTDUFF"
  temp_gc$...2[temp_gc$...2 == "Rock"] <- "G_ROCK3"
  temp_gc$...2[temp_gc$...2 == "Pavement"] <- "G_$QGJSWR6KN5"
  temp_gc$...2[temp_gc$...2 == "Cryptogams (moss lichen)"] <- "G_CRYP"
  
  w=1
  ## create a list of all the species entries
  ## initial list for 1st category
  hi<- rep(temp_gc[w,1], temp_gc[w,2])
  
  while (w < nrow(temp_gc)+1) {
    ## append for second time around
    if (w == 2) {
      hi2<- rep(temp_gc[w,1], temp_gc[w,2])
      hi2<- append(hi, hi2)
    }
    ## append for the rest
    if (w > 2) {
      hi<- rep(temp_gc[w,1], temp_gc[w,2])
      hi2<- append(hi, hi2)
    }
    w=w+1
  }
  ## for data log
  print(paste0(length(hi2)," ground cover points found..."))
  
  ## searching for specific term for specific protocol
  ## same one as before...
  term <- "BTNF Range Monitoring"
  
  ## Set FK_Event - Query .db for correct PK_Event ->
  ## works...
  find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
    INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
    INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
    INNER JOIN Site ON Site.PK_Site = Event.FK_Site
    where ProtocolName LIKE'%",term,"%' AND Protocol.Date = '",event_date, "'
    AND SiteID = '", site_name, "'", "
    AND EventName = 'Point Ground Cover'")
  
  Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
  checked_PK_Event <- Event_guid_info$`quote(PK_Event)`[1]
  
  ## if no gc data
  if (length(hi2) == 0) {
    print(paste0("No GC data for file ", batch_file))
    ## update event notes to remark no gc data for this event
    update_event_notes <- paste0("Update Protocol
               Set Notes = 'No Ground Cover data for this event'
               Where PK_Protocol=", checked_PK_Event)
    dbExecute(mydb, update_event_notes)
  }
  
  ## setting variables -->
  ## 20 per Transect
  SampleNumber_raw <- sort.int(rep(c(1:20),4))
  ## 4 per Sample
  Element_raw <- rep(c(1:4),20)
  belt_num_raw<- rep(belt_num, 80)
  ## only insert if data present
  if (length(hi2) > 0) {
    ## insert gc data
    insert_data(data = hi2, method = "GC", SampleNumber = SampleNumber_raw, 
                Element = Element_raw, Transect = belt_num_raw, 
                FK_Event = checked_PK_Event, SyncKey = 33, SyncState = 1)
  }
} ## End of nested freq tabs

print(paste0("Leaving R4_BT.R ", active_sheets[x]))
