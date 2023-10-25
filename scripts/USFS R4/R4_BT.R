
print(paste0("Parsing Data for Nested Freq..."))

## look for species label
find_label(search_term = "Symbol",data = historical_raw_data, location="below")

find_sp_data_col<- grep("Symbol",historical_raw_data)
raw_data<- historical_raw_data[,find_sp_data_col]
## start of freq data
start_freq<- grep("Symbol",raw_data[[1]])
## start of ground cover data
start_gc<- grep("Ground Cover",raw_data[[1]])

freq_data_raw<- historical_raw_data[c((start_freq+2):start_gc-1),c(find_sp_data_col:ncol(historical_raw_data))]

nf_data<- freq_data_raw %>% 
  filter(!is.na(freq_data_raw[1]))

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
  
  ## not doing this...
  ## get rid of rows if have a species but has no hits (sum of zero for freq)
  ##nest_freq_ready <- nf_data %>% filter(nf_data$...25 != 0)
  # View(nest_freq_ready)
  
  ## inserting columns for Qualifier/species/common name
  nf_data_and_q<- add_column(nf_data, qualifier = "NULL", .after = 1)
  ## adding blank common names
  nf_data_and_common<- add_column(nf_data_and_q, commonName = "", .after = 3)
  ## go through each row and update species name and common name
  move<- 1
  while (move < nrow(nf_data_and_common)+1) {
    ## reset names for if statments
    new_sp_name<- NA
    new_common_name<- NA
    
    ## find row based on USDA code
    find<- grep(paste0("^",nf_data_and_common[[1]][move],"$"),vgs_species_list_more$PK_Species)
    
    ## if could not find by code, try search by species
    if (length(find) == 0) {
      find<- grep(paste0("^",nf_data_and_common[[3]][move],"$"),vgs_species_list_more$SpeciesName)
      ## updated names from VGS .db
      new_sp_name<- vgs_species_list_more[find,]$SpeciesName
      new_common_name<- vgs_species_list_more[find,]$CommonName
    }
    ## if is na -> use original 'find' for code search
    if (is.na(new_sp_name)) {
      ## updated names from VGS .db
      new_sp_name<- vgs_species_list_more[find,]$SpeciesName
      new_common_name<- vgs_species_list_more[find,]$CommonName
    }

    ## update data table with them
    ## species name / common name column replace
    nf_data_and_common[[3]][move] <- new_sp_name
    nf_data_and_common[[4]][move] <- new_common_name
    
    ## move through each species
    move=move+1
  }
  
  nest_freq_ready<- nf_data_and_common
  View(nest_freq_ready)
  
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











gc_data<- historical_raw_data[c(start_gc:nrow(historical_raw_data)),c(find_sp_data_col:ncol(historical_raw_data))]




View(freq_data)
View(gc_data)


nested_freq_data<<- data[c(1:find_gc_data-1),] %>%
  filter(!is.na(`Nested Frequency Data`))
## change all NAs to zero??
#View(nested_freq_data)

find_gc_data<<- grep("Ground Cover",data$`Nested Frequency Data`)
gc_data<- data[c(find_gc_data:nrow(data)),] %>%
  filter(!is.na(`Nested Frequency Data`))
