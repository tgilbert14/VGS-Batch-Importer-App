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
  
  ## searching for specific term for specific protocol
  term <- "Standard"
  
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