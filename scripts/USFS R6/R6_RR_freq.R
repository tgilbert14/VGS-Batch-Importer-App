## Processing Nested Freq Data (GC on cover page for R6 RR batch)

print(paste0("Parsing Data for Nested Freq..."))
## selecting only data with no column names (-1 to get rid of sum column)
nf_data <- data_import[c(3:nrow(data_import)), c(1:ncol(data_import)-1)]
## get rid of NA columns with no species listed
nf_data <- nf_data %>%
  filter(!is.na(nf_data[1]))

trim_sheet_name <- trimws(active_sheets[x])
belt_num <- as.numeric(substr(trim_sheet_name, nchar(trim_sheet_name), nchar(trim_sheet_name)))

if (nrow(nf_data) == 0) {
  print(paste0("No Nested Freq for this Belt ", belt_num))
}

## only insert if data present
if (nrow(nf_data) > 0) {
  ## save as data frame
  nf_data <- as.data.frame(nf_data)
  
  ## trim white spaces
  nf_data<- nf_data %>%
    mutate_if(is.character, str_trim)
  nf_data<- nf_data %>%
    mutate_if(is.numeric, str_trim)
  
  ## if is NA will make NULL
  nf_data[, 2][is.na(nf_data[, 2])] <- "NULL"
  
  # ## zeros need to be NA's first
  # nf_data[nf_data == trimws(0)] <- NA
  # View(nf_data)
  
  ## redoing sum columns becuase some errors from user input
  ## looking at totals to filter out no data and get sum
  add_sum_col <- nf_data[5:(ncol(nf_data))]
  cols.num <- c(
    "...5", "...6", "...7", "...8", "...9", "...10", "...11",
    "...12", "...13", "...14", "...15", "...16", "...17", "...18",
    "...19", "...20", "...21", "...22", "...23", "...24"
  )
  ## convert to numeric to get sum
  add_sum_col[cols.num] <- sapply(add_sum_col[cols.num], as.numeric)
  
  Sum_of_nesteds <- rowSums(x = add_sum_col, na.rm = T)
  nf_data <- cbind(nf_data, Sum_of_nesteds)
  
  nf_data <- nf_data %>%
    filter(Sum_of_nesteds > 0)
  
  ## then change all 4's to 0's -> 0 means it is in the largest frame
  nf_data[nf_data == trimws(4)] <- 0
  
  ## making all codes upper case
  nf_data[, 1] <- toupper(nf_data[, 1])
  
  ## then change all 4's to 0's -> 0 means it is in the largest frame
  nf_data[nf_data == trimws(4)] <- 0
  
  ## all columns except summary
  nest_freq_ready <- nf_data[,c(1:ncol(nf_data))]
  
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