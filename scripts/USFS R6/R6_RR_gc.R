## GC tally data USFS R6 - RR

## searching for specific term for specific protocol
term <- "Tally"

## Set FK_Event - Query .db for correct PK_Event ->
## works...
find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
    INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
    INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
    INNER JOIN Site ON Site.PK_Site = Event.FK_Site
    where ProtocolName LIKE'%",term,"%' AND Protocol.Date = '",event_date, "'
    AND SiteID = '", site_name, "'", "
    AND EventName = 'Point Ground Cover (by tally)'")

Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
checked_PK_Event <- Event_guid_info$`quote(PK_Event)`[1]

## GC tally data - done on meta-data page
print(paste0("Parsing Data for GC Tally Data..."))

## saving gc data only with no column titles - RR specific
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
  insert_data(data = gc_ready, method = "GC", FK_Event = checked_PK_Event, SyncKey = 33, SyncState = 1)
}