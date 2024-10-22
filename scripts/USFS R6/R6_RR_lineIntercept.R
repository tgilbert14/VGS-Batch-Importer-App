## Processing Nested Freq Data (GC on cover page for R6 RR batch)

print(paste0("Parsing Data for Line Intercept..."))

## cleaning up data
temp_lpi <- data_import %>% 
  filter(!is.na(...2)) %>% 
  filter(!is.na(...6)) %>%
  filter(!is.na(...7)) %>%
  filter(!is.na(...8))

if (nrow(temp_lpi)>1) {
  ## if there is data for line intercept
  ## searching for specific term for specific protocol
  term <- "Standard"
  eventName <- "Line Intercept (feet)"
  
  find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
INNER JOIN Site ON Site.PK_Site = Event.FK_Site
where ProtocolName LIKE '%",term,"%' AND Protocol.Date = '",event_date, "'
AND SiteID = '", site_name, "'", " AND
EventName = '",eventName,"'")
  
  Event_guid_info <- DBI::dbGetQuery(mydb, find_event_guid)
  checked_PK_Event_nf <- Event_guid_info$`quote(PK_Event)`[1]
  
  ## insert nested LI data
  insert_data(data = temp_lpi, method = "LI", FK_Event = checked_PK_Event_nf, SyncKey = 33, SyncState = 1)
  
} else {
  ## this means there is only column names and no labels
  print("No Line Intercept Data found")
}