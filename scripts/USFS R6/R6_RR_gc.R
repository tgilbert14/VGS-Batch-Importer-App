## GC tally data USFS R6 - RR

## searching for specific term for specific protocol
term <- "Standard"

## Set FK_Event - Query .db for correct PK_Event ->
find_event_guid <- paste0("Select DISTINCT quote(PK_Event), ProtocolName from Protocol
    INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
    INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
    INNER JOIN Site ON Site.PK_Site = Event.FK_Site
    where ProtocolName LIKE'%",term,"%' AND Protocol.Date = '",event_date, "'
    AND SiteID = '", site_name, "'", "
    AND EventName = 'Point Ground Cover'")

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
  temp_gc <- gc_data %>% filter(gc_data$...5 != 0)
  # View(gc_ready)

  ## Update categories to PK_Species/Fk_Species/Field Symbol
  temp_gc$...4[temp_gc$...4 == "Litter"] <- "G_LITT"
  temp_gc$...4[temp_gc$...4 == "Rock"] <- "G_ROCK3"
  temp_gc$...4[temp_gc$...4 == "Vegetation"] <- "G_VEGE"
  temp_gc[temp_gc == "Pavement"] <- "G_$QGJSWR6KN5"
  temp_gc[temp_gc == "Moss"] <- "G_MOSS"
  temp_gc[temp_gc == "Soil"] <- "G_$YKLSYQARFV"
  
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
  ## each belt should have 80 ground cover points
  tot_num_belts<- length(hi2)/80
  ## checking if belt number is whole number
  is_whole_number<- tot_num_belts%%1==0
  
  if (!is_whole_number) {
    stop("GC adds up to ",length(hi2)," - missing or too much GC points")
  }

  belts<- 1
  first_belt<- rep(belts,80)
  first_SampleNumber_raw <- sort.int(rep(c(1:20),4))
  new<- sort.int(rep(c(1:20),4))
  ## go to next belt
  belts=belts+1
  while (belts < tot_num_belts+1) {
    ## for second time
    if (belts == 2) {
      ## belts
      next_belt<- rep(belts,80)
      next_belt<- append(first_belt, next_belt)
      ## sample numbers
      first_SampleNumber_raw <- append(first_SampleNumber_raw,new)
    }
    ## for the rest
    if (belts > 2) {
      another_belt<- rep(belts,80)
      next_belt<- append(next_belt, another_belt)
      first_SampleNumber_raw <- append(first_SampleNumber_raw,new)
    }
    belts=belts+1
  }

  belt_numbers <- next_belt
  SampleNumber_raw <- first_SampleNumber_raw
  ## setting other variables -->
  ## 4 per Sample - 80 per transect
  Element_raw <- rep(c(1:4),20*tot_num_belts)
  
  ## only insert if data present
  if (length(hi2) > 0) {
  ## insert gc data
  insert_data(data = hi2, method = "GC", Transect = belt_numbers, 
              SampleNumber = SampleNumber_raw, Element = Element_raw, 
              FK_Event = checked_PK_Event, SyncKey = 33, SyncState = 1)
  }
}