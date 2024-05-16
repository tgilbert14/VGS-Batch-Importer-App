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
checked_PK_Event_gc <- Event_guid_info$`quote(PK_Event)`[1]

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
               Where PK_Protocol=", checked_PK_Event_gc)
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
  
  
  ## checking if whole number or decimal (hits vs % for gc)
  whole_num_check<- unique(as.numeric(temp_gc[,2]) == round(as.numeric(temp_gc[,2]),0))
  whole_num_check_confirm<- unique(grep("FALSE", whole_num_check, value = T))

  ## everything in this if is only done if we think the gc is % rather than total count #
  ## if this is FALSE this means decimals percent, so may be % cover not hits - or if it is exactly 100
  if ((whole_num_check_confirm == "FALSE" && length(whole_num_check_confirm)!= 0) || sum(as.numeric(temp_gc[,2]))==100) {
    print("GC % detected instead of hits")
    ## add decimals up - should be 100% - make col numeric
    col_as_num<- sapply(temp_gc[2], as.numeric)
    sum_per_gc<- colSums(col_as_num)
    ## if not totaled to 100% give message
    if (sum_per_gc != 100) {
      print(paste0(
        "GC % sums to ",sum_per_gc,", needs to total to 100% for ",file_on))
      
      shinyalert("GC % detected", paste0(
        "GC % sums to ",sum_per_gc,", needs to total to 100% for ",file_on),
        type = "error", immediate = T)
      ## if in power mode just power through to get qa/qc report
    }
    
    ## assuming is 100% at this point - processing hit #'s based on percent
    ## set default total number here based on protocol/client
    Belt_predicted_gc_hits<- 80
    
    rounded_hits<- round(col_as_num*(Belt_predicted_gc_hits/100),0)
    
    ## after rounding if over 80 per transect, minus 1 (least effect on largest value)
    ## if greater than 1 difference, need to look at file by file for errors
    
    if (sum(rounded_hits) > 80) {
      max_value<- grep(pattern = max(rounded_hits), x = rounded_hits)
      rounded_hits[max_value][1] <- rounded_hits[max_value][1]-1
    }
    
    ## same for 1 value under 80, but add 1 to max
    if (sum(rounded_hits) < 80) {
      max_value<- grep(pattern = max(rounded_hits), x = rounded_hits)
      rounded_hits[max_value][1] <- rounded_hits[max_value][1]+1
    }
    
    ## replace with rounded hit numbers per transect
    temp_gc$...5<- rounded_hits
  }
  ## end of converting % values to estimated count #'s
  
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
  print(paste0(length(hi2)," ground cover points found - ",site_name))
  ## each belt should have 80 ground cover points
  tot_num_belts<- length(hi2)/80
  ## checking if belt number is whole number
  is_whole_number<- tot_num_belts%%1==0
  
  if (!is_whole_number) {
    print(paste0("Warning, GC adds up to ",length(hi2)," - double check ",site_name))
    ## rounding up belt
    tot_num_belts<- ceiling(tot_num_belts)
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

  ## if only 1 belt
  if (tot_num_belts == 1) {
    belt_numbers <- first_belt
  } else {
    belt_numbers <- next_belt
  }
  
  SampleNumber_raw <- first_SampleNumber_raw
  ## setting other variables -->
  ## 4 per Sample - 80 per transect
  Element_raw <- rep(c(1:4),20*tot_num_belts)
  
  ## only insert if data present
  if (length(hi2) > 0) {
  ## insert gc data
  insert_data(data = hi2, method = "GC", Transect = belt_numbers, 
              SampleNumber = SampleNumber_raw, Element = Element_raw, 
              FK_Event = checked_PK_Event_gc, SyncKey = 33, SyncState = 1)
  }
}
