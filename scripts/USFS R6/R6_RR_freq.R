## Processing Nested Freq Data (GC on cover page for R6 RR batch)

print(paste0("Parsing Data for Nested Freq..."))

## this means is missing initial row w/ info
if (data_import$...1[1] == "Code") {
  print("Excel sheet missing top row...")
  nf_data <- data_import[c(2:nrow(data_import)), c(1:ncol(data_import)-1)]
} else {
  ## selecting only data with no column names (-1 to get rid of sum column)
  nf_data <- data_import[c(3:nrow(data_import)), c(1:ncol(data_import)-1)]
}

## if the first row is not either of these - error in excel sheet
if (data_import$...1[1] != "Code" && data_import$...1[1] != "Enter Species Code") {
  print("This sheet needs to be fixed - top rows not as expected")
  stop("This sheet needs to be fixed - top rows not as expected")
}

## get rid of NA columns with no species listed
nf_data <- nf_data %>%
  filter(!is.na(nf_data[1]))

#View(nf_data)
trim_sheet_name <- trimws(active_sheets[x])

## this has issue when 'a' at the end of number...
belt_num <- as.numeric(substr(trim_sheet_name, nchar(trim_sheet_name), nchar(trim_sheet_name)))

## if there are 'a' belts, need to account for belt # on insert
if (exists("add_to_belt") && !is.na(add_to_belt) && !is.na(belt_num)) {
  belt_num<- as.numeric(belt_num + add_to_belt)
  print(paste0(trim_sheet_name," updated to Belt #",belt_num," for NF insert"))
}

## issues w/ edited excel tabs with letters at the end...
if (class(belt_num) != "numeric" || is.na(belt_num)) {
  ## going by x value (# sheet minus the metadata)
  belt_num<- as.numeric(x-1)
  print(paste0(trim_sheet_name," updated to Belt #",belt_num," for NF insert"))
  ## add var to environment to track belt num movement
  if (exists("add_to_belt") && !is.na(add_to_belt)) {
    add_to_belt = add_to_belt + 1
  } else {
    add_to_belt = 1
  }
}

## this is just looking for species codes present, not #'s
if (nrow(nf_data) == 0) {
  print(paste0("No Nested Freq for this Belt ", belt_num))
}

## 1st check -> only insert if data present
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
  
  ## 2nd check -> this is checking there is also #'s associated w/ sp codes
  if (nrow(nest_freq_ready) > 0) {
    
    ## searching for specific term for specific protocol
    term <- "Standard"
    
    # if (site_name == "Miller Glade #1 (2010-09)" && belt_num == 5) {
    #   stop("stop")
    # }
    
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
    checked_PK_Event_nf <- Event_guid_info$`quote(PK_Event)`[1]
    
    ## insert nested freq data
    insert_data(data = nest_freq_ready, method = "NF", FK_Event = checked_PK_Event_nf, Transect = belt_num, SyncKey = 33, SyncState = 1)
    
    ## need this if first file uploaded has no gc var to initiate - usually
    ## done in R6_RR_gc if gc present
    if (!exists("whole_num_check_confirm")) {
      ## so does not try to insert gc when none exists for 1st file
      whole_num_check_confirm<- TRUE
    }
    
    ## freq data exists... and gc data is % hits instead of actual hits
    ## need to insert proportional gc data again for this transect (Belt 1 inserted already)
    ## added the OR sum is equal to 80 because % entries will be converted to 80 gc hits
    if ((whole_num_check_confirm == "FALSE" && length(whole_num_check_confirm)!= 0) || sum(as.numeric(temp_gc[,2]))==80) {
      if (belt_num != 1) {
        ## insert gc data for other belts when present with updated belt num
        belt_numbers<- rep(belt_num,80)
        insert_data(data = hi2, method = "GC", Transect = belt_numbers, 
                    SampleNumber = SampleNumber_raw, Element = Element_raw, 
                    FK_Event = checked_PK_Event_gc, SyncKey = 33, SyncState = 1)
      }
    }
  }
  
  }
  
  