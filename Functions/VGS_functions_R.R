## Create function to create GUID for VGS

## Read in libraries -----------------------------------------------------------
library(uuid)
library(tidyverse)
library(readxl)
library(DBI)
library(RSQLite)
library(stringr)

## SQL local Connection info from R to local VGS5 (VGS50.db)
db_loc <<- "C:/ProgramData/VGSData/VGS50.db"
mydb <<- dbConnect(RSQLite::SQLite(), dbname = db_loc)
## -----------------------------------------------------------------------------

## GUID creation function ------------------------------------------------------
GUID <<- function(type='pk', number_of_GUIDS=1) {
  ## generate generic GUID
  g<-uuid::UUIDgenerate(n=number_of_GUIDS)
  ## formatting based on type of GUID
  if (type == 'pk') { ## for pk's
    g2<-paste0("X'",substr(g,1,8),substr(g,10,13),substr(g,15,18),
               substr(g,20,23),substr(g,25,36),"'")
  }
  if (type == 'sp') { ## for species
    ## Other and Ground Cover Lists Supported - Need to add OT or G in front
    g2<-toupper(paste0("_$",substr(g,1,8),substr(g,10,11),"'"))
  }
  return(g2)
}
## End of GUID function --------------------------------------------------------

## Standard to Hex Conversion
Hex <<- function(vgs5_guid) {
  ## converting to hex
  guid<- vgs5_guid
  # Remove curly braces and hyphens
  guid <- gsub("[{}-]", "", guid)
  guid
  # Convert to hexadecimal
  hex_guid <- tolower(paste0(substr(guid, 7, 8), substr(guid, 5, 6), substr(guid, 3, 4),
                             substr(guid, 1, 2),substr(guid,11,12),substr(guid,9,10),
                             substr(guid,15,16),substr(guid,13,14),
                             gsub("-", "", substr(guid, 17, 36))))
  
  hex_guid <- paste0("X'",hex_guid,"'")
  # Return the result
  return(hex_guid)
}

## finds data if to the right of label -> "search_term"
## multiple=T -> should fuction return multiple values if present or select 1st value
## location refers to selecting values location in reference to label
## location -> ('below','above','right','left)
find_label <<- function(data, search_term, location, multiple=TRUE) {
  location <- tolower(location) ## formatting location selection - lower case
  i=1 ## initial column search and variable
  ## to check if label is in first row
  term_here <- data[which(data[i] == search_term),i]
  ## only for 1st column
  if (location == "right") {
    found_data <- data[which(data[i] == search_term),i+1]
  }
  if (location == "left" && nrow(term_here) != 0) {
    stop(paste0(search_term, " was found in the first column, there is nothing to the left of it!"))
  }
  if (location == "below" || location == "down") {
    found_data <- data[which(data[i] == search_term)+1,i]
  }
  if (location == "above" || location == "up") {
    found_data <- data[which(data[i] == search_term)-1,i]
  }
  ## can only do this for how many columns are in data sheet/dataframe
  ## do for every row but last row
  while ( (i < ncol(data)+1) && (nrow(found_data) == 0) ) {
    ## if search term for data not found, check next column ->
    if (nrow(found_data) == 0) {
      ## i+1 for search term because data is alreay at coulumn '+1' (right)
      if (location == "right" && (i < ncol(data)-1)) {
        found_data <- data[which(data[i+1] == search_term),i+2]
      }
      if (location == "right" && (i == ncol(data))) {
        stop(paste0(search_term, " was found in the last column, there is nothing to the right of it!"))
      }
      if (location == "left") {
        found_data <- data[which(data[i+1] == search_term),i]
      }
      if (location == "below" || location == "down") {
        found_data <- data[which(data[i+1] == search_term)+1,i+1]
      }
      if (location == "above" || location == "up") {
        found_data <- data[which(data[i+1] == search_term)-1,i+1]
      }
      i=i+1
    }
  }
  ## for last column
  if (multiple == FALSE) {
    ## select 1st value
    found_data <- found_data[[1]][1]
  }
  ## if NA
  if (is.na(found_data) || nrow(found_data) == 0) {
    found_data <- paste0("Nothing string/label found called '",search_term,"'")
  }
  return(found_data)
}