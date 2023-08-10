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
GUID <- function(type='pk', number_of_GUIDS=1) {
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
Hex <- function(vgs5_guid) {
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

