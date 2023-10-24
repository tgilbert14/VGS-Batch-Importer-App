## function to read shape file for USFS Pastures and format for naming
## convention used for USFS sites -->

## silence loading warning for sp/rgdal
"rgdal_show_exportToProj4_warnings"="none"
## GLOBAL ----------------------------------------------------------------------
library(openxlsx)
library(tidyverse)
library(DBI)
library(RSQLite)
library(uuid)
library(sf)
library(sp)
library(rgdal)

#e.g., Region='04', Forest_Num='03'
#usfs_folder_names(Region = "04", Forest_Num = "03")
#View(pasture_data)
#usfs_folder_names()

usfs_folder_names<- function(Region='NULL',Forest_Num='NULL') {
  ##app_path set by app.R file
  shapefile<- rgdal::readOGR(paste0(app_path,"/www/S_USA.Pasture"), "S_USA.Pasture")
  pasture<- as.data.frame(shapefile)
  
  ## must have acres to be created - but keeping closed pastures for now
  pasture<- pasture %>% 
    filter(TOTAL_ACRE != 0)
  
  ## format naming convention
  pasture_w_codes<- pasture %>% 
    dplyr::mutate('USFS_Code' = paste0(substr(ADMIN_ORG,1,2),"-",
                                       substr(ADMIN_ORG,3,4),"-",
                                       substr(ADMIN_ORG,5,6),"-",
                                       ALLOTMENT1,"-",PASTURE_NU))
  
  ## format columns for metadata for folders
  p<- pasture_w_codes %>% 
    dplyr::mutate('Region_Filter' = paste0(substr(ADMIN_ORG,1,2))) %>% 
    dplyr::mutate('Forest_Number' = paste0(substr(ADMIN_ORG,3,4))) %>%
    dplyr::mutate('RD_Number' = paste0(substr(ADMIN_ORG,5,6))) %>% 
    dplyr::mutate('Ranger District' = paste0(ADMIN_ORG_)) %>%
    dplyr::mutate('Allotment' = paste0(ALLOTMENT_, " Allotment")) %>%
    dplyr::mutate('Pasture' = paste0(PASTURE_NA, " Pasture"))
  
  pasture_data<- p %>% 
    select(ADMIN_ORG_, RD_Number, Allotment, ALLOTMENT1, Pasture, PASTURE_NU, USFS_Code, Region_Filter, Forest_Number)
  
  if (Region != "NULL") {
    ## filter to region
    pasture_data <- pasture_data %>% 
      filter(Region_Filter == Region)
  }
  
  if (Forest_Num != "NULL") {
    ## filter to region
    pasture_data <- pasture_data %>% 
      filter(Forest_Number == Forest_Num)
  }
  
  ## Get rid of troublesome characters
  pasture_data$ADMIN_ORG_<- gsub(pattern = "&", replacement = "", pasture_data$ADMIN_ORG_)
  pasture_data$Allotment<- gsub(pattern = "&", replacement = "", pasture_data$Allotment)
  pasture_data$Pasture<- gsub(pattern = "&", replacement = "", pasture_data$Pasture)
  
  pasture_data$ADMIN_ORG_<- gsub(pattern = "/", replacement = "-", pasture_data$ADMIN_ORG_)
  pasture_data$Allotment<- gsub(pattern = "/", replacement = "-", pasture_data$Allotment)
  pasture_data$Pasture<- gsub(pattern = "/", replacement = "-", pasture_data$Pasture)
  
  pasture_data$ADMIN_ORG_<- gsub(pattern = "#", replacement = "", pasture_data$ADMIN_ORG_)
  pasture_data$Allotment<- gsub(pattern = "#", replacement = "", pasture_data$Allotment)
  pasture_data$Pasture<- gsub(pattern = "#", replacement = "", pasture_data$Pasture)
  
  pasture_data$ADMIN_ORG_<- str_to_title(pasture_data$ADMIN_ORG_)
  pasture_data$Allotment<- str_to_title(pasture_data$Allotment)
  pasture_data$Pasture<- str_to_title(pasture_data$Pasture)

  ## replace repeats - Allotment and Pasture
  replacements_a <- grep("Allotment Allotment", pasture_data$Allotment)
  replacements_p <- grep("Pasture Pasture", pasture_data$Pasture)
  
  ## Getting rid of doubles -------
  i=1
  while (i < length(replacements_a)+1) {
    pasture_data[replacements_a,][,3][i]<-
      paste0(substr(pasture_data[grep("Allotment Allotment", pasture_data$Allotment),][,3][i],1,
                    ## end of sub-string - get rid of extra allotment
                    nchar(pasture_data[grep("Allotment Allotment", pasture_data$Allotment),][,3][i])-10))
    print(paste0(i," - Updated ", pasture_data$Allotment[i]))
    i=i+1
  }
  ## replace repeats - Pasture
  i=1
  while (i < length(replacements_p)+1) {
    pasture_data[replacements_p,][,5][i]<-
      paste0(substr(pasture_data[grep("Pasture Pasture", pasture_data$Pasture),][,5][i],1,
                    ## end of sub-string - get rid of extra pasture
                    nchar(pasture_data[grep("Pasture Pasture", pasture_data$Pasture),][,5][i])-8))
    print(paste0(i," - Updated ", pasture_data$Pasture[i]))
    i=i+1
  }
  pasture_data<<- pasture_data
  ## save as xlsx file
  openxlsx::write.xlsx(pasture_data, file = "www/pasture_data.xlsx")
  ## ---------------
}

#usfs_folder_names()
#View(pasture_data)




