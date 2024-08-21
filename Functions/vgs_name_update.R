## in progress...

## updating site names after in correct folders
## run this after all sites are placed in correct folders
library(DBI)
library(tidyverse)

## this is locally, need to tie into app
#setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
#app_path <<- getwd()

## path for VGS database
db_loc <- "C:/ProgramData/VGSData/VGS50.db"
## connecting to VGS database
mydb <- dbConnect(RSQLite::SQLite(), dbname = db_loc)
## read in xlsx file with USFS shapefile/naming info
pasture_info<- openxlsx::read.xlsx(paste0(app_path,"/www/Export.xlsx"))

## compare to USFS pasture file to get names (pasture_info)-->
## filter to region 6
# pasture_names<- pasture_info %>% 
#   filter(Region_Filter == '06')
## filter to RR
# pasture_names<- pasture_names %>% 
#   filter(Forest_Number == '03')

site_lookup <- paste0("Select SiteID, SiteClass.ClassName, quote(Ck_parentClass) from Site
INNER JOIN SiteClassLink on SiteClassLink.FK_Site = Site.PK_Site
INNER JOIN SiteClass on SiteClass.PK_SiteClass = SiteClassLink.FK_SiteClass
Where SiteClass.SyncState = 0")
## dependent on download level...
# class_lookup <- paste0("Select ClassName, quote(PK_SiteClass) from SiteClass
# where SyncState = 0")
# 
# site_sql_info <- DBI::dbGetQuery(mydb, site_lookup)
# class_sql_info <- DBI::dbGetQuery(mydb, class_lookup)
# 
# names(site_sql_info)[3]<- "join_me"
# names(class_sql_info)[2]<- "join_me"
# 
# v<- full_join(site_sql_info, class_sql_info)
# View(v)
site_sql_info <- DBI::dbGetQuery(mydb, site_lookup)

## edit/format pasture info file
s<- pasture_info %>% 
  mutate(PASTURE_NA = paste0(str_to_title(PASTURE_NA)," Pasture"))
#View(s)

f<- grep(site_sql_info$ClassName[1], s$PASTURE_NA)
data<- s[f,]
#View(data)

View(pasture_info)
