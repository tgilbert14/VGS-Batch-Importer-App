##R4_BT_site_info

## using find_label()
site_name <<- find_label(historical_raw_data, "Transect Name:", location = "right")
event_date <<- find_label(historical_raw_data, "Date:", location = "right")
event_date <<- as.Date(as.numeric(event_date), origin = "1899-12-30")
event_date <<- format(event_date, "%Y-%m-%d")
event_date <<- paste0(event_date, " 00:00:00")
site_name <<- paste0(site_name," (",substr(event_date,0,7),")")

# elevation <<- find_label(historical_raw_data, "Elevation:", location = "below")
# slope <<- find_label(historical_raw_data, "Slope (%)", location = "below")
# aspect <<- find_label(historical_raw_data, "Aspect (°)", location = "below")
# site_notes <<- find_label(historical_raw_data, "Notes:", location = "below")

## BTNF historical designed sheets do not have this data
elevation <<- NA
slope <<- NA
aspect <<- NA
site_notes <<- NA

# site_notes <- gsub('"', "", site_notes_1, fixed = T)
# site_notes <- gsub("'", "", site_notes_1, fixed = T)

if (length(site_notes) == 0 || is.na(site_notes)) {
  site_notes <<- "NULL"
}

## no event notes for now
EventNotes <<- "NULL"

## LOCATOR TABLE -->
# lat <- find_label(historical_raw_data, "Latitude/Northing", location = "below")
# long <- find_label(historical_raw_data, "Longitude/Easting", location = "below")
# zone <- find_label(historical_raw_data, "Zone (if applicable):", location = "below")

## Also no locator data
lat <- NA
long <- NA
zone <- NA

## to make more readable -->
name<- basename(data_file[batch_file])
full_site_name<- substr(name,0,nchar(name)-5)

## get final site name
find_dash<- gregexpr(pattern = "-",text = full_site_name)[[1]]
len_dash<- length(find_dash)
loc_last_dash<- find_dash[[len_dash]]
site_name <<- substr(full_site_name,loc_last_dash+1,nchar(full_site_name))

## Get entire numeric name to compare -->
usfs_code<- substr(full_site_name,1,18)

## compare to USFS pasture file to get names (pasture_info)-->
## filter to region 4
pasture_names<- pasture_info %>% 
  filter(Region_Filter == '04')
## filter to Bridger Teton NF
pasture_names<- pasture_names %>% 
  filter(Forest_Number == '03')

site_folder_info<- pasture_names %>% 
  filter(USFS_Code == usfs_code)

if (nrow(site_folder_info)==0) {
  shinyalert("SiteName error for file name!", paste0("Could not find numeric site name in USFS shapefile - ",usfs_code), type = "error")
  Sys.sleep(10)
  ## print for log
  print(paste0("Could not find site name in USFS shapefile ",usfs_code))
  ##stop app
  stop(paste0("Could not find site name in USFS shapefile ",usfs_code))
}

## save folder names
pasture <- paste0(site_folder_info$Pasture)
allotment <- paste0(site_folder_info$Allotment)
ranger_district <- paste0(site_folder_info$ADMIN_ORG_)
forest <- paste0("Bridger-Teton National Forest")
