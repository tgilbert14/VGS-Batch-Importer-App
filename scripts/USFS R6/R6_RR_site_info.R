## if (ServerKey == "USFS R6-RR") {}
## formatting site meta data

# site_name <- historical_raw_data[grep("key", historical_raw_data[[1]], ignore.case = TRUE),][[2]]
site_name <<- historical_raw_data[grep("siteID", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
event_date <<- historical_raw_data[grep("date", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
event_date <<- as.Date(as.numeric(event_date), origin = "1899-12-30")
event_date <<- format(event_date, "%Y-%m-%d")
event_date <<- paste0(event_date, " 00:00:00")
## spelling error for key below -->
elevation <<- historical_raw_data[grep("elevat", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
# elevation <- historical_raw_data[grep("elevation", historical_raw_data[[1]], ignore.case = TRUE),][[2]]
slope <<- historical_raw_data[grep("slope", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
aspect <<- historical_raw_data[grep("aspect", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
# site_notes_1 <- historical_raw_data[grep("general", historical_raw_data[[1]], ignore.case = TRUE),][[2]]
site_notes_1 <- historical_raw_data[grep("site notes", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
site_notes_1 <- gsub('"', "", site_notes_1, fixed = T)
site_notes_1 <- gsub("'", "", site_notes_1, fixed = T)
# site_notes_2 <- historical_raw_data[grep("mlra", historical_raw_data[[1]], ignore.case = TRUE),][[2]]
site_notes_2 <- historical_raw_data[grep("event note", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
site_notes_2 <- gsub('"', "", site_notes_2, fixed = T)
site_notes_2 <- gsub("'", "", site_notes_2, fixed = T)

## Formatting notes ---- change depending on batch data
## if site_notes_1 present
if (length(site_notes_1) > 0 && !is.na(site_notes_1)) {
  site_notes <<- site_notes_1
}
if (length(site_notes_1) == 0 || is.na(site_notes_1)) {
  site_notes <<- "NULL"
}
## event notes
if (length(site_notes_2) > 0 && !is.na(site_notes_2)) {
  EventNotes <<- site_notes_2
}
if (length(site_notes_2) == 0 || is.na(site_notes_2)) {
  EventNotes <<- "NULL"
}
## End of formatting notes

## LOCATOR TABLE -->
lat <- historical_raw_data[grep("latitude", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]
long <- historical_raw_data[grep("longitude", historical_raw_data[[1]], ignore.case = TRUE), ][[2]]

## SiteClass Table -->
pasture <- find_label(historical_raw_data, search_term = "Pasture", location = "right")
allotment <- find_label(historical_raw_data, search_term = "Allotment", location = "right")
forest <- find_label(historical_raw_data, search_term = "Forest", location = "right")
ranger_district <- find_label(historical_raw_data, search_term = "Ranger District", location = "right")
