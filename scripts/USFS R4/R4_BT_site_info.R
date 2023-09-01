##R4_BT_site_info

## using find_label()
site_name <<- find_label(historical_raw_data, "Site:", location = "right")
event_date <<- find_label(historical_raw_data, "Visit Date:", location = "right")
event_date <<- as.Date(as.numeric(event_date), origin = "1899-12-30")
event_date <<- format(event_date, "%Y-%m-%d")
event_date <<- paste0(event_date, " 00:00:00")

elevation <<- find_label(historical_raw_data, "Elevation:", location = "below")
slope <<- find_label(historical_raw_data, "Slope (%)", location = "below")
aspect <<- find_label(historical_raw_data, "Aspect (°)", location = "below")
site_notes <<- find_label(historical_raw_data, "Notes:", location = "below")

# site_notes <- gsub('"', "", site_notes_1, fixed = T)
# site_notes <- gsub("'", "", site_notes_1, fixed = T)

if (length(site_notes) == 0 || is.na(site_notes)) {
  site_notes <<- "NULL"
}

## no event notes for now
EventNotes <<- "NULL"




## LOCATOR TABLE -->
lat <- find_label(historical_raw_data, "Latitude/Northing", location = "below")
long <- find_label(historical_raw_data, "Longitude/Easting", location = "below")
zone <- find_label(historical_raw_data, "Zone (if applicable):", location = "below")
