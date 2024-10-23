## notes update, site merge ->
## merge site metadata and locators, delete old site

## updated version to get rid of date and add to site notes
site.notes.q <- paste0("UPDATE Site
SET Notes = SUBSTR(SiteID, 1, INSTR(SiteID, '(') - 2) || ' - ' || COALESCE(Notes, '');")
dbExecute(mydb, site.notes.q)

print("Site Notes Updated")

## then getting rid of dates for site names and special characters
site.id.q <- paste0("UPDATE Site
SET SiteID = REPLACE(REPLACE(REPLACE(REPLACE(SUBSTR(SiteID, 1,
INSTR(SiteID, '(') - 2), '#', ''), '&', '-'), '$', ''), '%', '')
WHERE INSTR(SiteID, '(') > 0;")
dbExecute(mydb, site.id.q)

print("SiteID Names Updated")

## merge sites with same name
site.info.q <- paste0("SELECT quote(PK_Site) as PK_Site, SiteID, Notes, FK_Species_Site, FK_Species_SiteStatus, FK_Species_ElevUnits, Slope, Aspect, Elevation, DateEstablished
FROM Site
WHERE SiteID IN (
    SELECT SiteID
    FROM Site
    GROUP BY SiteID
    HAVING COUNT(*) > 1
)
ORDER BY SiteID;")

duplicated.sites.4.merge<- dbGetQuery(mydb, site.info.q)
#View(duplicated.sites.4.merge)

## list of each site
site.dup.list <- unique(duplicated.sites.4.merge$SiteID)

s=1
## go through each duplicated site and merge data into single site
while (s <= length(site.dup.list)) {
  
  print(paste0("Duplicate Site -> prepping to merge: ",site.dup.list[s]))
  
  ## compare each column for conflicts
  site.2.merge <- duplicated.sites.4.merge[duplicated.sites.4.merge$SiteID==site.dup.list[s],]
  row=1
  ## go through each row in the site.2.merge (could be 2, 3 , or more...)
  ### Only 2 tested thus far...
  while (row < nrow(site.2.merge)) {
    ## also identifying columns to check for merge, start at 3rd column
    ## 1st is PK_site, 2nd is SiteID so don't need to check those
    col<- 3
    while (col <= ncol(site.2.merge)) {
      ## looking at duplicate values that are not NA
      s.value <- unique(site.2.merge[col]) %>% 
        filter(unique(!is.na(site.2.merge[col])))
      
      ## stop app if any site col conflicts (except notes = append!)
      if (nrow(s.value)>1 && names(site.2.merge[col]) != "Notes") {
        ## don't know correct values if conflicting metadata (e.g., aspect/slope/etc...)
        stop(paste0(site.dup.list[s]," has conflict with merge - ",names(site.2.merge[col])))
      }
      
      ## if any site col conflicts for notes and there is additional site to merge, merge notes
      if (nrow(s.value)>1 && names(site.2.merge[col]) == "Notes" && !is.na(site.2.merge$Notes[row+1])) {
        # Remove the common part from note2
        note2_unique <- gsub(s.value$Notes[s], "", s.value$Notes[row+1])
        # Combine the notes
        combined_notes <- trimws(paste(s.value$Notes[s], note2_unique, sep = ""))
        col.q<- paste0("Update Site Set ",names(site.2.merge[col]),"='",combined_notes,"' Where PK_Site=",site.2.merge$PK_Site[1])
        dbExecute(mydb, col.q)
      }
      
      ## update site 1 to have all relevant data
      if (nrow(s.value) > 0 && names(site.2.merge[col]) != "Notes") {
        #print(paste0(names(site.2.merge[col])," updated to ",s.value[[1]]," for ",unique(site.2.merge$SiteID)))
        col.q<- paste0("Update Site Set ",names(site.2.merge[col]),"='",s.value[[1]],"' Where PK_Site=",site.2.merge$PK_Site[1])
        dbExecute(mydb, col.q)
      }
      ## move to next column
      col=col+1
    }
    
    ## move event in site 2 (row+1) if present to site 1
    if (!is.na(site.2.merge$PK_Site[row+1])) {
      move.event.q <- paste0("Update Event Set FK_Site= ",site.2.merge$PK_Site[1]," Where FK_Site=",site.2.merge$PK_Site[row+1])
      dbExecute(mydb, move.event.q)
      print(paste0("Merged Duplicate SitesID - PK_Site = ",site.2.merge$PK_Site[1]))
      
      ## check locators for site 1 before merging more ->
      get.loc.q <- paste0(
        "Select * from Locator Where FK_Site=",site.2.merge$PK_Site[1])
      loc.data <- dbGetQuery(mydb, get.loc.q)
      ## using max in case multiple locators for site 1 - if at least 1 primary,
      ## set IsPrimary=0, else set to 1 (primary if none prime already)
      if (nrow(loc.data)>0) { ## if locator exists
        if (max(loc.data$IsPrimary) == 1) {
          new.loc.primary <- 0
        } else {
          new.loc.primary <- 1
        }
      }
      
      ## check lat/long for site 2 before merging - check if different ->
      get.loc.q.2 <- paste0(
        "Select * from Locator Where FK_Site=",site.2.merge$PK_Site[row+1])
      loc.data.2 <- dbGetQuery(mydb, get.loc.q.2)
      ## if 0 rows, no new lat/long data
      new.loc.check<- nrow(setdiff(loc.data[c("DDLat","DDLong")],loc.data.2[c("DDLat","DDLong")]))
      
      if (new.loc.check>0) {
        ## move locators from site 2 ->
        move.loc.q <- paste0(
          "Update Locator Set FK_Site=",site.2.merge$PK_Site[1],
          ", IsPrimary=",new.loc.primary," Where FK_Site=",site.2.merge$PK_Site[row+1])
        dbExecute(mydb, move.loc.q)
        print(paste0("Moved Locator - PK_Site = ",site.2.merge$PK_Site[1]))
      }
      
      ## delete pk_site (row+1) after moved data to site 1
      delete.site.q <- paste0("Delete from Site Where PK_Site= ",site.2.merge$PK_Site[row+1])
      dbExecute(mydb, delete.site.q)
      print(paste0("Empty Site Deleted - PK_Site = ",site.2.merge$PK_Site[row+1]))
    }
    ## check for 3 or more duplicate sites...
    row=row+1
  }
  ## move to next duplicate sites to merge
  s=s+1
}

print("Site merge(s) complete...")

