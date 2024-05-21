## read in output log for qa/qc reports ->
qaqc <- read_csv(paste0(app_path, "/www/r_output.txt"))
#View(qaqc)

## looking for species conflicts in log
sp_conflicts <- unique(qaqc[grep("Species", qaqc[[1]], ignore.case = T), ])

names(sp_conflicts)[1] <- "Species errors in VGS"
if (nrow(sp_conflicts) == 0) {
  sp_conflicts <- "No species conflicts found"
}

sp_conflicts <- as.data.frame(sp_conflicts)

sp_errors <- sp_conflicts %>%
  arrange(sp_conflicts)

write.xlsx(sp_errors, paste0(app_path, "/www/Conflicts/sp_errors.xlsx"))
# if (nrow(sp_errors)>1) {
#   file.show(paste0(app_path, "/www/Conflicts/sp_errors.xlsx"))
# }

## looking for ground cover counts in log
gc_sum <- qaqc[grep("ground cover points", qaqc[[1]]), ]
names(gc_sum)[1] <- "Ground Cover Counts"
## report generated and opened if data
write.xlsx(gc_sum, paste0(app_path, "/www/Conflicts/gc_counts.xlsx"))
# if (nrow(gc_sum)>1) {
#   file.show(paste0(app_path, "/www/Conflicts/gc_counts.xlsx"))
# }

## looking for species counts in VGS .db
## Queries to check data in VGS .db
## list of all species added - do any of them need to be changed?

## list of gc categories
gc_used <- paste0("SELECT DISTINCT PK_Species, SpeciesName, CommonName  from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'GC'")
## report generated and opened if data
ground_covers_used_by_events <- dbGetQuery(mydb, gc_used)

write.xlsx(ground_covers_used_by_events, paste0(app_path, "/www/Conflicts/ground_cover_cat_used.xlsx"))
# if (nrow(ground_covers_used_by_events)>1) {
#   file.show(paste0(app_path, "/www/Conflicts/ground_cover_cat_used.xlsx"))
# }

## Check all species added
sp_count <- paste0("SELECT DISTINCT PK_Species, Species.NewSynonym as 'Updated Code', SpeciesName, CommonName, SpeciesQualifier, Count(PK_Species)  from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Site ON Site.PK_Site = Event.FK_Site
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'NRCS' and eventName LIKE '%Frequency%'
  group by PK_Species, SpeciesName, CommonName, SpeciesQualifier")

species_count <- dbGetQuery(mydb, sp_count)

# species_count <- species_count %>%
#   arrange(SpeciesName)

write.xlsx(species_count, paste0(app_path, "/www/Conflicts/species_count.xlsx"))
# if (nrow(species_count)>1) {
#   file.show(paste0(app_path, "/www/Conflicts/species_count.xlsx"))
# }

## Check all species added BY SITE
sp_count_site <- paste0("SELECT DISTINCT SiteID, Protocol.Date, PK_Species, Species.NewSynonym as 'Updated Code', SpeciesName, CommonName, SpeciesQualifier, Count(PK_Species)  from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Site ON Site.PK_Site = Event.FK_Site
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'NRCS' and eventName LIKE '%Frequency%'
  group by SiteID, Protocol.Date, PK_Species, SpeciesName, CommonName, SpeciesQualifier")

species_count_by_site <- dbGetQuery(mydb, sp_count_site)
# species_count <- species_count %>%
#   arrange(SpeciesName)

write.xlsx(species_count_by_site, paste0(app_path, "/www/Conflicts/species_count_by_site.xlsx"))


## generating codes that need to be updated
## Create "www/SpeciesReplace.xlsx" file with error species
if (nrow(sp_errors) > 1) {
  r=1
  sp_codes_2_update<- list()
  
  while (r < nrow(sp_errors)+1) {
    
    start_pos<- gregexpr("Species: ", sp_errors$`Species errors in VGS`[r])
    end_pos<- gregexpr("not in", sp_errors$`Species errors in VGS`[r])

    #print(substr(sp_errors$`Species errors in VGS`[r], start_pos[[1]][1]+9, end_pos[[1]][1]-2))
    sp_codes_2_update[r]<- substr(sp_errors$`Species errors in VGS`[r], start_pos[[1]][1]+9, end_pos[[1]][1]-2)

    r=r+1
  }

sp_codes_2_update<- unique(sp_codes_2_update)
sp_code_matrix<- matrix(sp_codes_2_update, nrow = length(sp_codes_2_update))
sp_code_df<- as.data.frame(sp_code_matrix)

sp_code_df <- sp_code_df %>% 
  filter(nchar(sp_code_df$V1)>0) %>% 
  arrange(V1)

names(sp_code_df)<- "OldCodes"
new_codes<- data.frame("NewCode"="")
sp_update_file<- cbind(sp_code_df, new_codes)

## update old file name and move, then save new one
gu<- GUID(type = "sp")
gu<- sub("\\$","",gu)
gu<- sub("'","",gu)
file.rename(from = paste0(app_path, "/www/SpeciesReplace.xlsx"), to = paste0(app_path, "/www/SpeciesReplaceBackups/SpeciesReplace",gu,".xlsx"))
write.xlsx(sp_update_file, paste0(app_path, "/www/SpeciesReplace.xlsx"))
}

## GC missing or too much
gc_miss <- qaqc[grep("Warning,", qaqc[[1]]), ]
names(gc_miss)[1] <- "Ground Cover Count Warnings"

write.xlsx(gc_miss, paste0(app_path, "/www/Conflicts/gc_missing_or_too_much.xlsx"))
# if (nrow(gc_miss)>1) {
#   file.show(paste0(app_path,"/www/Conflicts/gc_missing_or_too_much.xlsx"))
# }

## GC % errors
gc_perc_errors <- qaqc[grep("GC % sums", qaqc[[1]]), ]
names(gc_perc_errors)[1] <- "Ground Cover % Warnings"

write.xlsx(gc_perc_errors, paste0(app_path, "/www/Conflicts/gc_perc_errors.xlsx"))
# if (nrow(gc_perc_errors)>0) {
#   file.show(paste0(app_path,"/www/Conflicts/gc_perc_errors.xlsx"))
# }

## Script to pull siteclass names for folders to look for spelling errors to fix
siteClass <- paste0("SELECT ClassName from SiteClass
                    order by ClassName")

siteClassNames <- dbGetQuery(mydb, siteClass)

# nameCheck_RD <- siteClassNames %>%
#   filter(str_detect(ClassName, fixed("Ranger")))

className_check <- unique(siteClassNames)
write.xlsx(className_check, paste0(app_path, "/www/SiteClassNameChecks/FolderNames.xlsx"))
#file.show(paste0(app_path,"/www/SiteClassNameChecks/FolderNames.xlsx"))


## query transects in use for freq and count # of species per event
## flag if not expected 80 per transect !!
## -- to get transects being used for freq
transect_q <- "SELECT siteID, Protocol.Date, count(DISTINCT transect) from Protocol
INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
INNER JOIN Site ON Site.PK_Site = Event.FK_Site
INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
where eventName LIKE '%Frequency%'
group by siteID, Protocol.Date
order by SiteID"
## -- to get # of gc per site/date
gc_points_q <- "SELECT DISTINCT SiteID, Protocol.Date, count(PK_Sample) from Protocol
INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
INNER JOIN Site ON Site.PK_Site = Event.FK_Site
INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
where eventName LIKE '%Point Ground Cover%'
group by SiteID, Protocol.Date
order by SiteID"

## -- to get # of gc per site/date
notes_q <- "SELECT DISTINCT SiteID, Protocol.Date, Site.Notes as 'S.notes', Protocol.Notes as 'P.notes' from Protocol
INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
INNER JOIN Site ON Site.PK_Site = Event.FK_Site
INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
group by SiteID, Protocol.Date
order by SiteID"

transect_by_site <- dbGetQuery(mydb, transect_q)
gc_points_by_site <- dbGetQuery(mydb, gc_points_q)
notes <- dbGetQuery(mydb, notes_q)

data_summary <- full_join(transect_by_site, gc_points_by_site)

data_sum_expected <- data_summary %>% 
  mutate("Expected gc points" = `count(DISTINCT transect)`*80)
#View(data_sum_expected)

data_sum_expected <- left_join(data_sum_expected, notes)

## possible data entry errors to check in data
see_me_unexpected <- data_sum_expected %>% 
  filter(data_sum_expected$`count(PK_Sample)` != data_sum_expected$`Expected gc points`)

see_me_is.na <- data_sum_expected %>% 
  filter(is.na(data_sum_expected$`count(PK_Sample)`)) %>% 
  arrange(SiteID)

see_me_is.na$`count(PK_Sample)`[is.na(see_me_is.na$`count(PK_Sample)`)] <- 0

see_me_gc_errors <- rbind(see_me_unexpected, see_me_is.na)

write.xlsx(see_me_gc_errors, paste0(app_path, "/www/Conflicts/possible_data_errors_gc.xlsx"))
# if (nrow(see_me_gc_errors)>0) {
#   file.show(paste0(app_path,"/www/Conflicts/possible_data_errors_gc.xlsx"))
# }

## checking for messed up dates
date_check <- data_summary %>% 
  filter(str_detect(Date, fixed("NA")))

write.xlsx(date_check, paste0(app_path, "/www/Conflicts/possible_date_errors.xlsx"))
# if (nrow(date_check)>0) {
#   file.show(paste0(app_path,"/www/Conflicts/possible_date_errors.xlsx"))
# }

## looking for duplicates for species code in data - possible comparison report conflicts to fix
sp_qc_data <- read.xlsx(paste0(app_path,"/www/Conflicts/species_count_by_site.xlsx"))

possible_duplicated_species <- sp_qc_data %>% 
  group_by(SiteID, Date) %>%
  filter(duplicated(PK_Species))

possible_duplicated_species <- possible_duplicated_species %>% 
  select(!`Count(PK_Species)`)

message_for_sheet <- data.frame(SiteID = "Go through data sheet and update species/qualifiers...",
                                Date = "----->",PK_Species = "Codes seen multiple times for this SiteID/Date combo...",Updated.Code = "----->",
                                SpeciesName = "----->",CommonName = "",SpeciesQualifier = "----->")

freq_comp_check <- rbind(message_for_sheet, possible_duplicated_species)

## run comparison report and try to fix species for these
write.xlsx(freq_comp_check, paste0(app_path, "/www/Conflicts/possible_duplicated_species.xlsx"))
# if (nrow(freq_comp_check)>0) {
#   file.show(paste0(app_path,"/www/Conflicts/possible_duplicated_species.xlsx"))
# }

wb <- createWorkbook()
# Add a worksheet to the workbook
addWorksheet(wb, "SpeciesCounts")
# Write your data frame (df) to the worksheet
writeData(wb, "SpeciesCounts", species_count)

# highlightStyle <- createStyle(fgFill = "#FFFF00")  # Yellow fill
# conditionalFormatting(wb, "SpeciesCounts", cols = 1:nrow(species_count), rows = 1:ncol(species_count), rule = "(H:H)>100", style = highlightStyle)

addWorksheet(wb, "FreqComparisons")
writeData(wb, "FreqComparisons", freq_comp_check)

addWorksheet(wb, "SpeciesErrors")
writeData(wb, "SpeciesErrors", sp_errors)

addWorksheet(wb, "SiteDateErrors")
writeData(wb, "SiteDateErrors", date_check)

addWorksheet(wb, "PossibleGC_errors")
writeData(wb, "PossibleGC_errors", see_me_gc_errors)

addWorksheet(wb, "GC_percent_errors")
writeData(wb, "GC_percent_errors", gc_perc_errors)

addWorksheet(wb, "GCwarnings")
writeData(wb, "GCwarnings", gc_miss)

addWorksheet(wb, "GCsums")
writeData(wb, "GCsums", gc_sum)

addWorksheet(wb, "GCused")
writeData(wb, "GCused", ground_covers_used_by_events)

addWorksheet(wb, "FolderNames")
writeData(wb, "FolderNames", className_check)

file_loc <- sub(" ","",ServerKey)
file_loc <- gsub("[-&@]", "_", file_loc)

# Save the workbook
saveWorkbook(wb, paste0("www/data_qaqc_workbook_",file_loc,".xlsx"), overwrite = TRUE)
file.show(paste0(app_path,"/www/data_qaqc_workbook_",file_loc,".xlsx"))
