## read in output log for qa/qc reports ->
qaqc <- read_csv(paste0(app_path, "/www/r_output.txt"))
View(qaqc)

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
if (nrow(sp_errors)>1) {
  file.show(paste0(app_path, "/www/Conflicts/sp_errors.xlsx"))
}

## looking for ground cover counts in log
gc_sum <- qaqc[grep("ground cover points", qaqc[[1]]), ]
names(gc_sum)[1] <- "Ground Cover Counts"
## report generated and opened if data
write.xlsx(gc_sum, paste0(app_path, "/www/Conflicts/gc_counts.xlsx"))
if (nrow(gc_sum)>1) {
  file.show(paste0(app_path, "/www/Conflicts/gc_counts.xlsx"))
}

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
if (nrow(ground_covers_used_by_events)>1) {
  file.show(paste0(app_path, "/www/Conflicts/ground_cover_cat_used.xlsx"))
}

## Check all species added
sp_count <- paste0("SELECT DISTINCT PK_Species, SpeciesName, CommonName, SpeciesQualifier, Count(PK_Species)  from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'NRCS'
  group by PK_Species, SpeciesName, CommonName, SpeciesQualifier")

species_count <- dbGetQuery(mydb, sp_count)

species_count <- species_count %>%
  arrange(SpeciesName)

write.xlsx(species_count, paste0(app_path, "/www/Conflicts/species_count.xlsx"))
if (nrow(species_count)>1) {
  file.show(paste0(app_path, "/www/Conflicts/species_count.xlsx"))
}

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
if (nrow(gc_miss)>1) {
  file.show(paste0(app_path,"/www/Conflicts/gc_missing_or_too_much.xlsx"))
}

## GC % errors
gc_perc_errors <- qaqc[grep("GC % sums", qaqc[[1]]), ]
names(gc_miss)[1] <- "Ground Cover % Warnings"

write.xlsx(gc_perc_errors, paste0(app_path, "/www/Conflicts/gc_perc_errors.xlsx"))
if (nrow(gc_miss)>0) {
  file.show(paste0(app_path,"/www/Conflicts/gc_perc_errors.xlsx"))
}

## Script to pull siteclass names for folders to look for spelling errors to fix
# siteClass <- paste0("SELECT ClassName from SiteClass")
# 
# siteClassNames <- dbGetQuery(mydb, siteClass)
# 
# nameCheck_RD <- siteClassNames %>%
#   filter(str_detect(ClassName, fixed("Ranger")))
# 
# rd_check<- unique(nameCheck_RD)

