## read in output log
qaqc <- read_csv(paste0(app_path, "/www/r_output.txt"))
#View(qaqc)

sp_conflicts <- unique(qaqc[grep("Species", qaqc[[1]], ignore.case = T), ])

names(sp_conflicts)[1] <- "Species errors in VGS"
if (nrow(sp_conflicts) == 0) {
  sp_conflicts <- "No species conflicts found"
}

sp_conflicts <- as.data.frame(sp_conflicts)

sp_errors <- sp_conflicts %>%
  arrange(sp_conflicts)

write.xlsx(sp_errors, paste0(app_path, "/www/Conflicts/sp_errors.xlsx"))
## open file
file.show(paste0(app_path, "/www/Conflicts/sp_errors.xlsx"))
# file.show(paste0(app_path,"/www/r_output.txt"))

## GC counts
#file_sum <- unique(qaqc[grep("Moving to File", qaqc[[1]]), ])
gc_sum <- qaqc[grep("ground cover points", qaqc[[1]]), ]
#gc_check <- rbind(file_sum, gc_sum)
write.xlsx(gc_sum, paste0(app_path, "/www/Conflicts/gc_counts.xlsx"))
## open file
# file.show(paste0(app_path,"/www/Conflicts/gc_counts_",site_name,".xlsx"))

## queries to check data in VGS .db
## list of all species added - do any of them need to be changed?

## ## list of gc categories
gc_used <- paste0("SELECT DISTINCT PK_Species, SpeciesName, CommonName  from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'GC'")

ground_covers_used_by_events <- dbGetQuery(mydb, gc_used)
write.xlsx(ground_covers_used_by_events, paste0(app_path, "/www/Conflicts/ground_cover_cat_used.xlsx"))

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
file.show(paste0(app_path, "/www/Conflicts/species_count.xlsx"))

## generating codes that need to be updated
## Create "www/SpeciesReplace.xlsx" file with error species
if (nrow(sp_errors) > 0) {
  r=1
  sp_codes_2_update<- list()
  while (r < nrow(sp_errors)+1) {
    
    start_pos<- gregexpr("Species: ", sp_errors$`Species errors in VGS`[r])
    end_pos<- gregexpr("not in", sp_errors$`Species errors in VGS`[r])

    #print(substr(sp_errors$`Species errors in VGS`[r], start_pos[[1]][1]+9, end_pos[[1]][1]-2))
    sp_codes_2_update[r]<- substr(sp_errors$`Species errors in VGS`[r], start_pos[[1]][1]+9, end_pos[[1]][1]-2)

    r=r+1
  }

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
