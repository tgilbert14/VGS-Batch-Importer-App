## read in output log
qaqc<-read_csv(paste0(app_path,"/www/r_output.txt"))
#View(qaqc)

sp_conflicts<- unique(qaqc[grep("Species", qaqc[[1]]),])

names(sp_conflicts)[1]<- "Species errors in VGS"
if (nrow(sp_conflicts) == 0) {
  sp_conflicts<- "No species conflicts found"
}

write.xlsx(sp_conflicts, paste0(app_path,"/www/Conflicts/r_sp_conflicts_",site_name,".xlsx"))
## open file
file.show(paste0(app_path,"/www/Conflicts/r_sp_conflicts_",site_name,".xlsx"))
#file.show(paste0(app_path,"/www/r_output.txt"))

## GC counts
file_sum<- unique(qaqc[grep("Moving to File", qaqc[[1]]),])
gc_sum<- unique(qaqc[grep("ground cover points found", qaqc[[1]]),])
gc_check<- rbind(file_sum,gc_sum)
write.xlsx(gc_check, paste0(app_path,"/www/Conflicts/gc_counts.xlsx"))
## open file
#file.show(paste0(app_path,"/www/Conflicts/gc_counts_",site_name,".xlsx"))

## queries to check data in VGS .db
## list of all species added - do any of them need to be changed?

## ## list of gc categories 
gc_used<- paste0("SELECT DISTINCT PK_Species, SpeciesName, CommonName  from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'GC'")

ground_covers_used_by_events<- dbGetQuery(mydb, gc_used)
write.xlsx(ground_covers_used_by_events, paste0(app_path,"/www/Conflicts/ground_cover_cat_used.xlsx"))

## Check all species added
sp_count <- paste0("SELECT DISTINCT PK_Species, SpeciesName, CommonName, SpeciesQualifier, Count(PK_Species)  from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'NRCS'
  group by PK_Species, SpeciesName, CommonName, SpeciesQualifier")

species_count<- dbGetQuery(mydb, sp_count)

species_count<- species_count %>% 
  arrange(SpeciesName)

write.xlsx(species_count, paste0(app_path,"/www/Conflicts/species_count.xlsx"))
file.show(paste0(app_path,"/www/Conflicts/species_count.xlsx"))
