## read in output log for qa/qc reports ->
qaqc <- read_csv(paste0(app_path, "/www/r_output.txt"))
#View(qaqc)
#power_mode<- F

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
if (power_mode == TRUE) {
  if (nrow(sp_errors)>1) {
    file.show(paste0(app_path, "/www/Conflicts/sp_errors.xlsx"))
  }
}

## looking for ground cover count567s in log
gc_sum <- qaqc[grep("ground cover points", qaqc[[1]]), ]
names(gc_sum)[1] <- "Ground Cover Counts"
## report generated and opened if data
write.xlsx(gc_sum, paste0(app_path, "/www/Conflicts/gc_counts.xlsx"))
if (power_mode == TRUE) {
  if (nrow(gc_sum)>1) {
    file.show(paste0(app_path, "/www/Conflicts/gc_counts.xlsx"))
  }
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
if (power_mode == TRUE) {
  if (nrow(ground_covers_used_by_events)>1) {
    file.show(paste0(app_path, "/www/Conflicts/ground_cover_cat_used.xlsx"))
  }
}

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

sp_in_num_of_sites <- paste0("SELECT DISTINCT PK_Species, Species.NewSynonym as 'Updated Code', SpeciesName, CommonName, SpeciesQualifier, Count(DISTINCT PK_Site) as '#DistinctSites' from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Site ON Site.PK_Site = Event.FK_Site
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'NRCS' and eventName LIKE '%Frequency%'
  group by PK_Species, SpeciesName, CommonName, SpeciesQualifier")

species_seen_at_each_site <- dbGetQuery(mydb, sp_in_num_of_sites)

species_count_w_siteInfo <- full_join(species_count, species_seen_at_each_site)

# species_count <- species_count %>%
#   arrange(SpeciesName)

write.xlsx(species_count_w_siteInfo, paste0(app_path, "/www/Conflicts/species_count_w_siteInfo.xlsx"))
if (power_mode == TRUE) {
  if (nrow(species_count_w_siteInfo)>1) {
    file.show(paste0(app_path, "/www/Conflicts/species_count_w_siteInfo.xlsx"))
  }
}

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
if (power_mode == TRUE) {
  if (nrow(gc_miss)>1) {
    file.show(paste0(app_path,"/www/Conflicts/gc_missing_or_too_much.xlsx"))
  }
}


## GC % errors
gc_perc_errors <- qaqc[grep("GC % sums", qaqc[[1]]), ]
names(gc_perc_errors)[1] <- "Ground Cover % Warnings"

write.xlsx(gc_perc_errors, paste0(app_path, "/www/Conflicts/gc_perc_errors.xlsx"))
if (power_mode == TRUE) {
  if (nrow(gc_perc_errors)>0) {
    file.show(paste0(app_path,"/www/Conflicts/gc_perc_errors.xlsx"))
  }
}


## Script to pull siteclass names for folders to look for spelling errors to fix
siteClass <- paste0("SELECT ClassName from SiteClass
                    order by ClassName")

siteClassNames <- dbGetQuery(mydb, siteClass)

# nameCheck_RD <- siteClassNames %>%
#   filter(str_detect(ClassName, fixed("Ranger")))

className_check <- unique(siteClassNames)
write.xlsx(className_check, paste0(app_path, "/www/SiteClassNameChecks/FolderNames.xlsx"))
if (power_mode == TRUE) {
  file.show(paste0(app_path,"/www/SiteClassNameChecks/FolderNames.xlsx"))
}


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
if (power_mode == TRUE) {
  if (nrow(see_me_gc_errors)>0) {
    file.show(paste0(app_path,"/www/Conflicts/possible_data_errors_gc.xlsx"))
  }
}


## checking for messed up dates
date_check <- data_summary %>% 
  filter(str_detect(Date, fixed("NA")))

write.xlsx(date_check, paste0(app_path, "/www/Conflicts/possible_date_errors.xlsx"))
if (power_mode == TRUE) {
  if (nrow(date_check)>0) {
    file.show(paste0(app_path,"/www/Conflicts/possible_date_errors.xlsx"))
  }
}


## looking for duplicates for species code in data - possible comparison report conflicts to fix
sp_qc_data <- read.xlsx(paste0(app_path,"/www/Conflicts/species_count_by_site.xlsx"))
#View(sp_qc_data)
possible_duplicated_species <- sp_qc_data %>% 
  group_by(SiteID) %>%
  filter(duplicated(PK_Species))
#View(possible_duplicated_species)
possible_duplicated_species <- possible_duplicated_species %>% 
  select(!`Count(PK_Species)`)
#View(possible_duplicated_species)
message_for_sheet <- data.frame(SiteID = "Go through data sheet and update species/qualifiers...",
                                Date = "----->",PK_Species = "Codes seen multiple times for this SiteID/Date combo...",Updated.Code = "----->",
                                SpeciesName = "----->",CommonName = "",SpeciesQualifier = "----->")

freq_comp_check <- rbind(message_for_sheet, possible_duplicated_species)
#View(freq_comp_check)

## run comparison report and try to fix species for these
write.xlsx(freq_comp_check, paste0(app_path, "/www/Conflicts/possible_duplicated_species.xlsx"))
if (power_mode == TRUE) {
  if (nrow(freq_comp_check)>0) {
    file.show(paste0(app_path,"/www/Conflicts/possible_duplicated_species.xlsx"))
  }
}


## only happens if species are inserted...
if (power_mode == FALSE) {
  ## special for RR state check...
  if (ServerKey == "USFS R6-RR") {
    ## set USDA states to compare ->
    selected_state <- c("California","Oregon","Washington","Idaho","Nevada")
    state_num <- 1
    ## temp df to bind to later
    final_sp_list <- data.frame("code" = "")
    while (state_num < length(selected_state)+1) {
      ## goes through each state
      selected_state_s<- selected_state[state_num]
      selected_state_file_path<- paste0(app_path,"/www/sp_lists_USDA/",selected_state_s,".csv")
      
      st_plant_data<- read_csv(selected_state_file_path)
      ## get species code name
      sp_codes_avaliable_by_state<- as.data.frame(unique(st_plant_data$symbol))
      names(sp_codes_avaliable_by_state) <- "code"
      ## get species code synomns to help catch old codes
      more_sp_codes_avaliable_by_state<- as.data.frame(unique(st_plant_data$synonym_symbol))
      names(more_sp_codes_avaliable_by_state) <- "code"
      
      final_sp_list_temp <- unique(rbind(sp_codes_avaliable_by_state, more_sp_codes_avaliable_by_state)) %>%
        arrange(code)
      
      final_sp_list <- rbind(final_sp_list, final_sp_list_temp)
      
      state_num = state_num+1
    }
    
    ## plant list of all codes in selected states from USDA website
    final_sp_list_temp <- unique(final_sp_list)
    
    ## merge vgs old codes to state list as well ->
    names(vgs_species_list_least)[1] <- "code"
    vgs_temp_w_state_codes <- unique(left_join(final_sp_list, vgs_species_list_least))
    
    vgs_temp_old_codes_only <- as.data.frame(vgs_temp_w_state_codes$NewSynonym)
    names(vgs_temp_old_codes_only)[1] <- "code"
    
    final_sp_list <- unique(rbind(final_sp_list, vgs_temp_old_codes_only)) %>% 
      filter(!is.na(code)) %>% 
      arrange(code)
    
    ## Check all species added
    sp_query <- paste0("SELECT DISTINCT PK_Species, SpeciesName from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
  where List = 'NRCS'")
    
    species_in_db <- dbGetQuery(mydb, sp_query)
    
    sp_in_data <- as.data.frame(unique(species_in_db$PK_Species))
    
    not_in_state <- setdiff(species_in_db$PK_Species,final_sp_list$code)
    
    ## get rid of allowed VGS 2 codes
    updated_not_in <- not_in_state[grep("^2", not_in_state, invert = T)]
    
    updated_not_in <- as.data.frame(updated_not_in)
    names(updated_not_in)[1]<- "PK_Species"
    
    ## plants in .db that do not show up in selected state USDA lists
    updated_not_in_2<- left_join(updated_not_in, vgs_species_list)
    
    ## add column so can merge and know its not in state lists
    if (nrow(updated_not_in_2) > 0) {
      updated_not_in_2[2] <- "Not in USDA list for CA, WA, OR, ID, or NV"
      names(updated_not_in_2)[2] <- "Species in USDA plant list?"
      
      sp_by_state_check_all <- left_join(species_count_w_siteInfo, updated_not_in_2)
      sp_by_state_check_all$`Species in USDA plant list?`[is.na(sp_by_state_check_all$`Species in USDA plant list?`)] <- "yes"
      names(sp_by_state_check_all)[8] <- "InUSDA_PlantList?"
      sp_by_state_check_all <- sp_by_state_check_all %>% 
        arrange(`InUSDA_PlantList?`)
      #View(sp_by_state_check_all)
      names(sp_by_state_check_all)[2] <- "UpdatedCode"
      names(sp_by_state_check_all)[6] <- "SampleHits"
      names(sp_by_state_check_all)[7] <- "SiteHits"
      
      write.xlsx(sp_by_state_check_all, paste0(app_path, "/www/Conflicts/sp_by_state_check_all.xlsx"))
      if (power_mode == TRUE) {
        file.show(paste0(app_path, "/www/Conflicts/sp_by_state_check_all.xlsx"))
      }
      
    }

    
    # # get siteID for species Not in USDA list
    # sp_find_siteID <- sp_by_state_check_all %>% 
    #   filter(`InUSDA_PlantList?` == "Not in USDA list for CA, WA, OR, ID, or NV")
    #   t=1
    #   
    #   temp_sp_find<- sp_by_state_check_all$PK_Species[t]
    #   temp_qualifier_find<- sp_by_state_check_all$SpeciesQualifier[t]
    #   # if no qualifier - use IS NULL
    #   if (is.na(temp_qualifier_find)) {
    #     find_site_q <- paste0("select DISTINCT SiteID, Protocol.Date, PK_Species, SpeciesQualifier from Protocol
    # INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
    # INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
    # INNER JOIN Site ON Site.PK_Site = Event.FK_Site
    # INNER JOIN AncestryCombinedPath ON AncestryCombinedPath.PK_Site = Site.PK_Site
    # INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
    # INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
    # where PK_Species = '",temp_sp_find,"' and SpeciesQualifier IS NULL")
    #   } else { # has a qualifier
    #     find_site_q <- paste0("select DISTINCT SiteID, Protocol.Date, PK_Species, SpeciesQualifier from Protocol
    # INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
    # INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
    # INNER JOIN Site ON Site.PK_Site = Event.FK_Site
    # INNER JOIN AncestryCombinedPath ON AncestryCombinedPath.PK_Site = Site.PK_Site
    # INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
    # INNER JOIN Species ON Species.PK_Species = Sample.FK_Species
    # where PK_Species = '",temp_sp_find,"' and SpeciesQualifier = '",temp_qualifier_find,"'")
    #   }
    #   
    #   found_sites<- dbGetQuery(mydb, find_site_q)
    

  }
}


if (power_mode == FALSE) {
  ## this works for data with EventNames having both frequency and ground cover
  ## Check for missing data for GC and Freq data
  events_check_query<- paste0("SELECT DISTINCT EventName from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Site ON Site.PK_Site = Event.FK_Site
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event")
  
  events_check <- dbGetQuery(mydb, events_check_query)
  
  if ("Frequency (by quadrat)" %in% events_check$`EventName` && "Point Ground Cover" %in% events_check$`EventName` ) {

    missing_data_query <- paste0("SELECT 
    quote(GC.PK_Protocol) as quoted_PK_Protocol, 
    GC.EventName AS GC_EventName, 
    GC.Transect, 
    GC.sampleCount AS GC_SampleCount,
    FQ.EventName AS FQ_EventName, 
    FQ.sampleCount AS FQ_SampleCount,
    GC.Date AS Protocol_Date,
    GC.siteID AS Site_ID
FROM 
    (SELECT Protocol.PK_Protocol, Protocol.Date, Site.siteID, EventName, Transect, Count(Sample.PK_Sample) as sampleCount 
     FROM Protocol
     INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
     INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
     INNER JOIN Site ON Site.PK_Site = Event.FK_Site
     INNER JOIN AncestryCombinedPath ON AncestryCombinedPath.PK_Site = Site.PK_Site
     INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
     WHERE EventName = 'Point Ground Cover'
     GROUP BY Protocol.PK_Protocol, Protocol.Date, Site.siteID, EventName, Transect
     HAVING sampleCount > 1) AS GC
LEFT JOIN 
    (SELECT Protocol.PK_Protocol, EventName, Transect, Count(Sample.PK_Sample) as sampleCount 
     FROM Protocol
     INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
     INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
     INNER JOIN Site ON Site.PK_Site = Event.FK_Site
     INNER JOIN AncestryCombinedPath ON AncestryCombinedPath.PK_Site = Site.PK_Site
     INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
     WHERE EventName = 'Frequency (by quadrat)'
     GROUP BY Protocol.PK_Protocol, EventName, Transect
     HAVING sampleCount > 1) AS FQ
ON GC.PK_Protocol = FQ.PK_Protocol AND GC.Transect = FQ.Transect
WHERE FQ.PK_Protocol IS NULL;")
    
    possible_missing_data <- dbGetQuery(mydb, missing_data_query)
    
    missing_data <- as.data.frame(possible_missing_data)
    missing_data <- missing_data %>% 
      select(Transect, Site_ID, Protocol_Date)
    
    write.xlsx(missing_data, paste0(app_path, "/www/Conflicts/possible_missing_data.xlsx"))
  }
  
}



# only create workbook when not in power_mode
if (power_mode == FALSE) {
  ## FINAL WB ----
  wb <- createWorkbook()
  # Add a worksheet to the workbook
  addWorksheet(wb, "SpeciesCountEval")
  # Write your data frame (df) to the worksheet
  writeData(wb, "SpeciesCountEval", species_count_w_siteInfo)
  # auto width columns ?
  setColWidths(wb, "SpeciesCountEval", cols = 1:ncol(species_count_w_siteInfo), widths = "auto")
  
  # highlightStyle <- createStyle(fgFill = "#FFFF00")  # Yellow fill
  # conditionalFormatting(wb, "SpeciesCounts", cols = 1:nrow(species_count), rows = 1:ncol(species_count), rule = "(H:H)>100", style = highlightStyle)
  
  addWorksheet(wb, "SpeciesLookupBySite")
  writeData(wb, "SpeciesLookupBySite", species_count_by_site)
  
  addWorksheet(wb, "FreqComparisons")
  writeData(wb, "FreqComparisons", freq_comp_check)
  
  addWorksheet(wb, "PossibleMissingData")
  writeData(wb, "PossibleMissingData", missing_data)
  
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
}

