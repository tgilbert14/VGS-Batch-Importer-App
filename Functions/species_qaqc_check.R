## read in output log
qaqc<-read_csv(paste0(app_path,"/www/r_output.txt"))
#View(qaqc)

sp_conflicts<- unique(qaqc[grep("Species", qaqc[[1]]),])

names(sp_conflicts)[1]<- "Species errors in VGS"
if (nrow(sp_conflicts) == 0) {
  sp_conflicts<- "No species conflicts found"
}

#time<- substr(Sys.time(),0,nchar(Sys.time())-3)
write.xlsx(sp_conflicts, paste0(app_path,"/www/Conflicts/r_sp_conflicts_",site_name,".xlsx"))
## open file
file.show(paste0(app_path,"/www/Conflicts/r_sp_conflicts_",site_name,".xlsx"))
#file.show(paste0(app_path,"/www/r_output.txt"))

## GC counts
file_sum<- unique(qaqc[grep("Moving to File", qaqc[[1]]),])
gc_sum<- unique(qaqc[grep("ground cover points found", qaqc[[1]]),])
gc_check<- rbind(file_sum,gc_sum)
write.xlsx(gc_check, paste0(app_path,"/www/Conflicts/gc_counts_",site_name,".xlsx"))
## open file
file.show(paste0(app_path,"/www/Conflicts/gc_counts_",site_name,".xlsx"))
