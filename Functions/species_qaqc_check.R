## read in output log
qaqc<-read_csv(paste0(app_path,"/www/r_output.txt"))
#View(qaqc)

sp_conflicts<- unique(qaqc[grep("Species", qaqc$`[1] " "`),])
names(sp_conflicts)[1]<- "Species errors in VGS"
if (nrow(sp_conflicts) == 0) {
  sp_conflicts<- "No species conflicts found"
}
write.xlsx(sp_conflicts, paste0(app_path,"/www/r_sp_conflicts.xlsx"))
## open file
file.show(paste0(app_path,"/www/r_sp_conflicts.xlsx"))
#file.show(paste0(app_path,"/www/r_output.txt"))
