## not done yet...
# ## look for species label
# find_sp_data<- grep("Species",historical_raw_data$`Nested Frequency Data`)
# data<- historical_raw_data[c(find_sp_data+2:nrow(historical_raw_data)),c(2:ncol(historical_raw_data))]
#
# nested_freq_data<<- data[c(1:find_gc_data-1),] %>%
#   filter(!is.na(`Nested Frequency Data`))
# ## change all NAs to zero??
# #View(nested_freq_data)
#
# find_gc_data<<- grep("Ground Cover",data$`Nested Frequency Data`)
# gc_data<- data[c(find_gc_data:nrow(data)),] %>%
#   filter(!is.na(`Nested Frequency Data`))