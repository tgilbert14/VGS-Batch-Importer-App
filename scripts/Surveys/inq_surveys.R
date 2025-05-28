## inserting inquiry data in ind datum

#data_import

## get pk's for inq datum inserts
query.inq.pks <- paste0("Select quote(PK_Inquiry), InquiryText from Inquiry
                        WHERE FK_Event = ",event_PK)
inq.info<- dbGetQuery(mydb, query.inq.pks)

View(inq.info)

# insert_data(data = hi2, method = "GC", Transect = belt_numbers, 
#             SampleNumber = SampleNumber_raw, Element = Element_raw, 
#             FK_Event = checked_PK_Event_gc, SyncKey = 33, SyncState = 1)


stop("in progress inq_survyes.R")