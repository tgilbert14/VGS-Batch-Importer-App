##locatorSyncState_update
##updating SyncKEY, not syncSTATE

## this should be done after sites moved over to syncable...

# query .db for highest SyncKey
siteSyncCheck <- paste0("SELECT MAX(Key) FROM SyncTracking
  where Status = 'LastTransaction'")
site_sync.data <- dbGetQuery(mydb, siteSyncCheck)

# update locator for that max SyncKey +1 for locator.SyncKey
locator_update <- paste0("Update locator
Set SyncKey = ",as.numeric(site_sync.data$`MAX(Key)` +1),"
Where PK_Locator IS NOT NULL
And DDLat IS NOT NULL
And DDlong IS NOT NULL")
dbExecute(mydb, locator_update)
