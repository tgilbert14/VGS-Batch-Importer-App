##locatorSyncState_update
##updating SyncKEY, not syncSTATE

# query .db for highest site.SyncState
siteSyncCheck <- paste0("SELECT MAX(SyncKey) FROM locator")
site_sync.data <- dbGetQuery(mydb, siteSyncCheck)

# update locator for that site.SyncState +1 for locator.SyncState
locator_update <- paste0("Update locator
Set SyncKey = ",site_sync.data$`MAX(SyncKey)`,"
Where PK_Locator IS NOT NULL
And DDLat IS NOT NULL
And DDlong IS NOT NULL")
dbExecute(mydb, locator_update)
