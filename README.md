## VGS-Batch-Importer

Shiny application that connects to a local VGS database (VGS50.db file – SQL database) and imports historical batch data from selected excel sheets. It can be adapted to be used for a variety of different digital data sheets that contain historical data in different formats. Basic structure works like so; reads in data, parses data based on keys set in script (e.g., “SiteID” to find site name) and selected inputs from UI, organizes data, connects to local SQL database, creates and executes SQL insert statements to create site, site metadata, locations for site, protocol for event data in site (which include event groups and events), then inserts sample data into created events. Various QA/QC checks are built in to prevent corrupt data from being inserted. Code can be seen at https://github.com/tgilbert14/VGS-Batch-Importer-App

App works as follows...
Run app and import data (Can run in Power Mode to get more info on data errors)
Check QA/QC logs and fix mistakes
Update data sheets if needed
Re-run app again with corrected sheets
Clean up folder and merge repeated sites
Download Folder Structure from server and place sites in correct folder
Run 'vgs_name_update.R' script in the Functions folder to create correct names (syncable folders only)
Sync Data as in pieces until all data is uploaded.
