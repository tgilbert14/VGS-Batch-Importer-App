---
editor_options: 
  markdown: 
    wrap: 72
---

## VGS-Batch-Importer

This R Shiny application was built as an ETL pipeline to ingest batch
historical excel datasheets into a SQLite database. [Generic Data
Sheets](www/Generic%20Sheets%20for%20Data%20Import/NestedFreq_GC_LineIntercept_HistoricalDataEntry_2025.xlsm)
were build with QA validations built in. This example allows for site,
point ground cover, line intercept, and nested frequency data for
multiple transects to be ingested for multiple sheets at a time.

This app is specifically set to connect to a local
[VGS](https://vgs.arizona.edu/) database. It can be adapted to be used
for a variety of different digital data sheets that contain historical
data in different formats. Basic structure works like so; reads data,
parses data based on keys set in script (e.g., "SiteID" to find site
name) and selected inputs from UI, organizes data, connects to local SQL
database, creates and executes SQL insert statements to create site,
site metadata, locations, protocols for event data (includes event
groups and events in each eventgroup), then inserts sample data into
each event. Various QA/QC checks are built in to prevent corrupt data
from being inserted. Code can be seen at
<https://github.com/tgilbert14/VGS-Batch-Importer-App>

## App View & Features

![*Main UI View*](assets/mainUI.jpg)

The app interface has options to select protocols available for ingest.
It has various options depending on the import:

-   [Power Mode] : This check box bypasses errors by generating and
    opening a excel workbook to review. The errors are then fixed and
    eventually the import happens with this setting turned off.

-   [Species Replace] : This check box enables a
    [SpeciesReplace.xlsx](www/SpeciesReplace.xlsx) file that can be used
    to mass update species codes for every file instead of going into
    each individual file and changing it individually. This is
    exceptionally helpful when a USDA code has had an update or a client
    uses the wrong code consistency.

-   Select Protocol for Import :

-   Batch Import Data :

-   [Species Count] :

-   [Species Check] :

-   Update Site Name :

-   🦐 :

-   Survey Log Input? :

#### Power Mode

![](assets/powerMode.jpg)

#### Species Replace

![](assets/speciesReplace.jpg)

#### Species Count

![](assets/speciesCount.jpg)

#### Species Check

![](assets/speciesCheck.jpg)

This app also generates a log text file stored in the 'www/' folder to
track code flow and debug import issues.

![*www/r_output.txt log*](assets/logExample.jpg)
