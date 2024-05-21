## Queries the db for specific attributes and updates the settings
## specific to each forest


## Even Group update for USFS R6 - RR
if (ServerKey == "USFS R6-RR") {
  ## expect 5 belts, if more then need to update attributes from EventGroup/Form
  EG.att.query <- paste0("SELECT quote(PK_EventGroup), Count(DISTINCT Transect), SiteID from Protocol
  INNER JOIN EventGroup ON EventGroup.FK_Protocol = Protocol.PK_Protocol
  INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
  INNER JOIN Site ON Site.PK_Site = Event.FK_Site
  INNER JOIN Sample ON Sample.FK_Event = Event.PK_Event
  group by siteID, Protocol.Date
  having Count(DISTINCT Transect) > 5")
  
  EG_att_fromVGS <- dbGetQuery(mydb, EG.att.query)
  
  ## go through and update each EG that needs it
  go_go <- 1
  while (go_go <= nrow(EG_att_fromVGS)) {
    ## new attributes based on db data
    eventGroup.attributes <- paste0("<FormAttributes>
  <Level1>
    <ID>NumberOfTransects</ID>
    <Label>Number of Transects (minimum 1)</Label>
    <Value>",EG_att_fromVGS$`Count(DISTINCT Transect)`[go_go],"</Value>
    <Qualifier />
    <Visible>1</Visible>
  </Level1>
  <Level1>
    <ID>TransectLength</ID>
    <Label>Transect Length (feet or meters only)</Label>
    <Value>100.00</Value>
    <Qualifier>Feet</Qualifier>
    <Visible>1</Visible>
  </Level1>
  <Level1>
    <ID>BeltWidth</ID>
    <Label>Transect Width (feet or meters only, 0 = line)</Label>
    <Value>6.00</Value>
    <Qualifier>Feet</Qualifier>
    <Visible>1</Visible>
  </Level1>
  <Level1>
    <ID>TransectLabelList</ID>
    <Label>Transect Labels (comma delimited list)</Label>
    <Qualifier />
    <Visible>1</Visible>
  </Level1>
  <Level1>
    <ID>SamplesLayout</ID>
    <Label>Samples</Label>
    <LayoutType>Header</LayoutType>
    <Value />
    <Qualifier />
    <Visible>1</Visible>
    <Level2>
      <ID>SamplesPerTransect</ID>
      <Label>Samples Per Transect (minimum 1)</Label>
      <Parent>SamplesLayout</Parent>
      <Value>20</Value>
      <Qualifier />
      <Visible>1</Visible>
    </Level2>
    <Level2>
      <ID>SamplesDisplayStyle</ID>
      <Label>Samples Display Style</Label>
      <Parent>SamplesLayout</Parent>
      <ValueList>Sequence,Incremental Measurements,Specified Measurements,None</ValueList>
      <Value>Sequence</Value>
      <Qualifier />
      <Visible>1</Visible>
      <Level3>
        <ID>SamplesStartingPoint</ID>
        <Label>First Sample Measurement (if applicable)</Label>
        <Parent>SamplesDisplayStyle</Parent>
        <Value>0</Value>
        <Qualifier />
        <Visible>1</Visible>
        <LayoutType />
      </Level3>
      <Level3>
        <ID>SamplesSpecifiedPoints</ID>
        <Label>Specified Measurements (if applicable) - comma delimited list. Number of values should = samples per transect above.</Label>
        <Parent>SamplesDisplayStyle</Parent>
        <Value />
        <Qualifier />
        <Visible>1</Visible>
      </Level3>
    </Level2>
    <Level2>
      <ID>SampleActionsLayout</ID>
      <Label>Sample Actions</Label>
      <Parent>SamplesLayout</Parent>
      <LayoutType>Header</LayoutType>
      <Value />
      <Qualifier>Add Action</Qualifier>
      <Visible>1</Visible>
    </Level2>
  </Level1>
</FormAttributes>")
    
    update_EG_att_Q <- paste0("UPDATE EventGroup
    SET Attributes = '",eventGroup.attributes,"'
    WHERE PK_EventGroup = ",EG_att_fromVGS$`quote(PK_EventGroup)`[go_go])
    
    dbExecute(mydb, update_EG_att_Q)
    print(paste0("Updating Attributes for ",EG_att_fromVGS$SiteID[go_go]))
    go_go = go_go+1
    }
  
  }


  

  
  
  