## Sample Table Insert Guide based on Method
## Selects an insert statement based on method

## Cover Methods ----
if (method == "Cover/Frequency by quadrat") {
  ## Cover/Frequency by quadrat
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", FieldSymbol, ",", nValue, ")"
    ## nValue = Numeric % Entry for Cover/Frequency
  )
}
if (method == "Line Intercept") {
  ## Line Intercept
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue, nValue2, nValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ",",
    nValue2, ",", nValue3, ")"
  )
  ## nValue = Numeric Start on Line Intercept
  ## nValue2 = Numeric End on Line Intercept
  ## nValue3 = Numeric Length (automatically calculated)
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
}
if (method == "Point Ground Cover") {
  ## Point Ground Cover
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## Element = Always 1
  ## nValue = Always 1
}
if (method == "Point Intercept") {
  ## Point Intercept (Has Global Qualifier and Global Identifier Options)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, SubElement, FieldSymbol, cParameter, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", SubElement, ",",
    FieldSymbol, ",", cParameter, ",", nValue, ")"
  )
  ## Element = Always 1
  ## SubElement = Order entered on Point Intercept, first (top) entry is 1 (e.g., 1, 2, 3, 4, 5, etc.)
  ## cParameter = Based on Global Qualifier typed into settings (e.g., ‘Dead’)
  ## nValue = Always 1
}


## Density Methods ----
if (method == "Density") {
  ## Density (Has Global Qualifier Option)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, cParameter, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",",
    cParameter, ",", nValue, ")"
  )
  ## Element = Always 1
  ## cParameter = Based on Global Qualifier typed into settings (e.g., ‘yes’)
  ## cParameter2 = Based on Global Qualifier typed into settings (e.g., ‘no’)
  ## cParameter3 = Based on Global Qualifier typed into settings (e.g., ‘maybe’)
  ## nValue = Numeric Count for Density
}


## Frequency Methods ----
if (method == "Frequency (by quadrat)") {
  ## Frequency (by quadrat)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (Insert into Sample (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## Element = Numeric Nested Frame of Species in Frequency (e.g., 1, 2, 3, or 0)
  ## 0 = entire frame size (e.g., 40x40cm)
  ## 1 = smallest nested frame if available (e.g., 5x5cm)
  ## 2 = next smallest nested frame if available (e.g., 10x10cm)
  ## 3 = next smallest nested frame if available (e.g., 20x20cm)
  ## nValue = Always 1
}


## Qualitative Methods ----
if (method == "Exclusive Checklist") {
  ## Exclusive Checklist (Has Global Qualifier Option)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, cParameter) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", cParameter, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## cParameter = Based on Global Qualifier typed into settings (e.g., ‘mature’)
}
if (method == "Generic Values") {
  ## Generic Values (Has Global Qualifier Option)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue. nValue2, nValue3, cValue, cValue2, cValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ",",
    nValue2, ",", nValue3, ",", cValue, ",", cValue2, ",", cValue3, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry
  ## nValue2 = Numeric entry
  ## nValue3 = Numeric entry
  ## cValue = Character entry
  ## cValue2 = Character entry
  ## cValue3 = Character entry
}

if (method == "Item Qualities") {
  ## Item Qualities
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, FieldSymbol, nValue, nValue2, nValue3, cValue, cValue2, cValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", FieldSymbol, ",", nValue, ",", nValue2, ",",
    nValue3, ",", cValue, ",", cValue2, ",", cValue3, ")"
  )
  ## nValue = Numeric entry
  ## nValue2 = Numeric entry
  ## nValue3 = Numeric entry
  ## cValue = PK_Species from list selection
  ## cValue2 = PK_Species from list selection
  ## cValue3 = PK_Species from list selection
}
if (method == "Items List") {
  ## Items List
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## Element = Always 0
  ## nValue = Always 1
}


## Residue Methods ----
if (method == "Landscape Appearance/Key Forage") {
  ## Landscape Appearance/Key Forage
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, Transect, SampleNumber, Element, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", Transect, ",", SampleNumber, ",",
    Element, ",", nValue, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry
}
if (method == "Stubble Height") {
  ## Stubble Height
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry
}


## Structure Methods ----
if (method == "Dimension/Measurement") {
  ## Dimension/Measurement (Global Qualifier Options)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, cParameter, cParameter2, cParameter3, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",",
    cParameter, ",", cParameter2, ",", cParameter3, ",", nValue, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## cParameter = Based on Global Qualifier typed into settings (e.g., ‘yes’)
  ## cParameter2 = Based on Global Qualifier typed into settings (e.g., ‘no’)
  ## cParameter3 = Based on Global Qualifier typed into settings (e.g., ‘maybe’)
  ## nValue = Numeric entry for measurement
}
if (method == "Fetch") {
  ## Fetch
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry for fetch distance
}
if (method == "Gap Intercept") {
  ## Gap Intercept
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, FieldSymbol, nValue, nValue2) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", FieldSymbol, ",", nValue, ",", nValue2, ")"
  )
  ## FK_Species/FieldSymbol
  ## If basal gap, PK_Species/FieldSymbol = SYS_BGAP
  ## If canopy gap, PK_Species/FieldSymbol = SYS_CGAP
  ## nValue = Numeric start of gap
  ## nValue2 = Numeric end of gap
}
if (method == "Robel Pole/VOM") {
  ## Robel Pole/VOM
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry
}


## Tally/Historical Data Entry Methods ----
if (method == "Point Ground Cover (by tally)") {
  ## Point Ground Cover (by tally)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## SampleNumber = Always 1
  ## Element = Always 0
  ## nValue = Numic summary of Point Ground Cover hits
}
if (method == "Frequency (by tally)") {
  ## Frequency (by tally)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## SampleNumber = Always 1
  ## Element = Always 0
  ## nValue = Numic summary of Frequency hits
}
if (method == "Fetch Summary") {
  ## Fetch Summary
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## FK_Species/FieldSymbol
  ## Asymmetry = STAT_ASYMM
  ## Average = STAT_AVG
  ## Count = STAT_COUNT
  ## Maximum = STAT_MAX
  ## Median = STAT_MED
  ## Minimum = STAT_MIN
  ## SE = STAT_SE
  ## SampleNumber = Always 1
  ## Element = Always 0
  ## nValue = Numic summary for fetch specifics
}
if (method == "DWR (by tally)") {
  ## DWR (by tally)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue, nValue2) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ",",
    nValue2, ")"
  )
  ## Transect = Always 0
  ## SampleNumber = Always 1
  ## Element = Always 0
  ## nValue = Numic summary of DWR hits for specific rank (nValue2)
  ## nValue2 = Rank linked to summary (nValue) (e.g., 1, 2, or 3)
}


## Utilization Methods ----
if (method == "Grazed class") {
  ## Grazed class -->
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry
}
if (method == "Ocular Estimates") {
  ## Ocular Estimates
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry
}


## Weight Methods ----
if (method == "Clipping") {
  ## Clipping (Climate % Options) -->
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue2, nValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue2, ",",
    nValue3, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue2 = Numeric entry for Clipped Field Wt.
  ## nValue3 = Numeric entry for Clipped Dry Wt.
  ##
  ## Climate Options saved in another entry
  method_data_insert_2 <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, cParameter, cValue, cValue2, cValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",",
    cParameter, ",", cValue, ",", cValue2, ",", cValue3, ")"
  )
  ## Transect = Always 0
  ## SampleNumber = Always 0
  ## Element = Always 000001
  ## cParameter = Numeric entry for % Climate
  ## cValue = Numeric entry for % Dry Wt.
  ## cValue2 = Numeric entry for % Current Growth Ungrazed
  ## cValue3 = Numeric entry for % Growth Completed
}
if (method == "Comparative Yield") {
  ## Comparative Yield (Climate % Options) -->
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue, nValue2, nValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ",",
    nValue2, ",", nValue3, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry for Rank
  ## nValue2 = Numeric entry for Clipped Field Wt.
  ## nValue3 = Numeric entry for Clipped Dry Wt.
  ##
  ## Climate Options saved in another entry
  method_data_insert_2 <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, cParameter, cValue, cValue2, cValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",",
    cParameter, ",", cValue, ",", cValue2, ",", cValue3, ")"
  )
  ## Transect = Always 0
  ## SampleNumber = Always 0
  ## Element = Always 000001
  ## cParameter = Numeric entry for % Climate
  ## cValue = Numeric entry for % Dry Wt.
  ## cValue2 = Numeric entry for % Current Growth Ungrazed
  ## cValue3 = Numeric entry for % Growth Completed
}
if (method == "Double-Sampling") {
  ## Double-Sampling (Climate % Options) -->
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue, nValue2, nValue3) VALUES (Insert into Sample (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ",",
    nValue2, ",", nValue3, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry for Est. Wt.
  ## nValue2 = Numeric entry for Clipped Field Wt.
  ## nValue3 = Numeric entry for Clipped Dry Wt.
  ##
  ## Climate Options saved in another entry
  method_data_insert_2 <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, cParameter, cValue, cValue2, cValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",",
    cParameter, ",", cValue, ",", cValue2, ",", cValue3, ")"
  )
  ## Transect = Always 0
  ## SampleNumber = Always 0
  ## Element = Always 000001
  ## cParameter = Numeric entry for % Climate
  ## cValue = Numeric entry for % Dry Wt.
  ## cValue2 = Numeric entry for % Current Growth Ungrazed
  ## cValue3 = Numeric entry for % Growth Completed
}
if (method == "Dry Weight Rank (by quadrat)") {
  ## Dry Weight Rank (by quadrat) -->
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue, nValue2, nValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ",",
    nValue2, ",", nValue3, ")"
  )
  ## Element = Always 0
  ## nValue = Numeric entry for Rank (e.g., 1, 2, or 3)
}
if (method == "Weight-Unit Sampling") {
  ## Weight-Unit Sampling (Climate % Options)
  method_data_insert <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, nValue, nValue2) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",", nValue, ",",
    nValue2, ")"
  )
  ## Element = Goes up one for every species entered (e.g., 000001, 000002, 000003, etc.)
  ## nValue = Numeric entry for #Units during collection
  ## nValue2 = Numeric entry for Clipped Field Wt.
  ##
  ## Weight Unit info and Climate Options saved in another entry
  method_data_insert_2 <- paste0(
    "Insert into Sample (PK_Sample, FK_Event, FK_Species, Transect, SampleNumber, Element, FieldSymbol, cParameter, nValue. nValue2, nValue3, cValue, cValue2, cValue3) VALUES (",
    PK_Sample, ",", FK_Event, ",", FK_Species, ",", Transect, ",",
    SampleNumber, ",", Element, ",", FieldSymbol, ",",
    cParameter, ",", nValue, ",", nValue2, ",", nValue3, ",",
    cValue, ",", cValue2, ",", cValue3, ")"
  )
  ## Transect = Always 0
  ## SampleNumber = Always 0
  ## Element = Always 000001
  ## cParameter = Numeric entry for % Climate
  ## nValue = Numeric entry for #Units Standards (pop-up data entry)
  ## nValue2 = Numeric entry for Clipped Field Wt.
  ## nValue3 = Numeric entry for Clipped Dry Wt.
  ## cValue = Numeric entry for % Dry Wt.
  ## cValue2 = Numeric entry for % Current Growth Ungrazed
  ## cValue3 = Numeric entry for % Growth Completed
}

