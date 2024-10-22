#notes_update

## concatenate site name to notes section ->
site.notes.q <- paste0("UPDATE Site
SET Notes = (SELECT SiteID || ' - ' || Site.Notes
             FROM EventGroup
             INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
             INNER JOIN Site ON Site.PK_Site = Event.FK_Site
             WHERE Protocol.PK_Protocol = EventGroup.FK_Protocol)
WHERE EXISTS (SELECT 1
              FROM EventGroup
              INNER JOIN Event ON Event.FK_EventGroup = EventGroup.PK_EventGroup
              INNER JOIN Site ON Site.PK_Site = Event.FK_Site
              WHERE Protocol.PK_Protocol = EventGroup.FK_Protocol);")

dbExecute(mydb, site.notes.q)
