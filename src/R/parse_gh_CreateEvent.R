library(jsonlite)
library(magrittr)
library(data.table)
library(foreach)

#Date: 4/23/20
#Description: Take in a single entry that has type ="CreateEvent" and parse it into a data table
#Next goal is to parse every entry with "CreateEvent" into a data table




List_first_week <-makeJSONList(first_week_events)

CreateEvents <- Filter(function(x) x$type == "CreateEvent", List_first_week)


sapply(testm, function(x) x$type) %>% unique


 n <- length(CreateEvents)

foreach i in n {


}
CreateEvent <- checkRunEvent[[1]]


CreateEvent$


Actors <- CreateEvent$actor


parseCreateEvent <- function(result) {

  actorTable <- as.data.table(result$actor)
  payloadTable <- as.data.table(result$payload)
  repoTable <- as.data.table(result$repo)

  data.table(
    id = result$id,
    type = result$type,
    public = result$public,
    created_at = result$created_at,
    actorTable,
    payloadTable,
    repoTable
  )
}


CreateEventTable <- lapply(parseCreateEvent, CreateEvents)

CreateEventTable$created_at
