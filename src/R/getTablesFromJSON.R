# method for compiling tables from JSON
library(data.table)

#' Adds a prefix to a data.table
#' @param DT data.table to pass
#' @param prefix prefix to apply to column names
addPrefix <- function(DT, prefix) {
  setnames(DT,
           colnames(DT),
           paste0(prefix, colnames(DT)))
  DT
}

getTable <- function(event, type) {
  if(is.null(event[[type]])) {
    return(NULL)
  }
  tbl <- as.data.table(event[[type]])
  tbl <- addPrefix(tbl, paste0(type, "_"))
  tbl
}

getActorTable <- function(event) {
  getTable(event, "actor")
}

getOrgTable <- function(event) {
  getTable(event, "org")
}

getRepoTable <- function(event) {
  getTable(event, "repo")
}

getParentTable <- function(event) {
  data.table(
    event_id = event$id,
    event_type = event$type,
    event_public = event$pubic,
    event_created_at = event$created_at
  )
}

# this needs to change by event type
getPayloadTable <- function(event) {
  do.call(
    paste0("get",
           event$type,
           "Payload"),
    args = list(
      payload = event$payload
    )
  )
}


# parse payload for create event
getCreateEventPayload <- function(payload) {
  payload <- as.data.table(payload)
  payload <- addPrefix(payload, "create_")
  payload
}

# return of all the tables from the event
getAllTables <- function(event) {
  list(
    EventTable = getParentTable(event),
    ActorTable = getActorTable(event),
    OrgTable = getOrgTable(event),
    RepoTable = getRepoTable(event),
    PayloadTable = getPayloadTable(event)
  )
}


