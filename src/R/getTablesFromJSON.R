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

getEventTable <- function(event) {
  data.table(
    event_id = event$id,
    event_type = event$type,
    event_public = event$pubic,
    event_created_at = event$created_at,
    actor_id = event$actor$id,
    org_id = event$org$id,
    repo_id = event$repo$id
  )
}

#' Dispatches relevant payload parser based on event type
#' @param event item in list of JSON responses converted by jsonlite
getPayloadTable <- function(event) {
  do.call(
    paste0("get",
           event$type,
           "Payload"),
    args = list(
      payload = event$payload,
      event_id = event$id
    )
  )
}


# parse payload for create event
getCreateEventPayload <- function(payload, event_id) {
  payload <- as.data.table(payload, event_id)
  payload <- addPrefix(payload, "create_")
  payload
}


# parse payload for create event
getPullRequestEventPayload <- function(payload, event_id) {
  browser()
}


# return of all the tables from the event
getAllTables <- function(event) {
  list(
    EventTable = getEventTable(event),
    ActorTable = getActorTable(event),
    OrgTable = getOrgTable(event),
    RepoTable = getRepoTable(event),
    PayloadTable = getPayloadTable(event)
  )
}


