# method for compiling tables from JSON
library(data.table)
source("src/R/generalize_parser.R")

#' Adds a prefix to a data.table
#' @param DT data.table to pass
#' @param prefix prefix to apply to column names
addPrefix <- function(DT, prefix) {
  setnames(DT,
           colnames(DT),
           paste0(prefix, colnames(DT)))
  DT
}

#' Get table from event with associated label
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
  if(length(event$payload) == 0) {
    return(NULL)
  }
  node_table(
      node = event$payload,
      parent_id = event$id,
      nodename = event$type,
      parent_name = "event"
  ) %>%
    get_data(name = event$type)
}

# parse payload for create event
getPullRequestEventPayload <- function(payload, event_id) {
  browser()
}

# return of all the tables from the event
getAllTables <- function(event) {
  dataTables <- getPayloadTable(event)

  c(
    list(
    EventTable = getEventTable(event),
    ActorTable = getActorTable(event),
    OrgTable = getOrgTable(event),
    RepoTable = getRepoTable(event)
    ),
    dataTables
  )
}


bindRowsByName <- function(returnList) {
  nms <- sapply(returnList, names) %>% unlist %>% unique
  l <- lapply(nms,
         function(name, dataList) {
           rbindlist(dataList[which(names(dataList) == name)], fill = T)
         },
  dataList = rlang::flatten(returnList))

  names(l) <- nms

  l
}



