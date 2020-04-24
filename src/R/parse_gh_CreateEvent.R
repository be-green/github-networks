library(jsonlite)
library(magrittr)
library(data.table)

#Date: 4/23/20
#Description: Take in a single entry that
# has type = "CreateEvent" and parse it into a data table
# Next goal is to parse every entry with
# "CreateEvent" into a data table

source("src/R/parse_gh_archive.R")

first_week_events <-
  readArchiveFile("2019-02-14-23") %>%
  makeJSONList

create_events <-
  Filter(function(x) x$type == "CreateEvent",
         first_week_events)

#' Adds a prefix to a data.table
#' @param DT data.table to pass
#' @param prefix prefix to apply to column names
addPrefix <- function(DT, prefix) {
  setnames(DT,
           colnames(DT),
           paste0("actor_", colnames(DT)))
}

#' Parses a CreateEvent returned from the
#' github API
#' @param result CreateEvent result from the API
#' @return returns a data.table that parsed the event
parseCreateEvent <- function(result) {

  actorTable <- as.data.table(result$actor)
  addPrefix(actorTable, "actor_")

  payloadTable <- as.data.table(result$payload)
  addPrefix(payloadTable, "payload_")

  repoTable <- as.data.table(result$repo)
  addPrefix(repoTable, "repo_")

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

# should have 9281 rows
create_events_table <-
  lapply(create_events, parseCreateEvent) %>%
  rbindlist(fill = T)
