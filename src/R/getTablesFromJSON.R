# method for compiling tables from JSON


#' Adds a prefix to a data.table
#' @param DT data.table to pass
#' @param prefix prefix to apply to column names
addPrefix <- function(DT, prefix) {
  setnames(DT,
           colnames(DT),
           paste0("actor_", colnames(DT)))
}

getTable <- function(event, type) {
  tbl <- as.data.table(result[[type]])
  addPrefix(tbl, paste0(type, "_"))
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

getAllTables <
