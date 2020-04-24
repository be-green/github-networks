library(jsonlite)
library(magrittr)


readArchiveFile <- function(timestamp) {
  # temporary directory to download to
  tmpdir <- tempdir()

  # name of file to download to
  destfile <- paste0(tmpdir,"\\",timestamp,".json.gz")

  # url to download from
  file_url <- makeArchiveQueryString(timestamp)
  download.file(file_url,
                destfile = destfile,
                method = "curl",
                quiet = T)

  con <- gzfile(destfile)
  lines <- readLines(con)
  close(con)

  # delete tempfile
  unlink(destfile)

  lines
}

makeJSONList <- function(archiveFileLines) {
  lapply(archiveFileLines, jsonlite::fromJSON)
}

parseResult <- function(result) {

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

#' List all hours on a day
#' for purposes of query GH Archive
listAllHours <- function(date) {
  paste0(date,"-", 0:23)
}

#' Convenience function for seq.Date
listAllDates <- function(start, end) {
  seq(as.Date(start),
      as.Date(end), by="days")
}

# Turn
makeArchiveQueryString <- function(timestamp) {
  paste0("https://data.gharchive.org/",
         timestamp,
         ".json.gz")
}

getObjSize <- function(obj, units = "Gb") {
  format(object.size(obj), units = units, digits = 7)
}


readArchiveFiles <- function(timestamps) {
  lapply(timestamps, readArchiveFile)
}

parseResult <- function(jsonResult, type) {
  do.call(paste0(parse,stringr::str_to_title(type)),
          jsonObj = jsonResult)
}

