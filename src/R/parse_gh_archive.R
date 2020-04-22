library(httr)
library(jsonlite)

tmpdir <- tempdir()

download.file("https://data.gharchive.org/2015-01-01-15-0.json.gz",
              destfile = tmpfile)

unzip(tmpfile)

all <- readLines("C:\\Users\\heroo\\AppData\\Local\\Temp\\RtmpGEnzS7\\file12b0331f499~\\file12b0331f499~")

first_result <- all[[2]] %>%
  jsonlite::fromJSON()

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

all <- all %>%
  lapply(jsonlite::fromJSON) %>%
  lapply(parseResult) %>%
  rbindlist(idcol = T, fill = T)

