

writeToCSVs <- function(tableList, timestamp) {
  if(!dir.exists(paste0("data/", timestamp))) {
    dir.create(paste0("data/", timestamp))
  }
  map2(tableList, names(tableList), function(x, nm) {
    fwrite(x, paste0("data/",timestamp, "/", nm, ".csv"))
  })

  paste0("data/",timestamp, "/", names(tableList), ".csv")
}


processHourlyFiles <- function(hourly_string) {

  readArchiveFile(hourly_string) %>%
    makeJSONList %>%
    lapply(getAllTables) %>%
    bindRowsByName %>%
    writeToCSVs(., timestamp = hourly_string) %>%
    lapply(saveFileToWasabi)

}

saveFileToWasabi <- function(filename) {
  aws.s3::put_object(object = filename,
                     file = filename,
                     bucket = "github-archive",
                     region = "")
}






