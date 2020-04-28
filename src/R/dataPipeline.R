

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

  logfile <- paste0("logs/", hourly_string, ".log")
  writeLines(paste0("processing ", hourly_string), paste0("logs/", hourly_string, ".log"))
  message("Processing ", hourly_string)

  log <- function(x, message, file) {
    write(message, file, append = T)
    x
  }

  hourly_string %>%
    log("making list",file=logfile) %>%
    readArchiveFile() %>%
    log("reading file",file=logfile) %>%
    makeJSONList %>%
    log("getting tables",file=logfile) %>%
    lapply(getAllTables) %>%
    log("binding rows",file=logfile) %>%
    bindRowsByName %>%
    log("writing tables",file=logfile) %>%
    writeToCSVs(., timestamp = hourly_string) %>%
    log("saving to cloud",file=logfile) %>%
    lapply(saveFileToWasabi)

  write("done.",file=logfile,append=TRUE)

}
saveFileToWasabi <- function(filename) {
  aws.s3::put_object(object = filename,
                     file = filename,
                     bucket = "github-archive",
                     region = "")
}






