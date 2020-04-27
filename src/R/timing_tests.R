source("src/R/setup.R")
source("src/R/parse_gh_archive.R")
source("src/R/generalize_parser.R")
source("src/R/getTablesFromJSON.R")
source("src/R/dataPipeline.R")

cl <- parallel::makeCluster(detectCores())

parallel::clusterEvalQ(cl, {
  source("src/R/setup.R")
  source("src/R/parse_gh_archive.R")
  source("src/R/generalize_parser.R")
  source("src/R/getTablesFromJSON.R")
  source("src/R/dataPipeline.R")

})

date_sequence <- listAllDates("2019-12-31",
                              "2020-04-26") %>%
  lapply(listAllHours) %>%
  unlist

parallel::parLapply(
  cl,
  date_sequence,
  processHourlyFiles
)
