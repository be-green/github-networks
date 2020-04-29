source("src/R/setup.R")
source("src/R/parse_gh_archive.R")
source("src/R/generalize_parser.R")
source("src/R/getTablesFromJSON.R")
source("src/R/dataPipeline.R")

library(parallel)

in_bucket <- aws.s3::get_bucket("github-archive",
                                region = "", max = Inf) %>%
  sapply(function(x) x$Key %>%
           str_extract("/(.*?)/") %>%
           str_replace_all("/","")) %>%
  unique

date_sequence <- listAllDates("2020-03-01", Sys.Date()) %>%
  lapply(listAllHours) %>%
  unlist %>%
  setdiff(in_bucket)

cl <- parallel::makeCluster(16)

parallel::clusterEvalQ(cl, {
  source("src/R/setup.R")
  source("src/R/parse_gh_archive.R")
  source("src/R/generalize_parser.R")
  source("src/R/getTablesFromJSON.R")
  source("src/R/dataPipeline.R")
})

parallel::parLapply(
  cl,
  date_sequence,
  processHourlyFiles
)

parallel::stopCluster(cl)


processHourlyFiles(date_sequence[2])

