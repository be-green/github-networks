source("src/R/setup.R")
source("src/R/parse_gh_archive.R")
source("src/R/generalize_parser.R")
source("src/R/getTablesFromJSON.R")
source("src/R/dataPipeline.R")

library(parallel)

cl <- parallel::makeCluster(2)

parallel::clusterEvalQ(cl, {
  source("src/R/setup.R")
  source("src/R/parse_gh_archive.R")
  source("src/R/generalize_parser.R")
  source("src/R/getTablesFromJSON.R")
  source("src/R/dataPipeline.R")
})

in_bucket <- aws.s3::get_bucket("github-archive",
                                region = "") %>%
  sapply(function(x) x$Key %>%
           str_extract("/(.*?)/") %>%
           str_replace_all("/",""))

date_sequence <- listAllDates("2020-01-01", Sys.Date()) %>%
  lapply(listAllHours) %>%
  unlist %>%
  setdiff(in_bucket)

parallel::parLapply(
  cl,
  tail(date_sequence, 2),
  processHourlyFiles
)
