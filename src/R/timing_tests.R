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

check_updates <- function(date_sequence) {
  date_sequence[
    which(
      paste0("data/", date_sequence, "/ActorTable.csv") %>%
        sapply(function(x){
          suppressMessages({

            aws.s3::object_exists(object = x, bucket = "github-archive", region = "")

          })
        })
    )
  ]
}

new_stuff <- check_updates(date_sequence)

in_bucket <- unique(c(in_bucket, new_stuff))

date_sequence <- listAllDates("2020-01-01", Sys.Date()) %>%
  lapply(listAllHours) %>%
  unlist %>%
  setdiff(in_bucket)

qplot(as.Date(date_sequence), geom = "bar")

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

