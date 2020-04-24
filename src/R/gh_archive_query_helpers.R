source("src/R/parse_gh_archive.R")

first_week_events <-
  readArchiveFile("2019-02-14-23") %>%
  makeJSONList

first_week_events %>%
  Filter(function(x) x$type == "IssuesEvent", .)

