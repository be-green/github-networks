source("src/R/parse_gh_archive.R")

first_week_events <-
  readArchiveFile("2020-02-14-23") %>%
  makeJSONList

subsample <- first_week_events[
  sample(length(first_week_events), 100)]


events <- first_week_events %>%
  lapply(function(x) x$type) %>%
  unique %>%
  unlist


grab_first_event <- function(event, data) {
  Filter(function(x) x$type == event, data)[[1]]
}

top_events <- lapply(events, grab_first_event,
                     data = first_week_events)


start <- proc.time()
parsed_events <- lapply(first_week_events,
         function(x) {
           getAllTables(x)
           })
end <- proc.time()

end - start

