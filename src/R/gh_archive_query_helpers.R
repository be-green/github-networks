

first_week_events <-
  listAllDates("2020-02-01", "2020-02-14") %>%
  listAllHours() %>%
  readArchiveFiles()

