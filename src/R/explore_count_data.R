library(data.table)
library(ggplot2)
library(magrittr)
library(stringr)
library(anytime)


event_data <- list.files("data/aggregated-data/events-by-hour/",
                         pattern = "*.csv$",
                         full.names = T) %>%
  lapply(function(x) {
    gc()
    fread(x)
  }) %>%
  rbindlist(fill = T)

orgs <- list.files("data/aggregated-data/org-table/",
                   pattern = "*.csv$",
                   full.names = T) %>%
  lapply(function(x) {
    gc()
    fread(x)
  }) %>%
  rbindlist(fill = T)

event_data[,datetime := date]
event_data[,date := anydate(datetime, calcUnique = T)]

event_data <- event_data[date >= as.Date("2020-01-01")]


to_posix <- function(date, hour) {
  as.POSIXct(
    paste0(date, " ", hour, ":00:00")
  )
}

event_data[,.(TotalEvent = sum(num, na.rm = T)), by = c("hour",
                                                        "date")] %>%
  .[,WeekDay := factor(weekdays(date),
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[,PostCovid := fifelse(
    date > as.Date("2020-03-04"),
    "WFH",
    "Not WFH")] %>%
  ggplot(., aes(x = hour, y = TotalEvent,
             color = PostCovid)) +
  geom_smooth(span = 0.3) +
  facet_wrap(~WeekDay)


event_data[,AvgActorEvents := mean(num),
           by = c("hour", "WeekDay",
                  "actor_id")]

event_data[,.(TotalEvent = sum(num - AvgActorEvents, na.rm = T)),
           by = c("hour",
                  "date")] %>%
  .[,isWeekend := fifelse(weekdays(date) %in% c("Saturday",
                                         "Sunday"),
    "Weekend",
    "Weekday")] %>%
  .[,PostCovid := fifelse(
    date > as.Date("2020-03-04"),
    "Post-Covid",
    "Pre-Covid")] %>%
  ggplot(., aes(x = hour, y = TotalEvent,
                color = PostCovid)) +
  geom_smooth() +
  facet_wrap(~isWeekend + PostCovid)

event_data[,TotalActorEvents := sum(num), by = actor_id]


event_data[,.(TotalEvent = sum(num - AvgActorEvents, na.rm = T)),
           by = c("hour",
                  "date")] %>%
  .[,WeekDay := factor(weekdays(date),
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[,PostCovid := fifelse(
    date > as.Date("2020-03-04"),
    "WFH",
    "Not WFH")] %>%
  ggplot(., aes(x = hour, y = TotalEvent,
                color = PostCovid)) +
  geom_smooth(span = 0.3) +
  facet_wrap(~WeekDay)


event_data[,.(TotalActorEvents = sum(num)), by = actor_id] %>%
  ggplot(aes(x = TotalActorEvents)) +
  geom_histogram(bins = 50) +
  scale_x_log10(labels = scales::comma) +
  labs(y = "Frequency", x = "") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Number of Events by Person",
          subtitle = "Over the Last 6 Months") +
  theme_minimal()

ggsave("figures/summary-stats/total-events-by-person.png",
       dpi = 300,
       width = 7,
       height = 4)

# "Active" might mean more than 40 total actions over 6 months?
# these actions make up more than half the sample (37 million event-hours)
event_data <- event_data[TotalActorEvents > 40]

event_data[,.(TotalEvent = sum(num, na.rm = T)),
           by = c("hour",
                  "date")] %>%
  .[,WeekDay := factor(weekdays(date),
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[,PostCovid := factor(fifelse(
    date > as.Date("2020-03-04"),
    "Post-Covid",
    "Pre-Covid"),
    levels = c("Pre-Covid",
               "Post-Covid"))] %>%
  ggplot(., aes(x = hour, y = TotalEvent)) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(span = 0.5) +
  facet_wrap(~WeekDay) +
  labs(color = NULL, x = "Hour", y = "log(# of Events)") +
  ggtitle("Average Activity by Hour and Weekday",
          subtitle = "Smoothed with loess regression")

ggsave("figures/summary-stats/events-by-hour-and-weekday.png",
       dpi = 300,
       width = 7,
       height = 7)



event_data[,.(TotalEvent = sum(num, na.rm = T)),
           by = c("hour",
                  "date")] %>%
  .[,WeekDay := factor(weekdays(date),
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[,PostCovid := factor(fifelse(
    date > as.Date("2020-03-04"),
    "Post-Covid",
    "Pre-Covid"),
    levels = c("Pre-Covid",
               "Post-Covid"))] %>%
  ggplot(., aes(x = hour, y = TotalEvent,
                color = PostCovid)) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(span = 0.5) +
  facet_wrap(~WeekDay) +
  labs(color = NULL, x = "Hour", y = "log(# of Events)") +
  ggtitle("Average Activity by Hour and Weekday, Pre and Post Covid",
          subtitle = "Smoothed with loess regression, COVID based on 2020-03-04")

ggsave("figures/summary-stats/events-by-hour-and-weekday-pre-post-covid.png",
       dpi = 300,
       width = 7,
       height = 7)

event_data[,PostCovid := factor(fifelse(
  date > as.Date("2020-03-04"),
  "Post-Covid",
  "Pre-Covid"),
  levels = c("Pre-Covid",
             "Post-Covid"))]

event_data[,.(AvgEvent = mean(num, na.rm = T)),
           by = c("hour",
                  "WeekDay",
                  "actor_id",
                  "PostCovid")] %>%
  dcast(...~PostCovid, value.var = "AvgEvent") %>%
  .[,Diff := exp(log(`Post-Covid`) - log(`Pre-Covid`))] %>%
  .[,WeekDay := factor(WeekDay,
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[,.(Diff = mean(Diff, na.rm = T)), by = c("hour","WeekDay")] %>%
  ggplot(., aes(x = hour, y = Diff)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~WeekDay)

event_data[,.(sum(num)), by = c("date","hour")] %>%
  ggplot(aes(x = to_posix(date, hour),
                         y = V1)) +
  geom_line() +
  ggtitle("Event Count by Date and Hour",
          subtitle = "Evidence of Daily & Weekly Seasonality") +
  xlab("Time") +
  ylab("Count")

ggsave("figures/summary-stats/total-events-by-day-and-hour.png",
       dpi = 300,
       width = 7,
       height = 4)




