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


actors <- list.files("data/aggregated-data/actors-table/",
                   pattern = "*.csv$",
                   full.names = T) %>%
  lapply(function(x) {
    gc()
    fread(x)
  }) %>%
  rbindlist(fill = T)


orgs <- orgs[event_data, nomatch = 0, on = "org_id"]

rm(event_data)

orgs[,date := anydate(date, calcUnique = T)]
orgs[,weekday := weekdays(date)]
orgs[,isWeekend := fifelse(weekday %in% c("Saturday",
                                                "Sunday"),
                                 "Weekend",
                                 "Weekday")]


orgs[,.(sum(num)), by = org_login] %>%
  setorder(-V1) %>%
  View

  ggplot(aes(x = V1)) +
  geom_histogram() +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Distribution of Total Events per Organization") +
  xlab("Number of Events") +
  ylab("Frequency")


orgs[,postcovid := forcats::fct_rev(factor(fifelse(date > as.Date("2020-03-04"),
                                 "After March 4th",
                                 "Before March 4th")))]

rm(event_data, orgs)

ggplot(orgs[org_login %in% c("google")][,.(
                                     num = sum(num)
                                   ),
                                   by = c("hour",
                                          "isWeekend",
                                          "date",
                                          "org_login",
                                          "postcovid")],
       aes(x = hour, y = num, color = postcovid)) +
  geom_smooth(span = 0.25) +
  facet_wrap(~isWeekend , scales = 'free_y')

ggplot(org_events[org_login %in% c("google",
                                   "microsoft",
                                   "rstudio")],
       aes(x = hour, y = num, color = org_login)) +
  geom_smooth() +
  facet_wrap(~org_login)

ggplot(org_events[org_login == "microsoft"][,.(NumUniquePeople = length(unique(actor_id))),
                                            by = c("date",
                                                   "postcovid",
                                                   "isWeekend")],
       aes(x = date, y = NumUniquePeople,
           fill = postcovid)) +
  geom_col()

ggplot(org_events[org_login == "microsoft"][,.(NumEvents = sum(num)),
                                            by = c("date",
                                                   "postcovid",
                                                   "isWeekend")],
       aes(x = date, y = NumEvents,
           fill = postcovid)) +
  geom_col() +
  facet_wrap(~isWeekend)

# case study, microsoft

microsoft <- org_events[org_login == "microsoft"]
pre_wfh <- microsoft[date < as.Date("2020-03-04") &
                       date > as.Date("2020-01-01")]

# predict with just day of week and hour
pre_fit <- glm(num ~ 0 + weekday + factor(hour),
               data = pre_wfh[,.(num = sum(num)),
                              by = c("hour",
                                     "weekday",
                                     "date")],
               family = "poisson")

pred <- predict(pre_fit,
                newdata =
                  microsoft[,.(num = sum(num)),
                          by = c("hour",
                                 "weekday",
                                 "date")],
                type = "response")

microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = to_posix(date, hour),
             y = Residuals)) +
  geom_smooth()


microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = to_posix(date, hour),
             y = Residuals,
             color = postcovid)) +
  geom_col()




# predict with trend also
pre_fit <- glm(num ~ 0 + weekday + factor(hour) + to_posix(date, hour),
               data = pre_wfh[,.(num = sum(num)),
                              by = c("hour",
                                     "weekday",
                                     "date")],
               family = "poisson")

pred <- predict(pre_fit,
                newdata =
                  microsoft[,.(num = sum(num)),
                            by = c("hour",
                                   "weekday",
                                   "date")],
                type = "response")

microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = to_posix(date, hour),
             y = Residuals)) +
  geom_smooth()

microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = to_posix(date, hour),
             y = Residuals,
             color = postcovid)) +
  geom_col()

# predict with trend also
pre_fit <- glm(num ~ 0 + weekday +
                 factor(hour) +
                 shift(num) +
                 to_posix(date, hour),
               data = pre_wfh[,.(num = sum(num)),
                              by = c("hour",
                                     "weekday",
                                     "date")],
               family = "poisson")

pred <- predict(pre_fit,
                newdata =
                  microsoft[,.(num = sum(num)),
                            by = c("hour",
                                   "weekday",
                                   "date")],
                type = "response")

microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = to_posix(date, hour),
             y = Residuals)) +
  geom_smooth()

microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = to_posix(date, hour),
             y = Residuals,
             fill = postcovid)) +
  geom_col()


microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = factor(hour), y = Residuals)) +
  geom_boxplot() +
  facet_wrap(~postcovid)


# predict with trend also
pre_fit <- MASS::glm.nb(num ~ 0 + weekday +
                 factor(hour) +
                 to_posix(date, hour),
               data = pre_wfh[,.(num = sum(num)),
                              by = c("hour",
                                     "weekday",
                                     "date")])

pred <- predict(pre_fit,
                newdata =
                  microsoft[,.(num = sum(num)),
                            by = c("hour",
                                   "weekday",
                                   "date")],
                type = "response")

microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = to_posix(date, hour),
             y = Residuals)) +
  geom_quantile()

microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = to_posix(date, hour),
             y = Residuals,
             fill = postcovid)) +
  geom_col()


microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,weekday := factor(weekday,
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  ggplot(aes(x = weekday, y = num)) +
  stat_summary() +
  facet_wrap(~postcovid)


microsoft[,.(num = sum(num)),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid")] %>%
  .[,weekday := factor(weekday,
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[,Predicted := pred] %>%
  .[,Residuals := num - pred] %>%
  .[month(date) < 5] %>%
  .[,month := factor(month(date),
                     levels = 1:4,
                     labels = c("January",
                                "February",
                                "March",
                                "April"))] %>%
  ggplot(aes(x = month, y = num)) +
  stat_summary() +
  facet_wrap(~weekday, scales = 'free_y') +
  ggtitle("Mean Number of Events on each Weekday")

test_orgs <- c("facebook",
               "microsoft",
               "google")

orgs%>%
  .[,.(participants = length(unique(actor_id))),
          by = c("hour",
                 "weekday",
                 "date",
                 "postcovid",
                 "org_login")] %>%
  .[,weekday := factor(weekday,
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[month(date) < 5 & month(date) > 1] %>%
  .[,month := factor(month(date),
                     levels = 1:4,
                     labels = c("January",
                                "February",
                                "March",
                                "April"))] %>%
  ggplot(aes(x = month,
             y = participants)) +
  stat_summary() +
  facet_grid(~weekday,
             scales = 'free_y') +
  ggtitle("Mean Number of Participants on each Weekday")


orgs %>%
  .[date > as.Date("2020-01-01")] %>%
  .[,.(num = sum(num)),
          by = c("hour",
                 "isWeekend",
                 "weekday",
                 "date",
                 "postcovid"
                 )] %>%
  .[,weekday := factor(weekday,
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[weekday == "Monday", WeekDate := date] %>%
  setorder(date, hour) %>%
  .[,WeekDate := zoo::na.locf(WeekDate, na.rm = F)] %>%
  .[month(date) < 5] %>%
  .[,month := factor(month(date),
                     levels = 1:4,
                     labels = c("January",
                                "February",
                                "March",
                                "April"))] %>%
  ggplot(aes(x = to_posix(date, hour), y = num, color = postcovid)) +
  geom_smooth() +
  facet_wrap(~isWeekend,
             scales = 'free_y',
             nrow = 7) +
  ggtitle("Mean Number of Events on each Weekday")


library(bsts)

data <- orgs[org_login %in% test_orgs][,.(
  sum(num)
), by = c("date","hour")] %>%
  .[date >= as.Date("2020-01-01")] %>%
  setorder(date, hour)

y <- orgs[org_login %in% test_orgs][,.(
  sum(num)
), by = c("date","hour")] %>%
  .[date >= as.Date("2020-01-01")] %>%
  setorder(date, hour) %>%
  .[["V1"]]

holidayList <- named.holidays %>% lapply(NamedHoliday)

### Run the bsts model

post_covid <- fifelse(data$date > as.Date("2020-03-04"),
                      "After COVID",
                      "Before COVID")

ss <- addLocalLevel(list(), y)

ss <- AddLocalLinearTrend(ss, y)

# hours in day
ss <- AddSeasonal(ss, y, nseasons = 24)

# days in week
ss <- AddSeasonal(ss, y, nseasons = 7, season.duration = 24)
#
# ss <- AddRegressionHoliday(ss, y, holiday.list = holidayList,
#                            time0 = )

fit <- bsts(y,
            state.specification = ss,
            niter = 2000,
            ping=100,
            seed=2016,
            family = "poisson")

plot_data <- data.table(
  Trend = colMeans(fit$state.contributions[-(1:burn), "trend",]),
  Hourly = colMeans(fit$state.contributions[-(1:burn),"seasonal.24.1",]),
  Weekly = colMeans(fit$state.contributions[-(1:burn),"seasonal.7.24",]),
  Date = data$date,
  Hour = data$hour,
  PostCovid = fifelse(data$date > as.Date("2020-03-04"),
                                          "After COVID",
                                          "Before COVID")) %>%
  .[Date > as.Date("2020-01-01")]


ggplot(plot_data,
       aes(x = to_posix(Date, Hour), y = Weekly)) +
  geom_line()



orgs[org_login %in% c("microsoft","google")] %>%
  .[,.(participants = sum(num)),
    by = c("hour",
           "weekday",
           "date",
           "postcovid",
           "org_login")] %>%
  .[,weekday := factor(weekday,
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[month(date) < 5 & month(date) > 1] %>%
  .[,month := factor(month(date),
                     levels = 1:4,
                     labels = c("January",
                                "February",
                                "March",
                                "April"))] %>%
  ggplot(aes(x = month,
             y = participants)) +
  stat_summary() +
  facet_grid(org_login~weekday,
             scales = 'free_y') +
  ggtitle("Mean Number of Events on each Weekday",
          subtitle = "At Microsoft and Google") +
  ylab("Number of Events")


orgs[org_login %in% c("microsoft","google")] %>%
  .[,.(participants = length(unique(actor_id))),
    by = c("hour",
           "weekday",
           "date",
           "postcovid",
           "org_login")] %>%
  .[,weekday := factor(weekday,
                       levels = c("Monday",
                                  "Tuesday",
                                  "Wednesday",
                                  "Thursday",
                                  "Friday",
                                  "Saturday",
                                  "Sunday"))] %>%
  .[month(date) < 5 & month(date) > 1] %>%
  .[,month := factor(month(date),
                     levels = 1:4,
                     labels = c("January",
                                "February",
                                "March",
                                "April"))] %>%
  ggplot(aes(x = month,
             y = participants)) +
  stat_summary() +
  facet_grid(org_login~weekday,
             scales = 'free_y') +
  ggtitle("Mean Number of Unique Participants on each Weekday",
          subtitle = "At Microsoft and Google")




