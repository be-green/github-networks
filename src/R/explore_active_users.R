library(data.table)
library(anytime)
library(data.table)
library(ggplot2)
library(magrittr)
library(stringr)
library(anytime)


push_event_data <- list.files("data/aggregated-data/push-events-by-hour/",
                         pattern = "*.csv$",
                         full.names = T) %>%
  lapply(function(x) {
    gc()
    fread(x)
  }) %>%
  rbindlist(fill = T) %>%
  .[,.(PushNum = sum(num)), by = c("date","hour","actor_id")]


all_actors <- list.files("data/aggregated-data/actors-table/",
                     pattern = "*.csv$",
                     full.names = T) %>%
  lapply(function(x) {
    gc()
    fread(x)
  }) %>%
  rbindlist(fill = T)


actors <- fread("data/summary-data/active-actor-events.csv")

actors <- merge(actors, push_event_data,
                all.x = T, by = c("hour","date","actor_id"))


print(nrow(actors))

actors <- merge(actors, all_actors, by = "actor_id")
print(nrow(actors))

rm(all_actors, push_event_data)

actors <- actors[,.(num = sum(num)), by = c("date","hour","actor_id",
                                            "actor_login")]
actors[,date := anydate(date, calcUnique = T)]

actors[,botInName := str_detect(actor_login, fixed("[bot]"))]
actors <- actors[!actors$botInName]

actors[,TotalEvents := sum(num), by = actor_id]
actors[,MaxNum := max(num), by = actor_id]
actors[,TotalPctile := as.vector(quantile(num, probs = 0.9)), by = actor_id]
actors[,PushPctile := as.vector(quantile(PushNum, probs = 0.9,
                                         na.rm = T)), by = actor_id]

actors <- actors[MaxNum < 1000]

actors[TotalEvents > 500 & TotalEvents < 30000]

unique_ids <- unique(actors$actor_id)

date_table <- data.table(
  date = seq(as.Date("2020-01-01"),
             as.Date("2020-05-01"),
             by = "day") %>%
    rep(24) %>%
    sort,
  hour = 0:23
)

to_posix <- function(date, hour) {
  as.POSIXct(
    paste0(date, " ", hour, ":00:00")
  )
}

fit_list <- list()

for(i in 1:length(unique_ids)) {
  print(unique_ids[i])
  reg_data <- actors[actor_id == unique_ids[i]]
  reg_data <- merge(date_table, reg_data,
                    by = c("hour","date"),
                    all.x = T)
  reg_data[,actor_id := unique_ids[i]]
  reg_data[is.na(actor_id), num := 0]
  reg_data[,post_covid := fifelse(date > as.Date("2020-03-04"),
                                  "After March 4th",
                                  "Before March 4th")]
  fit_list[[i]] <- glm(num ~ factor(hour) + to_posix(date, hour)*post_covid +
                         weekdays(date),
                       family = poisson(),
                       data = reg_data)
  rm(reg_data)
}

actors <- merge(actors, date_table, all = T)

actors <-
  split(actors,
        by = "actor_id") %>%
  lapply(function(x)
    merge(x, date_table,
          by = c("hour","date"),
          all.y = T)
) %>%
  rbindlist()

actors[is.na(num)]
