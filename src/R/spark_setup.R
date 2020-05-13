library(sparklyr)
library(dplyr)

conf <- spark_config()

conf$sparklyr.defaultPackages <- "org.apache.hadoop:hadoop-aws:2.7.3"
conf$spark.hadoop.fs.s3a.endpoint <- "https://s3.us-east-1.wasabisys.com"
conf$spark.hadoop.fs.s3a.access.key <- Sys.getenv("AWS_ACCESS_KEY_ID")
conf$spark.hadoop.fsa.s3a.secret.key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
conf$spark.hadoop.fsa.s3a.session.token <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
conf$spark.hadoop.fsa.s3a.region <- ""
conf$spark.memory.fraction <- 0.6
conf$`sparklyr.cores.local` <- 8
conf$`sparklyr.shell.driver-memory` <- "6G"

sc <- spark_connect(master = "local[8]",
                    spark_home = paste0(sparklyr::spark_install_dir(),
                                        "/spark-2.4.3-bin-hadoop2.7/"),
                    config = conf)

events <- spark_read_csv(sc, name = "events",
                         path = "data/aggregated-data/org-events/*.csv",
                         memory = TRUE,
                         infer_schema = TRUE)

events_by_hour <- spark_read_csv(sc, name = "events",
                         path = "data/aggregated-data/events-by-hour/*.csv",
                         memory = TRUE,
                         infer_schema = TRUE)

orgs <- spark_read_csv(sc,
                        name = "orgs",
                        path = "data/aggregated-data/org-table/*.csv",
                        memory = FALSE,
                        infer_schema = TRUE) %>%
  distinct

actors <- spark_read_csv(sc,
                        name = "actors",
                        path = "data/aggregated-data/actors-table/*.csv",
                        memory = TRUE,
                        infer_schema = TRUE) %>%
  distinct

select(orgs, org_login) %>%
  distinct %>%
  count

org_events <- filter(events, !is.na(org_id))

count_events_by_hour <- events %>%
  mutate(timestamp = to_timestamp(event_created_at)) %>%
  mutate(hour = hour(timestamp),
         date = to_date(timestamp)) %>%
  group_by(date, hour, actor_id, org_id, event_type) %>%
  summarize(
    num = n()
  )

spark_write_csv(count_events_by_hour, "data/aggregated-data/events-by-hour")

count_events_by_hour %>%
  filter(event_type == "PushEvent") %>%
  spark_write_csv("data/aggregated-data/push-events-by-hour")

all_joined <- events_by_hour %>%
  inner_join(distinct(orgs), by = "org_id") %>%
  inner_join(distinct(actors), by = "actor_id")

spark_write_csv(all_joined, path = "data/aggregated-data/all-joined")

