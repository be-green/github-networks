library(sparklyr)
library(dplyr)

conf <- spark_config()

conf$sparklyr.defaultPackages <- "org.apache.hadoop:hadoop-aws:2.7.3"

conf$spark.hadoop.fs.s3a.endpoint <- "https://s3.us-east-1.wasabisys.com"
conf$spark.hadoop.fs.s3a.access.key <- Sys.getenv("AWS_ACCESS_KEY_ID")
conf$spark.hadoop.fsa.s3a.secret.key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
conf$spark.hadoop.fsa.s3a.session.token <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
conf$spark.hadoop.fsa.s3a.region <- ""

sc <- spark_connect(master = "local",
                    spark_home = paste0(sparklyr::spark_install_dir(),
                                        "/spark-2.4.3-bin-hadoop2.7/"),
                    config = conf)

links <- spark_read_csv(sc, "s3a://github-archive/data/*/AssetsDetail.csv",
                        memory = FALSE,
                        infer_schema = TRUE)
