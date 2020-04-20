# Authenticate with your bigquery credentials
library(bigrquery)
library(DBI)

# should only need to do this once if you store
# between sessions
bigrquery::bq_auth()
