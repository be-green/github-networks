# Authenticate with your bigquery credentials
library(bigrquery)

# should only need to do this once if you store
# between sessions
bigrquery::bq_auth()

# list all your current bigquery projects
bigrquery::bq_projects()
