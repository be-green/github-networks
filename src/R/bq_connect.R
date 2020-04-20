#' Connect to a bigquery project
#' @param project project to be passed along to DBI
#' @param dataset dataset to be passed along to DBI
#' @details See ?DBI::dbConnect.biquery
bq_con <- function(project = "github-productivity",
                    dataset = "") {
  if(!bigrquery::bq_authable()) {
    bigrquery::bq_auth()
  }

  dbConnect(bigquery(),
            project,
            dataset,
            billing = project,
            page_size = 10000,
            quiet = NA,
            use_legacy_sql = FALSE,
            bigint = "integer64")

}

