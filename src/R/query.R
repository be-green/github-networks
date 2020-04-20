#' Query connection
#' @param con connection to either
#' @param text text or file
query <- function(con, text) {
  dbGetQuery(
    con,
    parse_query(text)
  )
}
