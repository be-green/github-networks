#' Process a file or string passed as a query
#' @param x a string to be parsed
#' @details This will
parse_query <- function(x) {
  UseMethod("parse_query")
}

parse_query.default <- function(x) {
  x <- guess_content(x)
  parse_query(x)
}

parse_query.sqlfile <- function(x) {
  paste0(
    readLines(x),
    collapse = "\n"
  )
}

parse_query.querytext <- function(x) {
  x
}

guess_content <- function(x) {
  if(!is.character(x)) {
    stop("Query does not appear to be text or a file.")
  } else {
    if(file.exists(x)) {
      structure(x, class = "sqlfile")
    } else if(file.exists(paste0("src/SQL/", x))) {
      structure(paste0("src/SQL/", x), class = "sqlfile")
    } else {
      structure(x, class = "querytext")
    }
  }
}

