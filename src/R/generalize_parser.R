library(tidyr)
library(rlist)
library(purrr)
library(data.table)

filter_not_list <- function(l) {

  Filter(function(x) {
    !is.list(x)
  }, l)
}


filter_is_list <- function(l) {
  Filter(function(x) {
    is.list(x) & length(x) > 0
  }, l)
}

flatten_with_names <- function(x) {
  nms <- names(x)
  x <- flatten(x)
  names(x) <- paste0(nms, "_", names(x))
  x
}

is_singleton_list <- function(l) {
  all(sapply(l, length) == rep(1, length(l)))
}

is_list_of_lists <- function(l) {
  all(sapply(l, class) == "list")
}

unnest_singleton_list <- function(x) {
  if(is_list_of_lists(x) &
     is_singleton_list(x)) {
    flatten_with_names(x)
  } else {
    x
  }
}

node_table <- function(node,
                       nodename,
                       parent_id = NULL) {
  terminal_nodes <- filter_not_list(node)
  list_nodes <- filter_is_list(node)
  list_nodes <- map(list_nodes,
                    unnest_singleton_list)

  terminal_dt <- as.data.table(terminal_nodes)

  # if(!is.null(nodename)) {
  #
  #   setnames(terminal_dt,
  #            colnames(terminal_dt),
  #            paste0(nodename,
  #                   "_" ,
  #                   colnames(terminal_dt)))
  #
  # }

  if(!is.null(parent_id)) {
    terminal_dt[,(paste0(nodename))]
  }

  terminal_dt_list <- list()
  terminal_dt_list[["data"]] <- terminal_dt

  if(length(list_nodes) > 0) {

    for(i in 1:length(list_nodes)) {
      terminal_dt_list[[names(list_nodes)[i]]] <-
        node_table(list_nodes[[i]],
                   nodename = names(list_nodes)[i])
    }
  }

  terminal_dt_list

}

tmp <- node_table(l)

tmp$payload$pull_request$head$data


