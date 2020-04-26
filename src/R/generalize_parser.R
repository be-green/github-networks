library(tidyr)
library(rlist)
library(rlang)
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
  x <- rlang::flatten(x)
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
                       nodename = NULL,
                       parent_id = NULL,
                       parent_name) {

  terminal_nodes <- filter_not_list(node)

  list_nodes <- filter_is_list(node)
  list_nodes <- map(list_nodes,
                    unnest_singleton_list)

  terminal_dt <- as.data.table(terminal_nodes)
  new_parent_id <- terminal_dt$id

  if(!is.null(nodename) & nrow(terminal_dt) > 0) {

    setnames(terminal_dt,
             colnames(terminal_dt),
             paste0(nodename,
                    "_" ,
                    colnames(terminal_dt)))

  }

  if(!is.null(parent_id) & nrow(terminal_dt) > 0) {
    terminal_dt[,(paste0(parent_name, "_id")) := parent_id]
  }

  terminal_dt_list <- list()
  if(nrow(terminal_dt) > 0) {

    terminal_dt_list[["data"]] <- terminal_dt

  }
  if(length(list_nodes) > 0) {

    for(i in 1:length(list_nodes)) {
      terminal_dt_list[[names(list_nodes)[i]]] <-
        node_table(list_nodes[[i]],
                   nodename = names(list_nodes)[i],
                   parent_id = new_parent_id,
                   parent_name = nodename)
    }
  }
  terminal_dt_list
}

get_data <- function(l, name) {

  envir <- new_environment()

  get_data_to_parent <- function(l, name) {

    for(i in 1:length(l)) {
      if(is.data.table(l[[i]])) {
        assign(name, l[[i]], pos = envir)
      } else {
        get_data_to_parent(l[[i]], names(l)[i])
      }
    }
  }

  get_data_to_parent(l, name)

  lapply(ls(envir), function(x) get(x, envir = envir))

}


