#' Interactive UI for queries.
#'
#' UI for the [repository::query] object. It employs R's dollar operator
#' `$` to provide an interactive access to the repository of artifacts.
#' The `key` of the `$` operator denotes an action. Supported actions
#' are:
#'
#'   * `history`
#'   * `tree`
#'   * `value`
#'   * query specifier: `name`, `id`, `class`, `time`, `session`
#'   * artifact name or identifier
#'
#' @param x [repository::query] object.
#' @param n action name.
#'
#' @importFrom rlang abort UQ
#' @importFrom lubridate as_date ymd
#' @importFrom storage enlongate
#'
#' @rdname ui-query
dollar_name.query <- function (x, n) {
  # TODO
  # 1. if `i` is an action key (browse, etc.) run that action
  # 2. if `i` is a valid key, return a key_wrapper
  # 3. if `i` is a valid id check if it exists; if so, add to the query
  # 4. else treat `i` as a name specifier

  is_query_key <- function (k) identical(k, dollar_names(x, k))

  is_history <- identical(n, "history")
  is_tree <- identical(n, "tree")

  if (is_history || is_tree) {
    a <- read_artifacts(x)
    g <- connect_artifacts(a)
    if (is_tree) return(new_tree(g)) else return(new_history(g))
  }

  # TODO check only actual search tags
  if (is_query_key(n)) {
    return(wrap(new_specifier(x, n)))
  }

  if (identical(n, "plots")) {
    return(wrap(repository::filter(x, 'plot' %in% class)))
  }

  # if there is only one element that matches
  if (identical(n, "value")) {
    res <- x %>% select(id) %>% summarise(n = n()) %>% execute
    if (res$n == 1) {
      res <- x %>% select(object) %>% execute
      return(first(res$object))
    }
    stop('value for multiple objects')
  }

  # if it looks like a date specification...
  d <- ymd(n, quiet = TRUE)
  if (!is.na(d)) {
    return(wrap(repository::filter(x, as_date(time) == UQ(as.character(d)))))
  }

  # if it doesn't look like anything else, it must be an attempt at name or id
  res <- x  %>% select(id) %>% repository::filter(UQ(n) %in% names) %>% summarise(n = n()) %>% execute %>% first
  if (is.numeric(res) && res > 0) {
    res <- repository::filter(x, UQ(n) %in% names)
  }
  else {
    id <- tryCatch(enlongate(n, x$repository$store), error = function (e) {
      abort(sprintf("`%s` does not identify an artifact", n))
    })
    res <- repository::filter(x$repository, UQ(id) == id)
  }

  dispatch_result(res)
}


# TODO expose search tags so that they can be checked against in
#      is_query_key() inside dollar_name.query
dollar_names.query <- function (x, pattern = "", action = TRUE) {
  search_tags <- c("class", "id", "name", "time", "session")
  action_keys <- c("history", "tree")

  tags <- if (isTRUE(action)) c(search_tags, action_keys) else search_tags
  grep(pattern, sort(tags), value = TRUE)
}




#' @importFrom rlang abort
#' @importFrom dplyr desc
double_bracket.query <- function (x, i) {
  ids <- x %>% select(id, time) %>% arrange(desc(time)) %>% execute %>% nth("id")
  if (!is_index_of(i, ids)) {
    abort(sprintf("`%s` is not an index in this query", as.character(i)))
  }

  id <- nth(ids, i)
  wrap(single_result(id, x$repository))
}


#' @importFrom repository execute select top_n
#' @importFrom stringi stri_paste
#' @export
print.query <- function (x, ..., n = 3) {

  # print the query itself
  ccat(silver = 'Query:\n')
  # TODO rename to format.query
  repository:::print.query(x, ...)

  # and a short summary of types of artifacts
  res <- as_tags(x) %>% read_tags(-object, -parent_commit, -id, -parents)
  ccat0(grey = '\nMatched ', nrow(res),
        grey = ' artifact(s), of that ', sum(vapply(res$class, function(x) "plot" %in% x, logical(1))),
        grey = " plot(s)\n")

  # print the first n objects
  if (nrow(res)) {
    obj <- as_artifacts(x$repository) %>% top_n(n) %>% read_artifacts
    lapply(obj, function (x) {
      ccat(green = '\n*\n')
      print(x)
    })
    cat('\n')

    if (n < nrow(res)) {
      ccat(grey = '... with', nrow(res)-n, grey = 'more artifact(s)\n')
    }
  }


  # TODO exclude tags that are already specified
  # inform what other tags are not yet specified
  all_tags <- dollar_names(x, action = FALSE)
  ccat(grey = 'You can still specify following tags:', all_tags)

  invisible(x)
}



is_artifact_a <- function (repo, id, what) {
  stopifnot(what %in% 'plot')

  if (identical(what, 'plot')) {
    class <- repo %>% filter(id == UQ(id)) %>% select(class) %>% execute %>% first %>% unlist
    return('plot' %in% class)
  }
}
