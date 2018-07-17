
# TODO expose search tags so that they can be checked against in
#      is_query_key() inside dollar_name.query
dollar_names.query <- function (x, pattern = "") {
  search_tags <- c("class", "id", "name", "time", "session")
  action_keys <- c("history", "tree")

  grep(pattern, sort(c(search_tags, action_keys)), value = TRUE)
}


#' @importFrom rlang abort UQ
#' @importFrom lubridate as_date ymd
#' @importFrom storage enlongate
#' @import utilities
#'
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
    ids <- x %>% select(id) %>% execute %>% first
    expl <- repository::repository_explain(x$repository, ids, ancestors = 0)
    if (is_tree) expl <- set_defaults(expl, style = "tree")
    return(expl)
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

  handle_result(res)
}


#' @importFrom rlang abort
double_bracket.query <- function (x, i) {
  ids <- x %>% select(id, time) %>% arrange(desc(time)) %>% execute %>% nth("id")
  if (!is_index_of(i, ids)) {
    abort(sprintf("`%s` is not an index in this query", as.character(i)))
  }

  id <- nth(ids, i)
  cinform0(silver = "Extracting element ", white = i, silver = " (", white = storage::shorten(id), silver = ")")
  x$repository %>% filter(id == UQ(id)) %>% select(object) %>% execute %>% first %>% first
}


#' @importFrom repository execute select top_n
#' @importFrom stringi stri_paste
#' @export
print.query <- function (x, ..., n = 3) {

  # print the query itself
  ccat(silver = 'Query:\n')
  repository:::print.query(x, ...)

  # and a short summary of types of artifacts
  res <- x %>% unselect %>% select(-object, -parent_commit, -id, -parents) %>% execute(.warn = FALSE)
  ccat0(grey = '\nMatched ', nrow(res),
        grey = ' object(s), of that ', sum(res$class == "plot"),
        grey = " plot(s)\n")

  # print the first n objects
  if (nrow(res)) {
    ids <- x %>% select(id, time) %>% top_n(n) %>% arrange(desc(time)) %>% execute %>% nth("id")

    obj <- repository::repository_explain(x$repository, ids, 0)

    z <- lapply(obj, function (x) {
      ccat(green = '\n*\n')
      print(x)
    })
    cat('\n')

    if (n < nrow(res)) {
      ccat(grey = '... with', nrow(res)-n, grey = 'more object(s)\n')
    }
  }


  # TODO exclude tags that are already specified
  # inform what other tags are not yet specified
  all_tags <- dollar_names(x)
  ccat(grey = 'You can still specify following tags:', all_tags)

  invisible(x)
}


# --- specifiers -------------------------------------------------------

#' Key-specifier object.
#'
#' Key specifier serves in the process of building an object query. It
#' provides an interface to define the `tag` and its value that will be
#' sent to the repository when querying for objects.
#'
#' Each key specifier must adhere to this rule: if the result of the
#' current query contains multiple objects, return a wrapped `query`
#' object. However, if there is only one object in the repository that
#' matches user query, return a wrapped single result.
#'
#' @rdname specifier
#' @importFrom rlang UQ
#'
new_specifier <- function (query, key) {
  structure(list(query = query, key = key), class = c(key, 'specifier'))
}

#' @rdname specifier
is_specifier <- function (x) inherits(x, 'specifier')

#' @rdname specifier
dollar_names.specifier <- function (x, pattern = "") {
  vls <- tag_values(x$query)[[x$key]]
  grep(pattern, vls, value = TRUE)
}

#' @rdname specifier
#'
#' @importFrom rlang UQ
#' @importFrom repository filter
dollar_name.specifier <- function (x, i) {
  tag <- as.symbol(x$key)
  handle_result(repository::filter(x$query, UQ(i) %in% UQ(tag)))
}


#' @rdname specifier
#' @export
print.specifier <- function (x, ...) {
  print_specifier(x)
}

#' @description `print_specifier` is an interceptor for the standard set
#' of S3 `print` methods. We do not want to redefine the print method for
#' arbitrary classes, but at the same time have the ability to handle
#' arbitrary specifier sub-classes (like `"time"` or `"class"`), whose
#' names could potentially collide with existing S3 methods.
#'
#' @rdname specifier
print_specifier <- function (x) UseMethod("print_specifier")


#' @rdname specifier
#' @importFrom rlang UQ
print_specifier.specifier <- function (x) {
  format_specifier_header(x$key)
  format_labels(table_to_labels(table_tag_values(x$query, x$key)))
  invisible(x)
}


#' @description `handle_result` makes the decision whether to return
#' a wrapped `query` object for further narrowing of the query, or
#' a `single_result` object which wraps a single object retrieved
#' from the [repository::repository].
#'
#' @rdname specifier
handle_result <- function (q) {
  stopifnot(repository::is_query(q))

  res <- q %>% select(id) %>% summarise(n = n(), id = min(id)) %>% execute
  if (identical(res$n, 1L)) {
    return(wrap(single_result(first(res$id), q$repo)))
  }

  wrap(q)
}


# --- key-speciic specifiers -------------------------------------------


dollar_names.name <- function (x, pattern = "") {
  vls <- tag_values(x$query)[["names"]]
  grep(pattern, vls, value = TRUE)
}

dollar_name.name <- function (x, i) {
  handle_result(repository::filter(x$query, UQ(i) %in% names))
}


print_specifier.name <- function (x) {
  format_specifier_header("names")
  format_labels(table_to_labels(table_tag_values(x$query, "names")))
}


dollar_names.time <- function (x, pattern = "") {
  top_keys <- c("last", "since", "today", "yesterday", "thisweek")

  # RStudio intercepts the pattern and calls .DollarNames with pattern
  # set to ""; see below for details
  # https://github.com/rstudio/rstudio/commit/c25739a15ca49fda68c10f6fd2d25266065cb80b
  if (rstudioapi::isAvailable()) {
    return(names(DollarNamesMapping$time))
  }

  if (any(stringi::stri_detect_fixed(pattern, c("last", "since")))) {
    keys <- grep(pattern, names(DollarNamesMapping$time), value = TRUE)
  }

  grep(pattern, keys, value = TRUE)
}


#' @importFrom rlang UQ
#' @import utilities
#'
dollar_name.time <- function (x, i) {
  stopifnot(has_name(DollarNamesMapping$time, i))

  expr <- DollarNamesMapping$time[[i]]
  handle_result(repository::filter(x$query, UQ(expr)))
}

print_specifier.time <- function (x) {
  format_specifier_header("time")
  format_labels(names(DollarNamesMapping$time))
}


#' @importFrom dplyr mutate group_by
#' @importFrom lubridate as_date hour minute
print_specifier.session <- function (x) {
  raw <- x$query %>% select(session, time) %>% execute
  vls <- raw %>% group_by(session) %>% summarise(time = min(time), n = n()) %>%
    mutate(label = sprintf("%s: %s %s:%s", session, as_date(time), hour(time), minute(time)))

  format_specifier_header("session")
  format_labels(table_to_labels(with_names(vls$n, vls$label)))
}


# --- query result -----------------------------------------------------

is_artifact_a <- function (repo, id, what) {
  stopifnot(what %in% 'plot')

  if (identical(what, 'plot')) {
    class <- repo %>% filter(id == UQ(id)) %>% select(class) %>% execute %>% first %>% unlist
    return('plot' %in% class)
  }
}

single_result <- function (id, repo) {
  structure(list(id = id, repo = repo), class = 'single_result')
}

#' @importFrom rlang UQ
#' @importFrom storage shorten
print.single_result <- function (x, ...) {
  ccat0(grey = 'Query points to a single object\n')
  print(first(repository::repository_explain(x$repo, x$id, ancestors = 0)))
}

dollar_names.single_result <- function (x, pattern = "") {
  keys <- c("explain", "inspect", "value")
  if (is_artifact_a(x$repo, x$id, 'plot')) {
    keys <- sort(c(keys, 'plot'))
  }

  grep(pattern, keys, value = TRUE)
}

#' @importFrom rlang abort UQ
dollar_name.single_result <- function (x, i) {

  if (identical(i, "explain")) {
    # TODO turn this result into a function that takes an extra parameter
    #      (the number of ancestors) or handle an extra number at the
    #      end of this key
    return(repository::repository_explain(x$repo, x$id, ancestors = 7))
  }

  if (identical(i, "inspect")) {
    abort("inspect not implemented yet")
  }

  if (is_artifact_a(x$repo, x$id, 'plot') && identical(i, 'plot')) {
    res <- x$repo %>% filter(id == UQ(x$id)) %>% select(object) %>% execute %>% first %>% first
    plot(res)
    return(invisible(res))
  }

  if (identical(i, "value")) {
    res <- x$repo %>% filter(id == UQ(x$id)) %>% select(object) %>% execute
    return(with_id(res, x$id))
  }

  abort("unknown key: ", i)
}
