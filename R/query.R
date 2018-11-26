# TODO update the doc: distinguish between first-level keys and reqular query keys
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
#' @param repository object returned by [repository::repository]
#' @param x [repository::query] object.
#' @param n action name.
#'
#' @importFrom rlang abort UQ
#' @importFrom lubridate as_date ymd
#' @importFrom storage match_short
#'
#' @export
#' @rdname ui-query
new_query_proxy <- function (repository) {
  stopifnot(is_repository(repository))

  wrap(structure(list(factory = function(...)as_artifacts(repository)),
                 class = 'query_proxy'))
}

#' @export
print.query_proxy <- function (x, ...) {
  cat('Query\n')
  # TODO inform about the possibility of specifying the name, id, date, etc.
  invisible(x)
}

dollar_names.query_proxy <- function (x, pattern) {
  c(dollar_names(x$factory()), c("plots", "models"))
}

dollar_name.query_proxy <- function (x, n) {

  q <- x$factory()

  if (match(n, dollar_names(q), nomatch = 0L) > 0L) {
    return(dollar_name(q, n))
  }

  # assign name to an artifact
  if (identical(n, "assign")) {
    # TODO implement assignment
    abort("assign not implemented yet")
  }

  # shortcut to class$plot
  if (identical(n, "plots")) {
    return(wrap(filter(q, 'plot' %in% class)))
  }

  # if it looks like a date specification...
  d <- ymd(n, quiet = TRUE)
  if (!is.na(d)) {
    return(wrap(filter(q, as_date(time) == UQ(as.character(d)))))
  }

  # if it doesn't look like anything else, it must be an attempt at name or id
  num <- as_artifacts(q) %>% filter(UQ(n) %in% names) %>% summarise(n = n()) %>% first
  if (is.numeric(num) && num > 0) {
    cinform(grey = "")
    res <- filter(q, UQ(n) %in% names)
  }
  else {
    id <- tryCatch(match_short(n, q$store), error = function (e) {
      abort(glue("{n} is not an artifact name nor identifier"))
    })
    # id is unique so we can drop all other filters
    # TODO introduce reset_query
    res <- as_query(q$store) %>% filter(UQ(id) == id)
  }

  dispatch_result(res)
}



#' @rdname ui-query
dollar_name.query <- function (x, n) {
  # TODO
  # 1. if `i` is an action key (browse, etc.) run that action
  # 2. if `i` is a valid key, return a key_wrapper
  # 3. if `i` is a valid id check if it exists; if so, add to the query
  # 4. else treat `i` as a name specifier

  # print as a tree
  if (identical(n, "tree")) {
    g <- connect_artifacts(read_artifacts(as_artifacts(x)))
    return(wrap(g, 'tree'))
  }

  # print as a history log
  if (identical(n, "history")) {
    g <- read_artifacts(as_artifacts(x))
    return(wrap(g, 'history'))
  }

  # if there is only one element that matches
  if (identical(n, "value")) {
    res <- x %>% summarise(n = n()) %>% nth("n")
    if (res == 1) {
      ans <- read_artifacts(as_artifacts(x)) %>% first
      return(artifact_data(ans))
    }

    abort('cannot extract value because query matches multiple artifacts')
  }

  # key specifier
  # TODO check only actual search tags
  is_query_key <- function (k) identical(k, dollar_names(x, k))
  if (is_query_key(n)) {
    return(wrap(new_specifier(x, n)))
  }

  abort(glue("Query specifier {n} is not supported."))
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
  ids <- as_tags(x) %>% arrange(desc(time)) %>% read_tags(id) %>% nth("id")
  if (!is_index_of(i, ids)) {
    abort(glue("{i} is not an index in this query"))
  }

  ans <- as_artifacts(x) %>% filter(id == nth(ids, i)) %>% read_artifacts %>% first
  wrap(new_single_result(ans))
}


#' @importFrom repository top_n
#' @importFrom stringi stri_paste
#' @export
print.query <- function (x, ..., n = 3) {
  # print the query itself
  ccat(silver = 'Query:\n')
  cat(format(x, ...), '\n')

  # and a short summary of types of artifacts
  res <- as_tags(x) %>% read_tags(-parent_commit, -id, -parents)
  ccat0(grey = '\nMatched ', nrow(res),
        grey = ' artifact(s), of that ', sum(map_lgl(res$class, function(x) "plot" %in% x)),
        grey = " plot(s)\n")

  # print the first n objects
  if (nrow(res)) {
    obj <- as_artifacts(x) %>% top_n(n) %>% read_artifacts
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

table_tag_values <- function (qry, tag) {
  vls <- as_tags(qry) %>% read_tags(UQ(as.symbol(tag))) %>% first
  table(unlist(vls))
}
