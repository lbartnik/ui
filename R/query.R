# TODO update the doc: distinguish between first-level keys and reqular query keys

#' Interactive user interface to retrieve artifacts.
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
#' @param name name of the object this proxy is accessible through.
#' @param x [repository::query] object.
#' @param n action name.
#'
#' @importFrom rlang abort UQ
#' @importFrom lubridate as_date ymd
#' @importFrom storage match_short
#'
#' @export
#' @rdname artifacts-query
new_query_proxy <- function (repository, name = 'proxy') {
  stopifnot(is_repository(repository))
  wrap(structure(list(repository = repository, name = name), class = 'query_proxy'))
}

#' @export
print.query_proxy <- function (x, ...) {
  if (!is_visible(x)) {
    return(invisible(x))
  }

  ccat0(grey = 'The `', x$name, grey = '` object provides access to artifacts in the repository.\n')
  ccat0(grey = 'Use the dollar-sign operator ', yellow = '$', grey = ' to specify the query. Examples:\n\n')

  ccat0(grey = '  * ', x$name, yellow = '$', '<name>', grey = ' returns the artifact data if name is unique\n')
  ccat0(grey = '  * ', x$name, yellow = '$', '<id>', grey = ' returns the artifact data\n')
  ccat0(grey = '  * ', x$name, yellow = '$', '<tag>', yellow = '$', '<value>',
        grey = ' find artifacts whose <tag> is equal to <value>\n\n')

  ccat0(grey = '  * ', x$name, yellow = '$', 'plots', grey = ' is a shortcut to ',
        grey = x$name, grey = '$class$plot\n')
  ccat0(grey = '  * ', x$name, yellow = '$', '<date>', grey = ' is a shortcut to ',
        grey = x$name, grey = '$time$<date>\n\n')

  ccat0(grey = 'Query keys: ', paste(query_keys(), collapse = ' '), '\n\n')

  ccat(grey = 'Currently operating on', toString(x$repository))

  invisible(x)
}

dollar_names.query_proxy <- function (x, pattern) {
  c(dollar_names(as_query(x$repository), pattern), c("plots"))
}

dollar_name.query_proxy <- function (x, n) {

  q <- as_artifacts(x$repository)

  # standard key specifier?
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
  stopifnot(is.numeric(num))

  if (num == 1) {
    a <- as_artifacts(q) %>% filter(UQ(n) %in% names) %>% read_artifacts %>% first
    ccat(grey = "Name", n, grey = "is unique, retrieving artifact", toString(a$id))
    return(a %>% artifact_data)
  }
  if (num > 1) {
    ccat0(default = 'grey', "Multiple objects named ", green = n, ", try ",
          x$name, "$", green = "name", "$", green = n, " instead.")
    return(set_invisible(x))
  }

  id <- match_short(n, q$store)
  if (is.null(id)) {
    abort(glue("{n} is not an artifact name nor identifier"))
  }

  cinform(grey = "Retrieving artifact with id matching", toString(id))
  reset_query(q) %>% as_artifacts %>% filter(UQ(id) == id) %>% read_artifacts %>% first %>% artifact_data
}


# --- query ------------------------------------------------------------

query_keys <- function () c("class", "id", "name", "time", "session")

is_query_key <- function (x) !identical(match(x, query_keys(), nomatch = 0L), 0L)


#' @rdname artifacts-query
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
  if (is_query_key(n)) {
    return(wrap(new_specifier(x, n)))
  }

  abort(glue("Query specifier {n} is not supported."))
}


dollar_names.query <- function (x, pattern = "", action = TRUE) {
  action_keys <- c("history", "tree")

  tags <- if (isTRUE(action)) c(query_keys(), action_keys) else query_keys()
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
  cat0(format(x, ...), '\n')

  # and a short summary of types of artifacts
  res <- tryCatch(as_tags(x) %>% read_tags(-parent_commit, -id, -parents),
                  `nothing-matched` = function(e) NULL)

  # if empty
  if (is.null(res)) {
    ccat0(grey = 'Query does not match any artifacts.')
    return(invisible(x))
  }

  # if not empty
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
