
#' @description `handle_result` makes the decision whether to return
#' a wrapped `query` object for further narrowing of the query, or
#' a `single_result` object which wraps a single object retrieved
#' from the [repository::repository].
#'
#' @param q a `query` object.
#'
#' @importFrom dplyr n
#' @rdname specifier
handle_result <- function (q) {
  stopifnot(repository::is_query(q))

  res <- q %>% select(id) %>% summarise(n = n(), id = min(id)) %>% execute
  if (identical(res$n, 1L)) {
    return(wrap(single_result(first(res$id), q$repo)))
  }

  wrap(q)
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
    graphics::plot(res)
    return(invisible(res))
  }

  if (identical(i, "value")) {
    cinform0(silver = "Extracting element ", white = storage::shorten(x$id))
    res <- x$repo %>% filter(id == UQ(x$id)) %>% select(object) %>% execute %>% first %>% first
    return(with_id(res, x$id))
  }

  abort("unknown key: ", i)
}
