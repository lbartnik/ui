#' Access a single artifact.
#'
#' @name single_result
#' @rdname single_result
NULL


#' @description `dispatch_result` makes the decision whether to return
#' a wrapped `query` object for further narrowing of the query, or
#' a `single_result` object which wraps a single artifact retrieved
#' from a [repository::repository].
#'
#' @param q a `query` object.
#' @return A [single_result] object or wrapped query `q`.
#'
#' @rdname single_result
dispatch_result <- function (q) {
  stopifnot(is_query(q))

  res <- q %>% summarise(n = n())
  if (identical(res$n, 1L)) {
    a <- as_artifacts(q) %>% read_artifacts %>% first
    return(wrap(single_result(a)))
  }

  wrap(q)
}

#' @description `new_single_result` creates a simple wrapper around an
#' artifact. It then dispatches calls to `print` and exposes additional
#' operations via the dollar operator `$`.
#'
#' @param artifact retrieved from a repository with [repository::read_artifacts]
#' @param repository origin of `artifact`.
#'
#' @rdname single_result
new_single_result <- function (artifact, repository) {
  stopifnot(is_artifact(artifact))
  stopifnot(is_repository(repository))
  structure(list(artifact = artifact, repository = repository),
            class = 'single_result')
}

#' @importFrom rlang UQ
#' @rdname single_result
print.single_result <- function (x, ...) {
  ccat0(grey = 'Query points to a single object\n')
  print(x$artifact)
}

dollar_names.single_result <- function (x, pattern = "") {
  keys <- c("explain", "inspect", "value")
  if (is_artifact_a(x$repo, x$id, 'plot')) {
    keys <- sort(c(keys, 'plot'))
  }

  grep(pattern, keys, value = TRUE)
}

#' @importFrom rlang UQ
dollar_name.single_result <- function (x, i) {

  # print the tree
  if (identical(i, "explain")) {
    ans <- as_artifacts(x$repository) %>% filter(ancestor_of(x$artifact$id)) %>% read_artifacts
    return(wrap(ans, 'explanation'))
  }

  # TODO ???
  if (identical(i, "inspect")) {
    abort("inspect not implemented yet")
  }

  # re-plot a plot
  if (identical(i, 'plot')) {
    if (!artifact_is(x$artifact, 'plot')) {
      abort('cannot plot a non-plot')
    }

    graphics::plot(artifact_data(x$artifact))
    return(invisible(x))
  }

  # return the raw value
  if (identical(i, "value")) {
    cinform0(silver = "Extracting element ", white = shorten(x$artifact$id))
    return(artifact_data(x$artifact))
  }

  abort("unknown key: ", i)
}
