

# --- reporting --------------------------------------------------------

reporter <- function (repo) {
  stopifnot(repository::is_repository(repo))

  structure(list(repo = repo), class = 'reporter')
}


#' @export
`.DollarNames.reporter` <- function (x, pattern = "") {
  nms <- c('last24', 'day', 'week', 'month')
  grep(pattern, nms, value = TRUE)
}


#' @export
`$.reporter` <- function (x, i) {
  # TODO maybe return the requested data subset instead
}


#' @export
print.reporter <- function (x, ..., mode = 'ask') {
  cat('printing report for:', mode, '\n')
}



