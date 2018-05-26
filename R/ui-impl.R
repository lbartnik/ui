#' Provide a summary of an object.
#'
#' @param object Object to be described.
#'
#' @import broom
#' @rdname internals
#'
description <- function (object)
{
  if (is_empty(object)) return(NA_character_)

  if (is.data.frame(object)) return(paste0('data.frame[', nrow(object), ', ', ncol(object), ']'))

  if (inherits(object, 'lm')) {
    g <- broom::glance(object)
    return(paste0('lm adjR2:', format(g$adj.r.squared, digits = 2),
                  ' AIC:', format(g$AIC, digits = 2),
                  ' df:', g$df))
  }

  paste(class(object), collapse = '::')
}


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
  evaluatee(print(x, mode = i))
}


#' @export
print.reporter <- function (x, ..., mode = 'ask') {
  cat('printing report for:', mode, '\n')
}



