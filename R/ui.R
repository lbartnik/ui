session_tracker <- function () {
  structure(list(), class = 'tracker')
}

#' @export
tracker <- session_tracker()

#' @export
`.DollarNames.tracker` <- function (x, pattern) {
  nms <- c('history')
  grep(pattern, nms, value = TRUE)
}

#' @export
`$.tracker` <- function (x, i) {
  if (identical(i, 'history')) {
    h <- repository::repository_history(state$repo)
    a <- repository::ancestors(h, repository::repository_last_commit(state$repo))
    mapply(seq_along(a), a, FUN = function (no, commit) {
      cat(no, ': ')
      ccat('green', deparse(commit$expr), '\n')
    })
  }

  invisible()
}

