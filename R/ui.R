session_tracker <- function () {
  structure(list(), class = 'tracker')
}

#' @export
tracker <- session_tracker()

#' @export
`.DollarNames.tracker` <- function (x, pattern) {
  nms <- c('history', 'branches')
  grep(pattern, nms, value = TRUE)
}

#' @export
`$.tracker` <- function (x, i) {
  if (identical(i, 'history')) {
    h <- repository::repository_history(state$repo)
    a <- repository::history_ancestors(h, repository::repository_last_commit(state$repo))
    mapply(rev(seq_along(a$data)), a$data, FUN = function (no, commit) {
      cat(no, ': ')
      ccat('green', deparse(commit$expr), '\n')
    })
  }

  if (identical(i, 'branches')) {
    hs <- repository::repository_history(state$repo)
    br <- repository::history_ends(hs)

    if (!length(br)) {
      warning("no branches found in the repository", call. = FALSE)
    }
    else {
      lapply(br, function (ct) {
        ccat('yellow', toString(ct$time))
        cat0(',  ', storage::shorten(ct$id), '\n')
        ccat_(list('  ', silver = 'expr: ', deparse(ct$expr), '\n'))
        ccat_(list('  ', silver = 'vars: ', paste(names(ct$objects), collapse = ' '), '\n'))
      })
    }
  }

  invisible()
}



