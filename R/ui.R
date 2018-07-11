session_tracker <- function () {
  structure(list(), class = 'tracker')
}

#' @export
tracker <- session_tracker()

#' @export
`.DollarNames.tracker` <- function (x, pattern) {
  nms <- c('branches', 'history', 'report')
  grep(pattern, nms, value = TRUE)
}

#' @export
`$.tracker` <- function (x, i) {
  if (identical(i, 'history')) {
    return(repository::repository_history(state$repo, 'current'))
  }

  if (identical(i, 'branches')) {
    hs <- repository::repository_history(state$repo)
    br <- repository::filter(hs, branch_tip())

    if (!length(br)) {
      stop("no branches found in the repository")
    }

    return(structure(br, class = c("branches", class(br))))
  }

  if (identical(i, 'report')) {
    return(reporter(state$repo))
  }

  stop("unknown tracker key: ", i, call. = FALSE)
}


#' @export
print.history <- function (x, ...) {
  mapply(rev(seq_along(x)), x, FUN = function (no, commit) {
    ccat0(default = 'green', no, ': ')
    ccat0(yellow = deparse(commit$expr), '\n')
    ccat0(silver = '  new:   ')
    lapply(commit$new, function (name) {
      cat0(name, ' [', description(commit$data[[name]]), '] ')
    })
    cat('\n')
    ccat0(silver = '  other: ')
    cat(paste(setdiff(names(commit$objects), commit$new), collapse = ' '), '\n')
  })
}


#' @export
print.branches <- function (x, ...) {
  cat0('Found ', length(x), ' branch', if (length(x)>1) 'es', ':\n\n')

  lapply(x, function (ct) {
    ccat0(yellow = toString(ct$time), '\n')
    ccat('  ', silver = 'id:   ', storage::shorten(ct$id), '\n')
    ccat('  ', silver = 'expr: ', deparse(ct$expr), '\n')
    ccat('  ', silver = 'vars: ', paste(names(ct$objects), collapse = ' '), '\n')
  })
}
