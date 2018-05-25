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

  stop("unknown tracker key: ", i, call. = FALSE)
}


#' @export
print.history <- function (x, ...) {
  mapply(rev(seq_along(x)), x, FUN = function (no, commit) {
    ccat0('green', no, ': ')
    ccat0('yellow', deparse(commit$expr), '\n')
    ccat0('silver', '  new:   ')
    lapply(commit$new, function (name) {
      cat0(name, ' [', description(commit$data[[name]]), '] ')
    })
    cat('\n')
    ccat0('silver', '  other: ')
    cat(paste(setdiff(names(commit$objects), commit$new), collapse = ' '), '\n')
  })
}


#' @export
print.branches <- function (x, ...) {
  cat0('Found ', length(x), ' branch', if (length(x)>1) 'es', ':\n\n')

  lapply(x, function (ct) {
    ccat0('yellow', toString(ct$time), '\n')
    ccat_(list('  ', silver = 'id:   ', storage::shorten(ct$id), '\n'))
    ccat_(list('  ', silver = 'expr: ', deparse(ct$expr), '\n'))
    ccat_(list('  ', silver = 'vars: ', paste(names(ct$objects), collapse = ' '), '\n'))
  })
}


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

