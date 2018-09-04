#' @title User Interface for Artifact Repository
#' @description Provides user interface for the [repository] package.
#'
#' Package-level [options]:
#'   * `ui.attach_on_load` (default: `TRUE`) find the most up-to-date
#'     [commit] in the repository and continue work from there
#'   * `ui.track` (default: `TRUE`) start tracking when the `ui` package
#'     is loaded
#'
#' @docType package
#'
#' @import repository
#' @import utilities
#' @importFrom storage shorten enlongate
#' @importFrom glue glue
#'
#' @name ui
#' @rdname ui
NULL


message_prefix <- 'Repository: '

msg <- function (..., sep = '') {
  paste0(message_prefix, paste(..., sep = sep))
}


#' @name dictionary
#' @rdname dictionary
#'
#' @title Dictionary of terms.
#'
#' @description Below are definitions which conceptually organize the
#' repository of artifacts.
NULL

#' @name commit
#' @rdname dictionary
#'
#' @description
#'   * `commit` is a record of the state of the R session: its [globalenv()],
#'     the last plot, the last printout, etc.; repository keeps track of
#'     artifacts by recording commits throught the duration of each R session
NULL

#' Search for artifacts.
#'
#' An entry point for artifact search. Exposes an interactive query
#' mechanism with tab-completion via the `$` (dollar) operator.
#'
#' @export
artifacts <- NULL


init_artifacts <- function () {
  unlockBinding('artifacts', asNamespace('ui'))
  artifacts <<- wrap(as_artifacts(state$repo))
  lockBinding('artifacts', asNamespace('ui'))
}


.onLoad <- function (libname, pkgname)
{
  if (interactive()) {
    initiate_state(state)
    open_default_repo(state, .GlobalEnv, create = TRUE)

    if (isTRUE(getOption("ui.attach_on_load", TRUE)) && !is_devtools_load()) {
      pick_branch(state, .GlobalEnv)

      if (is_tracking_allowed()) {
        start_tracking(state)
      }
    }

    # here it's still possible to change contents of the namespace
    init_artifacts()
  }

  DollarNamesMapping <<- createDollarNamesMapping()
}

.onUnload <- function (libpath)
{
  if (interactive() && is_tracking_allowed() && !is_devtools_load()) {
    stop_tracking(state)
  }
}

is_tracking_allowed <- function() isTRUE(getOption("ui.track", TRUE))

is_devtools_load <- function() {
  any(vapply(sys.calls(), function (c) {
    if (!is.call(c)) return(FALSE)
    c <- first(c)
    if (!is.call(c) || !identical(first(c), quote(`::`))) return(FALSE)
    identical(second(c), quote(devtools)) && identical(nth(c, 3), quote(load_all))
  }, logical(1)))
}
