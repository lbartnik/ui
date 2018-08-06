#' @name repository
#' @title User Interface for Artifact Repository
#' @description Provides user interface for the `repository` package.
NULL


#' @export
artifacts <- NULL


set_artifacts <- function (value) {
  artifacts <<- value
}

.onLoad <- function (libname, pkgname)
{
  if (interactive()) {
    initiate_state(state)
    open_default_repo(state, globalenv())

    if (is_tracking_allowed()) {
      start_tracking(state)
    }

    # here it's still possible to change contents of the namespace
    artifacts <<- wrap(repository::filter(state$repo, isTRUE(artifact)))
  }

  DollarNamesMapping <<- createDollarNamesMapping()
}

.onUnload <- function (libpath)
{
  if (interactive() && is_tracking_allowed()) {
    stop_tracking(state)
  }
}

is_tracking_allowed <- function () isTRUE(getOption("ui.track", FALSE))
