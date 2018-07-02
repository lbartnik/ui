#' @name repository
#' @title User Interface for Artifact Repository
#' @description Provides user interface for the `repository` package.
NULL


#' @export
artifacts <- NULL


.onLoad <- function (libname, pkgname)
{
  if (interactive()) {
    initiate_state()
    start_tracking()

    # here it's still possible to change contents of the namespace
    artifacts <<- wrap(repository::filter(state$repo, isTRUE(artifact)))
    DollarNamesMapping <<- createDollarNamesMapping()
  }
}

.onUnload <- function (libpath)
{
  if (interactive()) {
    stop_tracking()
  }
}
