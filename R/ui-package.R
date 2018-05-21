#' @name repository
#' @title User Interface for Artifact Repository
#' @description Provides user interface for the `repository` package.
NULL

.onLoad <- function (libname, pkgname)
{
  if (interactive()) {
    initiate_state()
    start_tracking()
  }
}

.onUnload <- function (libpath)
{
  if (interactive()) {
    stop_tracking()
  }
}
