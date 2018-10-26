#' @title User Interface for the Repository of Artifacts
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
#' @import storage
#' @importFrom glue glue
#' @importFrom rlang inform
#'
#' @name ui
#' @rdname ui
NULL

.onLoad <- function (libname, pkgname) {
  DollarNamesMapping <<- createDollarNamesMapping()
}

.onUnload <- function (libpath) {}
