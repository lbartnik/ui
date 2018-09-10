is_running_in_rstudio <- function () {
  requireNamespace('rstudioapi', quietly = TRUE) && rstudioapi::isAvailable()
}

#' Work with the sample repository.
#'
#' Switches the current repository to the sample one (see [repository::london_meters()]
#' for more details). The sample repository is stored in a temporary
#' directory so as long as it is writeable, no changes are preserved
#' beyond the current R session.
#'
#' @export
#' @rdname samples
#'
#' @import repository
#' @importFrom rlang inform
#'
use_london_meters <- function () {
  inform("switching to the sample repository, in order to switch back to the current project reload the 'ui' package")
  state$repo <- london_meters()
  init_artifacts()
}
