is_running_in_rstudio <- function () {
  requireNamespace('rstudioapi', quietly = TRUE) && rstudioapi::isAvailable()
}

#' Work with the sample repository.
#'
#' Switches the current repository to the sample one (see [repository::sample_repository()]
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
use_sample_repository <- function () {
  inform("switching to the sample repository, in order to switch back to the current project reload the 'ui' package")
  state$repo <- repository::sample_repository()
  init_artifacts()
}
