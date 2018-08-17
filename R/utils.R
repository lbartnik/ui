is_running_in_rstudio <- function () {
  requireNamespace('rstudioapi', quietly = TRUE) && rstudioapi::isAvailable()
}
