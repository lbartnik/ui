#' RStudio AddIn binding.
#'
#' @export
#' @importFrom browser browser_addin
browser_addin_binding <- function (.inBrowser = FALSE) {
  data <- read_artifacts(as_artifacts(state$repo))
  browser_addin(data, .inBrowser)
}
