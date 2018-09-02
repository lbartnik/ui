

dollar_names.name <- function (x, pattern = "") {
  vls <- tag_values(x$query)[["names"]]
  grep(pattern, vls, value = TRUE)
}

dollar_name.name <- function (x, i) {
  handle_result(repository::filter(x$query, UQ(i) %in% names))
}


print_specifier.name <- function (x) {
  format_specifier_header("names")
  format_labels(table_to_labels(table_tag_values(x$query, "names")))
}


dollar_name.id <- function (x, i) {
  i <- storage::enlongate(i, x$query$repository$store)
  stopifnot(length(i) > 0)
  handle_result(x$query$repository %>% repository::filter(UQ(i) == id))
}


dollar_names.time <- function (x, pattern = "") {
  top_keys <- c("last", "since", "today", "yesterday", "thisweek")

  # RStudio intercepts the pattern and calls .DollarNames with pattern
  # set to ""; see below for details
  # https://github.com/rstudio/rstudio/commit/c25739a15ca49fda68c10f6fd2d25266065cb80b
  if (is_running_in_rstudio()) {
    return(names(DollarNamesMapping$time))
  }

  if (any(stringi::stri_detect_fixed(pattern, c("last", "since")))) {
    keys <- grep(pattern, names(DollarNamesMapping$time), value = TRUE)
  }

  grep(pattern, keys, value = TRUE)
}


#' @importFrom rlang UQ
#' @import utilities
#'
dollar_name.time <- function (x, i) {
  stopifnot(has_name(DollarNamesMapping$time, i))

  expr <- DollarNamesMapping$time[[i]]
  handle_result(repository::filter(x$query, UQ(expr)))
}

print_specifier.time <- function (x) {
  format_specifier_header("time")
  format_labels(names(DollarNamesMapping$time))
}


#' @importFrom dplyr mutate group_by
#' @importFrom lubridate as_date hour minute
print_specifier.session <- function (x) {
  raw <- x$query %>% select(session, time) %>% execute
  vls <- raw %>% group_by(session) %>% summarise(time = min(time), n = n()) %>%
    mutate(label = sprintf("%s: %s %s:%02d", session, as_date(time), hour(time), minute(time)))

  format_specifier_header("session")
  format_labels(table_to_labels(with_names(vls$n, vls$label)))
}