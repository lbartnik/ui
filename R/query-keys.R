
dollar_names.name <- function (x, pattern = "") {
  vls <- tag_values(x$query)[["names"]]
  grep(pattern, vls, value = TRUE)
}

dollar_name.name <- function (x, i) {
  dispatch_result(filter(x$query, UQ(i) %in% names))
}

print_specifier.name <- function (x) {
  format_specifier_header("names")
  format_labels(table_to_labels(table_tag_values(x$query, "names")))
}


dollar_names.id <- function (x, pattern = "") {
  vls <- first(read_tags(as_tags(x$query), 'id'))
  grep(pattern, vls, value = TRUE)
}

#' @importFrom storage match_short
dollar_name.id <- function (x, i) {
  i <- match_short(i, x$query$store)
  stopifnot(length(i) > 0)
  dispatch_result(x$query %>% filter(UQ(i) == id))
}


dollar_names.time <- function (x, pattern = "") {
  # RStudio intercepts the pattern and calls .DollarNames with pattern
  # set to ""; see below for details
  # https://github.com/rstudio/rstudio/commit/c25739a15ca49fda68c10f6fd2d25266065cb80b
  if (is_rstudio()) {
    return(names(DollarNamesMapping$time))
  }

  keys <- c("last", "since", "today", "yesterday", "thisweek")

  if (any(stringi::stri_detect_fixed(pattern, c("last", "since")))) {
    keys <- grep(pattern, names(DollarNamesMapping$time), value = TRUE)
  }

  grep(pattern, keys, value = TRUE)
}

#' @importFrom rlang UQ
dollar_name.time <- function (x, i) {
  stopifnot(has_name(DollarNamesMapping$time, i))

  expr <- DollarNamesMapping$time[[i]]
  dispatch_result(filter(x$query, UQ(expr)))
}

print_specifier.time <- function (x) {
  format_specifier_header("time")
  format_labels(names(DollarNamesMapping$time))
}

#' Assigned in .onLoad
#'
#' @rdname dollar-names-mapping
DollarNamesMapping <- NULL

#' @importFrom rlang quo
#' @importFrom lubridate as_date ddays dhours floor_date today wday
#'
#' @rdname dollar-names-mapping
createDollarNamesMapping <- function () {
  last_wday <- function (which) {
    date <- today() - wday(today(), week_start = 7) + which
    if (date > today()) date <- date - 7
    as.character(date)
  }

  list(
    time = list(
      today           = quo(as_date(time) == today()),
      yesterday       = quo(as_date(time) == today()-1),
      thisweek        = quo(as_date(time) >= floor_date(today(), "week")),
      last_24hrs      = quo(time > today() - dhours(24)),
      last_3days      = quo(as_date(time) > today() - ddays(3)),
      last_7days      = quo(as_date(time) > today() - ddays(7)),
      last_day        = quo(as_date(time) > today() - ddays(1)),
      last_week       = quo(as_date(time) > today() - ddays(7)),
      since_yesterday = quo(as_date(time) >= today() - ddays(1)),
      since_Monday    = quo(as_date(time) >= UQ(last_wday(1))),
      since_Tuesday   = quo(as_date(time) >= UQ(last_wday(2))),
      since_Wednesay  = quo(as_date(time) >= UQ(last_wday(3))),
      since_Thursday  = quo(as_date(time) >= UQ(last_wday(4))),
      since_Friday    = quo(as_date(time) >= UQ(last_wday(5))),
      since_Saturday  = quo(as_date(time) >= UQ(last_wday(6))),
      since_Sunday    = quo(as_date(time) >= UQ(last_wday(7)))
    )
  )
}




#' @importFrom dplyr mutate group_by
#' @importFrom lubridate as_date hour minute
print_specifier.session <- function (x) {
  raw <- as_tags(x$query) %>% read_tags(session, time)
  vls <- raw %>%
    group_by(session) %>%
    summarise(time = min(time), n = n()) %>%
    mutate(label = glue("{session}: {as_date(time)} {hour(time)}:{formatC(minute(time), width = 2, flag = '0')}"))

  format_specifier_header("session")
  format_labels(table_to_labels(with_names(vls$n, vls$label)))
}


# --- older code -------------------------------------------------------

format_specifier_header <- function (tag_name) {
  ccat(silver = 'Tag: ', tag_name, silver = '\nValues (#artifacts):\n')
}

table_to_labels <- function (table) {
  paste0(names(table), ' (', as.integer(table), ')')
}

#' @importFrom stringi stri_length stri_pad_right stri_replace_all_fixed stri_trim_right stri_wrap
format_labels <- function (labels) {
  labels <- stri_replace_all_fixed(labels, ' ', '*')
  labels <- stri_pad_right(labels, max(stri_length(labels), na.rm = TRUE) + 2)
  text <- join(labels, '  ')
  lns <- stri_wrap(text, normalize = FALSE, indent = 2, exdent = 2)
  lns <- stri_replace_all_fixed(lns, '*', ' ')
  lns <- stri_trim_right(lns)
  cat(paste(lns, collapse = '\n'))
}
