with_id <- function (object, id) {
  attr(object, 'ui::id') <- id
  object
}

mark_single <- function (x) {
  attr(x, 'ui::single') <- TRUE
  x
}

is_single <- function (x) isTRUE(attr(x, 'ui::single'))


format_specifier_header <- function (tag_name) {
  ccat(silver = 'Tag: ', tag_name, silver = '\nPossible values:\n')
}

table_tag_values <- function (qry, tag) {
  vls <- qry %>% select(UQ(as.symbol(tag))) %>% execute %>% first
  table(unlist(vls))
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

#' Assigned in .onLoad
#'
DollarNamesMapping <- NULL


#' @importFrom rlang quo
#' @importFrom lubridate as_date ddays dhours floor_date today wday
#'
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

