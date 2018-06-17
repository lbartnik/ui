with_id <- function (object, id) {
  attr(object, 'ui::id') <- id
  object
}

mark_single <- function (x) {
  attr(x, 'ui::single') <- TRUE
  x
}

is_single <- function (x) isTRUE(attr(x, 'ui::single'))


check_single_result <- function (q) {
  stopifnot(repository::is_query(q))

  res <- q %>% select(id) %>% summarise(n = n()) %>% execute
  if (identical(res$n, 1L)) {
    ans <- q %>% select(object, id) %>% execute
    return(mark_single(with_id(first(ans$object), ans$id)))
  }

  q
}



table_tag_values <- function (qry, tag) {
  vls <- qry %>% select(UQ(as.symbol(tag))) %>% execute
  vls <- table(vls)
  paste0(names(vls), ' (', as.integer(vls), ')')
}

#' @importFrom stringi stri_wrap
format_tag_values <- function (tag, values) {
  cat0('Tag: ', tag, '\n\nAllowed values:\n')

  fmt <- paste0("%-", max(nchar(values)) + 2, "s")
  pad <- map_chr(values, function (v) sprintf(fmt, v))
  lns <- stri_wrap(paste(pad, collapse = ''), prefix = '  ', normalize = FALSE)
  cat(paste(lns, collapse = '\n'))
}

#' Assigned in .onLoad
#'
DollarNamesMapping <- NULL


#' @importFrom rlang quo
#' @importFrom lubridate as_date ddays dhours floor_date today
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

