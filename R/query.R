#' @export
objects <- NULL


wrap <- function (x) {
  structure(list(x), class = 'wrapper')
}


unwrap <- function (x) {
  stopifnot(is_wrapper(x))
  first(x)
}


is_wrapper <- function (x) inherits(x, 'wrapper')


#' @export
`.DollarNames.wrapper` <- function (x, pattern = "") {
  grep(pattern, dollar_names(unwrap(x), pattern), value = TRUE)
}


#' @export
`$.wrapper` <- function (x, i) {
  wrap(dollar_name(unwrap(x), i))
}

#' @export
print.wrapper <- function (x) {
  wrap(print(unwrap(x)))
}


# --- the actual implementation ---

dollar_names <- function (x, pattern = "") UseMethod("dollar_names")

dollar_name <- function (x, n) UseMethod("dollar_name")

print_dollar_names <- function (x) UseMethod("print_dollar_names")




dollar_names.query <- function (x, pattern = "") {
  c("class", "id", "name", "time")
}


#' @importFrom rlang abort
dollar_name.query <- function (x, n) {
  # TODO
  # 1. if `i` is an action key (browse, etc.) run that action
  # 2. if `i` is a valid key, return a key_wrapper
  # 3. if `i` is a valid id check if it exists; if so, add to the query
  # 4. else treat `i` as a name specifier

  is_query_key <- function (x) (x %in% c('time', 'name', 'class', 'id'))
  is_action_key <- function (x) FALSE

  if (is_action_key(n)) {
    # TODO run the action
  }

  if (is_query_key(n)) {
    return(new_specifier(x, n))
  }

  if (identical(n, "plots")) {
    return(repository::filter(x, 'plot' %in% class))
  }

  res <- x %>% summarise(n = n()) %>% execute
  if (res$n == 1) {
    res <- x %>% select(object) %>% execute
    return(first(res$object))
  }

  abort("unknown query key: ", n)
}


#' @importFrom repository execute select top_n
#' @export
print.query <- function (x, ...) {
  cat('Query:\n\n')

  repository:::print.query(x)

  # lapply(x$filter)

  res <- x %>% select(-object, -parent_commit, -id, -parents, .force = TRUE) %>% execute(.warn = FALSE)
  cat('\nMatched ', nrow(res), ' object(s), of that ', sum(res$class == "plot"), " plot(s)",
      sep = "")
  cat('\n\n')

  # TODO
  # 1. exclude tags that are already specified
  Map(names(res), res, f = function (name, values) {
    cat('   ', name, ': ', paste(unique(as.character(values)), collapse = ', '), '\n', sep = '')
  })

  if (nrow(res)) {
    cat('\nFirst three object(s):\n\n')
    # 2. print the first three objects
    res <- x %>% select(object, .force = TRUE) %>% top_n(3) %>% execute
    lapply(res$object, function (obj) {
      cat0('  ', repository:::description(obj), '\n')
    })

    cat('\n')
  }

  invisible(x)
}



#' @importFrom rlang UQ
new_specifier <- function (query, key) {
  structure(list(query = query, key = key), class = c(key, 'specifier'))
}

#' For this key there are following values
#' @export
print.specifier <- function (x, ...) {
  print_dollar_names(x)
}


dollar_names.specifier <- function (x, pattern = "") {
  vls <- tag_values(x$query)[[x$key]]
  grep(pattern, vls, value = TRUE)
}

#' @importFrom rlang UQ
#' @importFrom repository filter
#'
dollar_name.specifier <- function (x, i) {
  tag <- as.symbol(x$key)
  repository::filter(x$query, UQ(i) %in% UQ(tag))
}


#' @importFrom rlang UQ
print_dollar_names.specifier <- function (x) {
  format_tag_values(x$key, table_tag_values(x$query, x$key))
  invisible(x)
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

# --- key-speciic specifiers -------------------------------------------

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



dollar_names.name <- function (x, pattern = "") {
  vls <- tag_values(x$query)[["names"]]
  grep(pattern, vls, value = TRUE)
}

dollar_name.name <- function (x, i) {
  repository::filter(x$query, UQ(i) %in% names)
}




dollar_names.time <- function (x, pattern = "") {
  top_keys <- c("last", "since", "today", "yesterday", "thisweek")

  # RStudio intercepts the pattern and calls .DollarNames with pattern
  # set to ""; see below for details
  # https://github.com/rstudio/rstudio/commit/c25739a15ca49fda68c10f6fd2d25266065cb80b
  if (rstudioapi::isAvailable()) {
    return(names(DollarNamesMapping$time))
  }

  if (any(stringi::stri_detect_fixed(pattern, c("last", "since")))) {
    keys <- grep(pattern, names(DollarNamesMapping$time), value = TRUE)
  }

  grep(pattern, keys, value = TRUE)
}


#' @importFrom rlang UQ has_name
#'
dollar_name.time <- function (x, i) {
  stopifnot(has_name(DollarNamesMapping$time, i))

  expr <- DollarNamesMapping$time[[i]]
  repository::filter(x$query, UQ(expr))
}

print_dollar_names.time <- function (x) {
  format_tag_values("time", names(DollarNamesMapping$time))
}

print_dollar_names.name <- function (x) {
  format_tag_values("names", table_tag_values(x$query, "names"))
}


