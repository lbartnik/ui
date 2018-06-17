#' Dollar-name interceptors. Wrappers.
#'
#' In the process of building a query, a number of objects of different
#' classes are involved. In order to avoid defining multiple `$` operator
#' methods and multiple `.DollarName` methods, we define and export only
#' the pair of methods for the `wrapper` class. These methods pass the
#' control to `dollar_name` and `dollar_names` generics, respectively.
#' These, in turn, `unwrap` the actual object `o` and pass control to the
#' `dollar_name` or `dollar_names` method for that object `o`.
#'
#' This way we:
#'   * avoid the risk of defining and injecting the `$` operator method
#'     into unexpected places, especially other packages
#'   * expose explicit testing API
#'
#' @rdname wrapper
dollar_names <- function (x, pattern = "") UseMethod("dollar_names")

#' @rdname wrapper
dollar_name <- function (x, n) UseMethod("dollar_name")


#' @description `wrap` puts `x` in a list and sets that list's class
#' to `"wrapper"`.
#'
#' @rdname wrapper
wrap <- function (x) structure(list(x), class = 'wrapper')


#' @description `unwrap` returns the original wrapped object.
#'
#' @rdname wrapper
unwrap <- function (x) {
  stopifnot(is_wrapper(x))
  first(x)
}


#' @rdname wrapper
is_wrapper <- function (x) inherits(x, 'wrapper')



#' @rdname wrapper
#' @export
print.wrapper <- function (x) {
  invisible(wrap(print(unwrap(x))))
}

#' @rdname wrapper
#' @export
`.DollarNames.wrapper` <- function (x, pattern = "") dollar_names(x, pattern)


#' @rdname wrapper
#' @export
`$.wrapper` <- function (x, i) dollar_name(x, i)


# --- generic dollar_name(s) -------------------------------------------

#' @rdname wrapper
dollar_names.wrapper <- function (x, pattern = "") {
  grep(pattern, dollar_names(unwrap(x), pattern), value = TRUE)
}

#' @rdname wrapper
dollar_name.wrapper <- function (x, i) dollar_name(unwrap(x), i)


#' @rdname wrapper
dollar_names.default <- function (x, pattern = "") {
  grep(pattern, "unwrap", value = TRUE)
}

#' @rdname wrapper
#' @importFrom rlang abort
dollar_name.default <- function (x, i) {
  if (identical(i, "unwrap")) return(x)
  abort("unknown key: ", i)
}

# --- the actual implementation ---------------------------------------



dollar_names.query <- function (x, pattern = "") {
  grep(pattern, c("class", "id", "name", "time", "session"), value = TRUE)
}


#' @importFrom rlang abort
dollar_name.query <- function (x, n) {
  # TODO
  # 1. if `i` is an action key (browse, etc.) run that action
  # 2. if `i` is a valid key, return a key_wrapper
  # 3. if `i` is a valid id check if it exists; if so, add to the query
  # 4. else treat `i` as a name specifier

  is_query_key <- function (k) identical(k, dollar_names(x, k))
  is_action_key <- function (k) FALSE

  if (is_action_key(n)) {
    # TODO run the action
  }

  if (is_query_key(n)) {
    return(wrap(new_specifier(x, n)))
  }

  if (identical(n, "plots")) {
    return(wrap(repository::filter(x, 'plot' %in% class)))
  }

  # if there is only one element that matches
  if (identical(n, "value")) {
    res <- x %>% select(id) %>% summarise(n = n()) %>% execute
    if (res$n == 1) {
      res <- x %>% select(object) %>% execute
      return(first(res$object))
    }
    stop('value for multiple objects')
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


# --- query result -----------------------------------------------------



# --- specifiers -------------------------------------------------------

#' Key-specifier object.
#'
#' Key specifier serves in the process of building an object query. It
#' provides an interface to define the `tag` and its value that will be
#' sent to the repository when querying for objects.
#'
#' Each key specifier must adhere to this rule: if the result of the
#' current query contains multiple objects, return a wrapped `query`
#' object. However, if there is only one object in the repository that
#' matches user query, return a wrapped single result.
#'
#' @rdname specifier
#' @importFrom rlang UQ
#'
new_specifier <- function (query, key) {
  structure(list(query = query, key = key), class = c(key, 'specifier'))
}

#' @rdname specifier
is_specifier <- function (x) inherits(x, 'specifier')

#' @rdname specifier
dollar_names.specifier <- function (x, pattern = "") {
  vls <- tag_values(x$query)[[x$key]]
  grep(pattern, vls, value = TRUE)
}

#' @rdname specifier
#'
#' @importFrom rlang UQ
#' @importFrom repository filter
dollar_name.specifier <- function (x, i) {
  tag <- as.symbol(x$key)
  check_single_result(repository::filter(x$query, UQ(i) %in% UQ(tag)))
}


#' For this key there are following values
#' @export
print.specifier <- function (x, ...) {
  print_specifier(x)
}

# this way we can have print methods for classes like "time" or "class"
# and it does not interfere with actual print methods for these classes,
# esp. if at some point we introduce a specifier whose name collides
# with something important
print_specifier <- function (x) UseMethod("print_specifier")



#' @importFrom rlang UQ
print_specifier.specifier <- function (x) {
  format_tag_values(x$key, table_tag_values(x$query, x$key))
  invisible(x)
}


# --- key-speciic specifiers -------------------------------------------


dollar_names.name <- function (x, pattern = "") {
  vls <- tag_values(x$query)[["names"]]
  grep(pattern, vls, value = TRUE)
}

dollar_name.name <- function (x, i) {
  check_single_result(repository::filter(x$query, UQ(i) %in% names))
}


print_specifier.name <- function (x) {
  format_tag_values("names", table_tag_values(x$query, "names"))
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
  check_single_result(repository::filter(x$query, UQ(expr)))
}

print_specifier.time <- function (x) {
  format_tag_values("time", names(DollarNamesMapping$time))
}


#' @importFrom dplyr mutate group_by
#' @importFrom lubridate as_date hour minute
print_specifier.session <- function (x) {
  raw <- x$query %>% select(session, time) %>% execute
  vls <- raw %>% group_by(session) %>% summarise(time = min(time), n = n()) %>%
    mutate(label = sprintf("%s: %s %s:%s (%s)", session, as_date(time), hour(time), minute(time), n))
  format_tag_values("session", vls$label)
}

