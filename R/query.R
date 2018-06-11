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
  grep(pattern, dollar_names(unwrap(x)), value = TRUE)
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

dollar_names <- function (x) UseMethod("dollar_names")

dollar_name <- function (x, n) UseMethod("dollar_name")


dollar_names.query <- function (x) {
  c("class", "id", "name", "time")
}

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
    return(repository::filter(x, class == 'plot'))
  }

  x
}


#' @importFrom repository execute select top_n
#' @export
print.query <- function (x, ...) {
  cat('Query:\n\n')

  repository:::print.query(x)

  # lapply(x$filter)

  res <- x %>% select(-object, -parent_commit, -id, -parents) %>% execute
  cat('\nMatched ', nrow(res), ' object(s), of that ', sum(res$class == "plot"), " plot(s)",
      sep = "")
  cat('\n\n')

  # TODO
  # 1. exclude tags that are already specified
  Map(names(res), res, f = function (name, values) {
    cat('   ', name, ': ', paste(unique(as.character(values)), collapse = ', '), '\n', sep = '')
  })

  cat('\nFirst three object(s):\n\n')

  # 2. print the first three objects
  res <- x %>% select(object) %>% top_n(3) %>% execute
  lapply(res$object, function (obj) {
    cat0('  ', repository:::description(obj), '\n')
  })

  cat('\n')
  invisible(x)
}



#' @importFrom rlang UQ
new_specifier <- function (query, key) {
  structure(list(query = query, key = key), class = c(key, 'specifier'))
}

#' @export
print.specifier <- function (x, ...) {
  # TODO
  # ... for this key there are the following allowed values ...
}

dollar_names.specifier <- function (x, pattern = "") {
  vls <- tag_values(x$query)[[x$key]]
  grep(pattern, vls, value = TRUE)
}

#' @importFrom rlang UQ
dollar_name.specifier <- function (x, i) {
  tag <- as.symbol(x$key)
  repository::filter(x$query, UQ(i) %in% UQ(tag))
}


# --- key-speciic specifiers -------------------------------------------

dollar_names.name <- function (x, pattern = "") {
  vls <- tag_values(x$query)[["names"]]
  grep(pattern, vls, value = TRUE)
}

dollar_name.name <- function (x, i) {
  repository::filter(x$query, UQ(i) %in% names)
}


dollar_name.time <- function (x, i) {
  repository::filter(x$query, as.character(time) == UQ(i))
}



