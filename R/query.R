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
  dollar_name(unwrap(x), i)
}

#' @export
print.wrapper <- function (x) {
  print(unwrap(x))
}


# --- the actual implementation ---

dollar_names <- function (x) UseMethod("dollar_names")

dollar_name <- function (x, n) UseMethod("dollar_name")


ACTION_KEYS <- c('browse')

dollar_names.query <- function (x) {
  c(tag_names(x), ACTION_KEYS)
}

dollar_name.query <- function (x, n) {
  # TODO
  # 1. if `i` is an action key (browse, etc.) run that action
  # 2. if `i` is a valid key, return a key_wrapper
  # 3. if `i` is a valid id check if it exists; if so, add to the query
  # 4. else treat `i` as a name specifier

  is_query_key <- function (x) (x %in% c('time', 'name', 'class', 'id'))
  is_action_key <- function (x) (x %in% ACTION_KEYS)

  if (is_action_key(n)) {
    # TODO run the action
  }

  if (is_query_key(n)) {
    return(wrap(key_specifier(x, n)))
  }

  if (identical(n, "plots")) {
    return(wrap(repository::filter(x, class == 'plot')))
  }

  wrap(x)
}




#' @importFrom rlang UQ
key_specifier <- function (query, key) {
  structure(list(query = query, key = key), class = 'key_specifier')
}

#' @export
print.key_specifier <- function (ks) {
  # TODO print results
  # ... for this key there are the following allowed values ...
}

dollar_names.key_specifier <- function (x, pattern = "") {
  vls <- tag_values(x$query)[[x$key]]
  grep(pattern, vls, value = TRUE)
}

#' @importFrom rlang UQ
dollar_name.key_specifier <- function (x, i) {
  repository::filter(x$query, UQ(x$key) == UQ(i))
}


