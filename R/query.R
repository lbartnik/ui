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

  is_query_key <- function (x) (x %in% c('time', 'names', 'class', 'id'))
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


#' @export
print.query <- function (x, ...) {
  repository:::print.query(x)

  # lapply(x$filter)

  res <- x %>% select(-object, -parent_commit, -id, -parents) %>% execute
  cat('\nmatched ', nrow(res), ' object(s), of that ', sum(res$class == "plot"), " plot(s)",
      sep = "")
  cat('\n\n')

  Map(names(res), res, f = function (name, values) {
    cat('   ', name, ': ', paste(unique(as.character(values)), collapse = ', '), '\n', sep = '')
  })

  cat('\nFirst three object(s):\n\n')

  # TODO


  # 1. exclude tags that are already specified
  # 2. print the number of total objects matching, how many plots
  # 3. print the first three objects
}



#' @importFrom rlang UQ
key_specifier <- function (query, key) {
  structure(list(query = query, key = key), class = 'key_specifier')
}

#' @export
print.key_specifier <- function (ks) {
  # TODO
  # ... for this key there are the following allowed values ...
}

dollar_names.key_specifier <- function (x, pattern = "") {
  vls <- tag_values(x$query)[[x$key]]
  grep(pattern, vls, value = TRUE)
}

#' @importFrom rlang UQ
dollar_name.key_specifier <- function (x, i) {
  tag <- as.symbol(x$key)
  wrap(repository::filter(x$query, UQ(i) %in% UQ(tag)))
}


