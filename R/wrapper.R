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
