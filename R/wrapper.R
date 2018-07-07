#' Wrapper and dollar-name interceptors.
#'
#' In order to separate the user-facing API from the testable code that
#' implements user-facing API, all user-facing S3 classes are always
#' wrapped in the `"wrapper"` class. Moreover, instead of overloading
#' the `$` operator and the `.DollarNames` method for all these
#' user-facing classes, a `dollar_name` and `dollar_names` S3 methods
#' are introduced. They have the exact same semantics. This way, in the
#' test code, the `$` operator can be used to access actual data and
#' clearly distinguished from the tab-completion mechanism for the end
#' user.
#'
#' @rdname wrapper
#' @name wrapper
NULL


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


#' @description `print.wrapper` calls the actual `print` method for the
#' unwrapped object.
#'
#' @rdname wrapper
#' @export
print.wrapper <- function (x) {
  invisible(wrap(print(unwrap(x))))
}


#' @description the `.DollarNames` method and the `$` operator are the
#' only user-facing entry points into the tab-completion mechanism.
#'
#' @importFrom utils .DollarNames
#'
#' @rdname wrapper
#' @export
`.DollarNames.wrapper` <- function (x, pattern = "") dollar_names(x, pattern)


#' @rdname wrapper
#' @export
`$.wrapper` <- function (x, i) dollar_name(x, i)


#' @description `dollar_names` is an equivalent of `.DollarNames` and
#' `dollar_name` is an equivalent of the `$` operator. All S3 classes
#' that are publicly exposed via the `"wrapper"` class need to define
#' their own version of these two methods to enable tab completion.
#'
#' @rdname wrapper
dollar_names <- function (x, pattern = "") UseMethod("dollar_names")


#' @rdname wrapper
dollar_name <- function (x, n) UseMethod("dollar_name")


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
