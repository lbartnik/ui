#' Wrapper and dollar-name interceptors.
#'
#' In order to separate the user-facing API from the testable code that
#' implements user-facing API, all user-facing S3 classes are always
#' wrapped in the `"wrapper"` class. Moreover, instead of overloading
#' the `[[` and `$` operators and the `.DollarNames` method for all these
#' user-facing classes, three new methods are introduced, namely:
#' `double_bracket`, `dollar_name` and `dollar_names`, which have the
#' exact same semantics. This way, in the test code, accessing actual
#' data can be clearly distinguished from the user-facing tab-completion
#' mechanism.
#'
#' @rdname wrapper
#' @name wrapper
NULL


#' @description `wrap` puts `x` in a list and sets that list's class
#' to `"wrapper"`.
#'
#' @param x object to be wrapped, a `wrapper` object to be unwrapped
#'        or printed, or any other object to be tested.
#' @param class optional S3 class name assigned alongside the `"wrapper"`
#'        class.
#'
#' @importFrom rlang is_character
#' @rdname wrapper
wrap <- function (x, class) {
  if (missing(class)) class <- character()
  stopifnot(is_character(class))

  structure(list(x), class = c(class, 'wrapper'))
}

#' @description `unwrap` returns the original wrapped object.
#'
#' @rdname wrapper
unwrap <- function (x) {
  stopifnot(is_wrapper(x))
  first(unclass(x))
}


#' @rdname wrapper
is_wrapper <- function (x) inherits(x, 'wrapper')


#' @description `print.wrapper` calls the actual `print` method for the
#' unwrapped object.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname wrapper
#' @export
print.wrapper <- function (x, ...) {
  invisible(wrap(print(unwrap(x), ...)))
}


#' @description `str.wrapper` is required because RStudio calls str on
#' objects from the global environment, whose `.default` version calls
#' the `[[` operator, which produces a message. This is confusing to the
#' end user because they haven't requested an explicit object extraction.
#'
#' @param object `wrapper` object.
#'
#' @importFrom utils str
#' @rdname wrapper
#' @export
str.wrapper <- function (object, ...) str(unclass(unwrap(object)))


#' @description the `.DollarNames` method and the `$` operator are the
#' only user-facing entry points into the tab-completion mechanism.
#'
#' @param pattern regular expression; only matching names are returned.
#' @importFrom utils .DollarNames
#'
#' @rdname wrapper
#' @export
`.DollarNames.wrapper` <- function (x, pattern = "") dollar_names(x, pattern)


#' @param i key name, index value.
#' @rdname wrapper
#' @export
`$.wrapper` <- function (x, i) dollar_name(x, i)


#' @rdname wrapper
#' @export
`[[.wrapper` <- function (x, i) double_bracket(x, i)


#' @description `dollar_names` is an equivalent of `.DollarNames` and
#' `dollar_name` is an equivalent of the `$` operator. All S3 classes
#' that are publicly exposed via the `"wrapper"` class need to define
#' their own version of these two methods to enable tab completion.
#'
#' @rdname wrapper
dollar_names <- function (x, pattern = "", ...) UseMethod("dollar_names")


#' @rdname wrapper
dollar_name <- function (x, i) UseMethod("dollar_name")


#' @rdname wrapper
double_bracket <- function (x, i) UseMethod("double_bracket")


#' @rdname wrapper
dollar_names.wrapper <- function (x, pattern = "", ...) {
  grep(pattern, dollar_names(unwrap(x), pattern, ...), value = TRUE)
}

#' @rdname wrapper
dollar_name.wrapper <- function (x, i) dollar_name(unwrap(x), i)

#' @rdname wrapper
double_bracket.wrapper <- function (x, i) double_bracket(unwrap(x), i)


#' @rdname wrapper
dollar_names.default <- function (x, pattern = "", ...) {
  grep(pattern, "unwrap", value = TRUE)
}


#' @rdname wrapper
#' @importFrom rlang abort
dollar_name.default <- function (x, i) {
  if (identical(i, "unwrap")) return(x)
  abort("unknown key: ", i)
}

#' @importFrom rlang abort
double_bracket.default <- function (x, i) abort(sprintf("`[[` operator not defined for class %s", first(class(x))))
