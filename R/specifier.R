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
#' @param query `query` object.
#' @param key Tag name as appears in the search UI.
#'
#' @rdname specifier
#' @importFrom rlang UQ
#'
new_specifier <- function (query, key) {
  structure(list(query = query, key = key), class = c(key, 'specifier'))
}

#' @rdname specifier
is_specifier <- function (x) inherits(x, 'specifier')

#' @param x `specifier` object or an object to be tested.
#' @param pattern regular expression; only matching names are returned.
#'
#' @rdname specifier
dollar_names.specifier <- function (x, pattern = "") {
  vls <- unlist(tag_values(x$query)[[x$key]])
  grep(pattern, vls, value = TRUE)
}

#' @param i key name.
#' @rdname specifier
#'
#' @importFrom rlang UQ
dollar_name.specifier <- function (x, i) {
  tag <- as.symbol(x$key)
  dispatch_result(filter(x$query, UQ(i) %in% UQ(tag)))
}


#' @param ... further arguments passed to or from other methods.
#' @rdname specifier
#' @export
print.specifier <- function (x, ...) {
  n <- as_artifacts(x$query) %>% summarise(n = n()) %>% first

  if (identical(n, 0L)) {
    ccat(grey = "No artifacts match the query.\n")
    return(invisible(x))
  }

  print_specifier(x)
}

#' @description `print_specifier` is an interceptor for the standard set
#' of S3 `print` methods. We do not want to redefine the print method for
#' arbitrary classes, but at the same time have the ability to handle
#' arbitrary specifier sub-classes (like `"time"` or `"class"`), whose
#' names could potentially collide with existing S3 methods.
#'
#' @rdname specifier
print_specifier <- function (x) UseMethod("print_specifier")


#' @rdname specifier
#' @importFrom rlang UQ
print_specifier.specifier <- function (x) {
  format_specifier_header(x$key)
  format_labels(table_to_labels(table_tag_values(x$query, x$key)))
  invisible(x)
}
