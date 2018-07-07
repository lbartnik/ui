map <- function (x, f, ...) {
  ans <- lapply(x, f)
  if (!is.null(names(ans))) return(ans)
  names(ans) <- as.character(x)
  ans
}

map_chr <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.character(unlist(ans))
}

join <- function (x, sep = ', ') {
  paste(x, collapse = sep)
}

is_empty <- function (x) {
  (is.environment(x) && !length(x)) ||
    is.null(x) || is.na(x) || !length(x) || (is.character(x) && !nchar(x))
}


with_names <- function (lst, names) {
  stopifnot(identical(length(lst), length(names)))
  names(lst) <- names
  lst
}



nth <- function(x, n) {
  if (!length(x)) return(vector(mode = typeof(x)))
  x[[n]]
}

last <- function (x) nth(x, length(x))

first <- function(x) nth(x, 1)




cat0 <- function (..., sep = '') cat(..., sep = sep)

ccat <- function (..., sep = ' ', default = 'default')
{
  cat_chunk <- function (color, chunk, sep) {
    if (identical(color, 'default') || identical(color, '')) {
      color <- default
    } else {
      color <- get_color(color)
    }
    cat0(color(chunk), sep)
  }
  get_color <- function (color) get(color, envir = asNamespace("crayon"), inherits = FALSE)

  default <- if (identical(default, 'default')) as.character else get_color(default)
  chunks <- list(...)
  Map(cat_chunk, names(chunks), as.character(chunks), c(rep(sep, length(chunks)-1), ''))
}

ccat0 <- function (...) ccat(..., sep = '')

# --- evaluatee --------------------------------------------------------

evaluatee <- function (expr) {
  quo <- rlang::enquo(expr)
  env <- rlang::caller_env()

  structure(list(quo = quo, env = env), class = 'evaluatee')
}


#' @export
print.evaluatee <- function (x) {
  rlang::eval_tidy(x$quo, env = x$env)
}

# --- log & debug ------------------------------------------------------

log <- function (level, ...) {
  ccat0("red", '[', level, '] ', ..., '\n')
}

dbg <- function (...) {
  if (isTRUE(getOption("ui.debug"))) log("DEBUG", ...)
}

guard <- function (...) {
  x <- sys.call(-1)[[1]]
  fname <- if (is.symbol(x)) deparse(x) else '<unnamed>'
  dbg("-> ", fname, '() ', ...)

  parent <- sys.frame(sys.parent(1))
  expr <- substitute(dbg(x), list(x = paste0('<- ', fname, '()')))
  do.call(on.exit, list(expr = expr, add = TRUE), envir = parent)

  invisible()
}

