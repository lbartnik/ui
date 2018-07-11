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

map_dbl <- function (x, f, ...) {
  ans <- lapply(x, f, ...)
  as.numeric(unlist(ans))
}

join <- function (x, sep = ', ') {
  paste(x, collapse = sep)
}

is_empty <- function (x) {
  (is.environment(x) && !length(x)) ||
    is.null(x) || is.na(x) || !length(x) || (is.character(x) && !nchar(x))
}

is_index_of <- function (i, x) {
  if (is.numeric(i)) return(i > 0 && i <= length(x))
  i %in% names(x)
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


#' @importFrom stringi stri_paste
cpaste <- function (..., sep = ' ', default = 'default')
{
  cat_chunk <- function (color, chunk, sep) {
    if (identical(color, 'default') || identical(color, '')) {
      color <- default
    } else {
      color <- get_color(color)
    }
    stri_paste(color(chunk), sep, sep = '')
  }

  grey_style <- crayon::make_style(grDevices::grey(.6), grey = TRUE)
  grey <- function(...) crayon::style(paste0(...), grey_style)

  get_color <- function (color) {
    if (identical(color, "grey")) return(grey)
    get(color, envir = asNamespace("crayon"), inherits = FALSE)
  }

  default <- if (identical(default, 'default')) as.character else get_color(default)
  chunks <- lapply(list(...), stri_paste, collapse = sep)
  if (!length(names(chunks))) names(chunks) <- rep("", length(chunks))

  chunks <- Map(cat_chunk, names(chunks), chunks, c(rep(sep, length(chunks)-1), ''))
  stri_paste(chunks, collapse = '')
}


cat0 <- function (..., sep = '') cat(..., sep = sep)

ccat <- function (..., sep = ' ', default = 'default') cat(cpaste(..., sep = sep, default = default))

ccat0 <- function (..., default = 'default') ccat(..., sep = '', default = default)

#' @importFrom rlang inform
cinform <- function (..., sep = ' ', default = 'default') inform(cpaste(..., sep = sep, default = default))

cinform0 <- function (..., default = 'default') cinform(..., sep = '', default = default)


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

