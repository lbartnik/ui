is_empty <- function (x) {
  (is.environment(x) && !length(x)) ||
    is.null(x) || is.na(x) || !length(x) || (is.character(x) && !nchar(x))
}



nth <- function(x, n) {
  if (!length(x)) return(vector(mode = typeof(x)))
  x[[n]]
}

last <- function (x) nth(x, length(x))

first <- function(x) nth(x, 1)




cat0 <- function (..., sep = '') cat(..., sep = sep)

ccat <- function (color, ..., sep = ' ')
{
  if (identical(color, 'default'))
    cat(..., sep = sep)
  else {
    color <- get(color, envir = asNamespace("crayon"), inherits = FALSE)
    cat(color(paste(..., sep = sep)))
  }
}

ccat0 <- function (color, ...) ccat(color, ..., sep = '')


ccat_ <- function (chunks, sep = ' ')
{
  mapply(color = names(chunks), chunk = chunks,
         function (color, chunk)
         {
           if (!nchar(color)) color <- 'default'
           ccat0(color, paste(chunk, collapse = sep))
         })
}



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

