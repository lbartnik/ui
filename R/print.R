new_tree <- function (x) {
  stopifnot(is_container(x))
  wrap(x, 'tree')
}

new_history <- function (x) {
  stopifnot(is_container(x))
  wrap(x, 'history')
}

new_replot <- function (x) {
  stopifnot(is_rawplot(x))
  wrap(x, 'replot')
}


#' Pretty-print sets of artifacts.
#'
#' @param x artifacts, e.g. returned  [repository::read_artifacts].
#' @inheritDotParams base::print
#'
#' @importFrom rlang warn
#'
#' @export
#' @rdname printers
print.tree <- function (x, ...) {
  x <- unwrap(x)

  # if there is nothing to print
  if (!length(x)) {
    warn("container is empty")
  }

  vert  <- '\u2502   '
  vert0 <- '    '
  fork  <- '\u251c\u2500\u2500 '
  fork0 <- '\u2514\u2500\u2500 '

  print_level <- function (x, indent, exdent) {
    i <- order(map_dbl(x$children, `[[`, 'time'), decreasing = FALSE)
    chld <- x$children[i]

    ccat0(silver = indent)
    print(x, style = 'line')

    Map(y = chld, k = seq_along(chld), f = function (y, k) {
      if (k == length(chld)) {
        print_level(y, paste0(exdent, fork0), paste0(exdent, vert0))
      } else {
        print_level(y, paste0(exdent, fork), paste0(exdent, vert))
      }
    })
    invisible(x)
  }

  print_level(stratify(connect_artifacts(x)), '', '')
}


#' @export
#' @rdname printers
print.history <- function (x, ...) {
  x <- unwrap(x)

  # sort entries and then print them
  i <- order(map_dbl(x, `[[`, 'time'), decreasing = FALSE)
  x <- x[i]

  # insert \n between two printouts
  print(first(x), style = 'short')
  lapply(x[-1], function (y) { cat('\n'); print(y, style = 'short') })

  invisible(x)
}


#' @description `print.explained` pretty-prints a description of an
#' artifact.
#'
#' @param style `"full"`, `"short"` or `"line"`.
#'
#' @importFrom storage shorten
#'
#' @export
#' @rdname printers
print.artifact <- function (x, ..., style = 'full') {

  stopifnot(style %in% c('full', 'short', 'line'))
  is_plot <- ('plot' %in% x$class)

  # full artifact description
  if (identical(style, 'full')) {
    # preamble
    ccat0(silver = "Artifact: ", green = shorten(x$id), silver = if (is_plot) ' (plot)', '\n')

    # expression that produced this artifact
    ccat0(silver = 'Expression:\n', x$expression)

    # more meta-data
    if (!is_plot) ccat(silver = '\nName:   ', x$names)
    ccat(silver = '\nClass:  ', x$class)
    ccat(silver = '\nCreated:', x$time)
    ccat(silver = '\nSummary:', x$description)
    cat('\n')
  }

  # shortened artifact description
  if (identical(style, 'short')) {
    ccat0(green = storage::shorten(x$id))

    if (length(x$parents)) {
      ccat0(silver = '  parents:', yellow = join(storage::shorten(x$parents), ' '))
    }
    else {
      ccat0(silver = '  no parents')
    }

    ccat0('\n', x$expression, '\n')
  }

  # a single line
  if (identical(style, 'line')) {
    if ('plot' %in% x$class)
      ccat0(grey = '<plot> ', silver = '(', yellow = shorten(x$id), silver = ')\n')
    else
      ccat0(green = first(x$names), silver = ' (', yellow = shorten(x$id), silver = ') ',
            x$description, '\n')
  }

  invisible(x)
}

#' @export
print.replot <- function (x, ...) {
  graphics::plot(unwrap(x))
}
