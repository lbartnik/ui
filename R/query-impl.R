table_tag_values <- function (qry, tag) {
  vls <- qry %>% select(UQ(as.symbol(tag))) %>% execute
  vls <- table(vls)
  paste0(names(vls), ' (', as.integer(vls), ')')
}

#' @importFrom stringi stri_wrap
format_tag_values <- function (tag, values) {
  cat0('Tag: ', tag, '\n\nAllowed values:\n')

  fmt <- paste0("%-", max(nchar(values)) + 2, "s")
  pad <- map_chr(values, function (v) sprintf(fmt, v))
  lns <- stri_wrap(paste(pad, collapse = ''), prefix = '  ', normalize = FALSE)
  cat(paste(lns, collapse = '\n'))
}
