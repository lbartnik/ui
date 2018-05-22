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
           ccat0(color, paste(chunk, collapse = sep))
         })
}
