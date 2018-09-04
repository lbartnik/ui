fake_artifact <- function () {
  ans <- structure(list(id = 'id', class = 'class'),
                   class = 'artifact')
  stopifnot(is_artifact(ans))
  ans
}

fake_plot_artifact <- function () {
  ans <- structure(list(id = 'id', class = c('plot', 'rawplot')),
                   class = 'artifact')
  stopifnot(is_artifact(ans))
  ans
}

