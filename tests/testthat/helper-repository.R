fake_artifact <- function () {
  ans <- structure(list(id = 'id', class = 'class'),
                   class = 'artifact')
  stopifnot(repository::is_artifact(ans))
  ans
}

fake_plot_artifact <- function () {
  ans <- structure(list(id = 'id', class = c('plot', 'rawplot')),
                   class = 'artifact')
  stopifnot(repository::is_artifact(ans))
  ans
}

fake_repository <- function () {
  structure(list, class = 'repository')
}

sample_repository <- function () repository::london_meters()

sample_artifact_id <- function () '14e3598b1c58f1f48b74aab35d6c39183568286f'

sample_artifact <- function() {
  repository::as_artifacts(sample_repository()) %>%
    repository::filter(id == UQ(sample_artifact_id())) %>%
    repository::read_artifacts %>%
    utilities::first
}
