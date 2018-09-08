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

sample_artifact_id <- function () '2b67f4934da0aa3baecfdd3001008539217d5719'

sample_artifact <- function () extract_artifact(sample_artifact_id())

sample_plot_id <- function () '539d7b916fd845319be242640085b59dbcf52506'

sample_plot_artifact <- function () extract_artifact(sample_plot_id())


extract_artifact <- function(id) {
  repository::as_artifacts(sample_repository()) %>%
    repository::filter(id == UQ(id)) %>%
    repository::read_artifacts() %>%
    utilities::first()
}
