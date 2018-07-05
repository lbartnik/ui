sample_state <- function () {
  initiate_state()
}

sample_query <- function (state = sample_state()) {
  expect_true(requireNamespace("repository", quietly = TRUE))
  wrap(repository::filter(state$repo, isTRUE(artifact)))
}
