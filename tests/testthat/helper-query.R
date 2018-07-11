sample_state <- function () {
  state <- new.env(parent = emptyenv())
  initiate_state(state)
  state$repo <- repository::sample_repository()

  state
}

sample_query <- function (state = sample_state()) {
  expect_true(requireNamespace("repository", quietly = TRUE))
  wrap(repository::filter(state$repo, isTRUE(artifact)))
}
