sample_state <- function () {
  state <- new.env(parent = emptyenv())
  initiate_state(state)
  state$repo <- london_meters()

  state
}

sample_query <- function (state = sample_state()) {
  expect_true(requireNamespace("repository", quietly = TRUE))
  wrap(repository::as_artifacts(state$repo))
}
