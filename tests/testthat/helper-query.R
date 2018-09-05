new_test_state <- function (repo) {
  expect_true(requireNamespace("repository", quietly = TRUE))

  state <- new.env(parent = emptyenv())
  initiate_state(state)
  state$repo <- repo

  state
}

fake_state <- function () new_test_state(fake_repository())

sample_state <- function () new_test_state(repository::london_meters())


sample_query <- function (state = sample_state()) {
  expect_true(requireNamespace("repository", quietly = TRUE))
  wrap(repository::as_artifacts(state$repo))
}
