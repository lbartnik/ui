new_test_state <- function (repo) {
  expect_true(requireNamespace("repository", quietly = TRUE))

  state <- new_state()
  state$repo <- repo

  state
}

fake_state <- function () new_test_state(fake_repository())

sample_state <- function () new_test_state(repository::london_meters())

sample_query <- function (state = sample_state()) {
  expect_true(requireNamespace("repository", quietly = TRUE))
  repository::as_artifacts(state$repo)
}

sample_wrapped_query <- function (state = sample_state()) wrap(sample_query(state))
