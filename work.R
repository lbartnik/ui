options(repository.debug = TRUE)
options(repository.debug = FALSE)

generate_simple(state$repo)

state$repo$store

tracker$history

repository::history_leaves(repository::repository_history(state$repo))
