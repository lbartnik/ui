options(repository.debug = TRUE)
options(repository.debug = FALSE)

generate_simple(state$repo)

state$repo$store

x <-
  state$repo %>%
  filter(class == "commit") %>%
  select(object) %>%
  execute

debug(repository:::all_tag_names)
tag_names(state$repo)
