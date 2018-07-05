options(repository.debug = TRUE)
options(repository.debug = FALSE)

options(ui.track = FALSE)

generate_simple(state$repo)

state$repo$store

x <-
  state$repo %>%
  filter(class == "commit") %>%
  select(object) %>%
  top_n(3) %>%
  execute

debug(repository:::all_tag_names)
tag_names(state$repo)
