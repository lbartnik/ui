options(utilities.debug = TRUE)
options(utilities.debug = FALSE)



options(ui.track = FALSE)
options(ui.pick_branch = FALSE)


options(ui.pick_branch = TRUE)

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




> y=artifacts$name$x[[5]]
Extracting element 5 (f59403b5)
> x=artifacts$name$x[[4]]
Extracting element 4 (64228d01)

