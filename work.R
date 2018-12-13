library(rstudioapi)
library(dplyr)

cb <- function (expr, result, successful, printed, state) {
  print(getSourceEditorContext())
  return(T)
}

addTaskCallback(cb)

iris %>%
  mutate(x = Species)

