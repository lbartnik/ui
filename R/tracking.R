# Track user's interactions with R session.

#' Global state of the tracker.
#'
#' \describe{
#'   \item{repo}{a [repository::repository()] object}
#'   \item{task_callback_id}{id of the callback passed to [addTaskCallback]}
#' }
#'
#' @param state state object (environment).
#' @param env [environment] used to find the branch to attach to.
#' @param create if `TRUE`, create the repository if it does not exist.
#'
#' @rdname internal_state
state <- new.env()

#' @description `initiate_state` assigns the default values to all
#' parameters of the global `state` object. By default it:
#' * creates or open a file-system-based [repository::repository]
#' * turns tracking on
#'
#' @rdname internal_state
initiate_state <- function (state) {
  state$repo             <- NULL
  state$task_callback_id <- NA
}

#' @param path directory for the new/existing repository.
#' @rdname internal_state
open_repo <- function (state, env, path, create) {
  if (!file.exists(path) && isTRUE(create)) {
    inform(glue("{message_prefix}no repository found, creating one under '{path}'"))
  } else if (file.exists(path)) {
    inform(glue("{message_prefix}attaching to repository '{path}'"))
  }

  state$repo <- repository(filesystem(path, create = create))
}

#' @rdname internal_state
open_default_repo <- function (state, env, create = FALSE) {
  open_repo(state, env, file.path(getwd(), 'repository'), create)
}


#' @rdname internal_state
#' @import repository
#' @importFrom rlang abort inform
#'
pick_branch <- function (state, env) {
  most_recent <- function (commits) {
    nth(commits, last(order(map_int(commits, `[[`, i = 'time'))))
  }

  if (length(ls(env))) {
    m <- as_commits(state$repo) %>% filter(data_matches(data = as.list(env))) %>% read_commits
    if (!length(m)) {
      abort(msg("global environment is not empty but its contents cannot be matched against history, will not attach"))
      # TODO show a warning and implement an API call to attach manually
    }

    if (length(m) > 1) {
      inform(msg("global environment matched more than once against history, attaching to the most recent one"))
      m <- most_recent(m)
    } else {
      inform(msg("global environment matches history, attaching to repository"))
      m <- first(m)
    }

    repository_rewind(state$repo, m$id)
    return()
  }

  # if globalenv is empty try attaching to one of the "leaves"
  lv <- as_commits(state$repo) %>% filter(no_children()) %>% read_commits
  if (!length(lv)) return()

  # TODO return commit id and contents, assign to env outside of this function
  if (length(lv) == 1) {
    lv <- first(lv)
  } else {
    inform(msg("repository contains more than one branch, choosing the most recent one"))
    lv <- most_recent(lv)
  }

  inform(msg("attaching to repository, commit ", storage::shorten(lv$id)))

  repository_rewind(state$repo, lv$id)
  commit_checkout(lv, env)
}


#' @rdname internal_state
#'
start_tracking <- function (state)
{
  if (!is.na(state$task_callback_id)) {
    abort("task callback id found, tracking already started", call = TRUE)
  }

  state$task_callback_id <- addTaskCallback(task_callback)

  # TODO see if there is a match for the current globalenv() to continue
  #      work from a given point; if not, ask the user if they want to
  #      rewind to a specific existing commit
}


#' @rdname internal_state
#'
stop_tracking <- function (state)
{
  if (!is.numeric(state$task_callback_id)) {
    stop("task callback id not found, tracking not started")
  }

  removeTaskCallback(state$task_callback_id)
  state$task_callback_id <- NA_character_
}


#' A callback run after each top-level expression is evaluated.
#'
#' From [addTaskCallback()]: if the data argument was specified in
#' the call to addTaskCallback, that value is given as the fifth
#' argument.
#'
#' @param expr Expression for the top-level task.
#' @param result Result of the top-level task.
#' @param successful A logical value indicating whether it was
#'        successfully completed or not (always `TRUE` at present).
#' @param printed A logical value indicating whether the result was
#'        printed or not.
#'
#' @return A logical value indicating whether to keep this function in
#'         the list of active callbacks.
#'
#' @import grDevices
#' @import utilities
#'
task_callback <- function (expr, result, successful, printed)
{
  guard()

  if (!isTRUE(successful))
    return(TRUE)

  tryCatch(
    error = function(e) warning('could not update the repository: ',
                                e$message, call. = FALSE),
    {
      plot <- tryCatch(recordPlot(), error = function(e)'error')
      if (!inherits(plot, 'recordedplot')) {
        plot <- NULL
      }

      # it's length of ls() because we don't care for hidden objects
      if (length(ls(globalenv()))) {
        repository_update(state$repo, globalenv(), plot, expr)
      }
    }
  )

  TRUE
}
