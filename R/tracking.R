# Track user's interactions with R session.

#' Global state of the tracker.
#'
#' \describe{
#'   \item{repo}{a [repository::repository()] object}
#'   \item{task_callback_id}{id of the callback passed to [addTaskCallback]}
#' }
#'
#' @description `new_state` creates a new `state` object and
#' assigns the default values to all its attributes.
#'
#' @rdname state
#' @export
new_state <- function () {
  state <- new.env()
  state_reset(state)
  state
}

#' @param x object being tested.
#' @rdname state
is_state <- function (x) is.environment(x)


#' @description `state_reset` assign default values to all attributes
#' of the `state` object.
#'
#' @param state state object (environment).
#'
#' @export
#' @rdname state
state_reset <- function (state) {
  state$repo             <- NULL
  state$task_callback_id <- NA
}


#' @param path directory for the new/existing repository.
#' @param int an [interactions] object.
#'
#' @rdname state
#' @export
open_repository <- function (state, path, int = interactions()) {
  if (file.exists(path)) {
    inform(glue("attaching to repository '{path}'"))
  } else {
    if (isTRUE(int$create_repository())) {
      inform(glue("no repository found, creating one under '{path}'"))
    } else {
      abort(glue("repository '{path}' not found, aborting"))
    }
  }

  state$repo <- repository(filesystem(path, create = TRUE))
}


#' @description `pick_branch` implements the logic of choosing the commit
#' to attach to.
#'
#' @param env [environment] used to find the branch to attach to.
#'
#' @rdname state
#' @export
pick_branch <- function (state, env, int = interactions()) {
  stopifnot(is_interactions(int))

  if (length(ls(env))) {
    m <- as_commits(state$repo) %>% filter(data_matches(data = as.list(env))) %>% read_commits

    # if there is no match but the session is not empty, ask user if the session
    # should be cleaned first; by default int$clean_env() throws an exception
    if (!length(m) && int$clean_env()) {
      rm(list = ls(envir = env, all.names = TRUE), envir = env)
    }

    # if there are multiple matches, ask user which attach to;
    # the default is the most recent one
    if (length(m) > 1) {
      m <- int$choose_commit(m)
    } else {
      inform("global environment matches history, attaching to repository")
      m <- first(m)
    }

    repository_rewind(state$repo, m$id)
    return()
  }

  # if globalenv is empty try attaching to one of the "leaves"
  lv <- as_commits(state$repo) %>% filter(no_children()) %>% read_commits
  if (!length(lv)) {
    n <- as_commits(state$repo) %>% summary(n = n()) %>% first
    if (isTRUE(n > 0)) {
      abort("repository is not empty but there are no leaf commits")
    }

    inform("attached to an empty repository")
    return()
  }

  # finally, if there is more than one leaf, ask the user; by default choose
  # the most recent one
  if (length(lv) > 1) {
    lv <- int$choose_branch(lv)
  } else {
    lv <- first(lv)
  }

  inform(glue("attaching to repository, commit {storage::shorten(lv$id)}"))

  # TODO return commit id and contents, assign to env outside of this function
  repository_rewind(state$repo, lv$id)
  commit_checkout(lv, env)
}


#' @description `interactions` creates an object with a number of callbacks
#' to be used when decision can be delegated to the user. It provides default
#' implementations which either make the most straightforwards decision or
#' abort the call with a descriptive user message.
#'
#' Provided callbacks:
#' * `create_repository` to decide if a new repository should be created
#' * `clean_env` to decide if objects in the global environment should be
#'   removed in order to attach to repository
#' * `choose_commit` when there are multiple commits matching global environment
#' * `choose_branch` when there is more than one branch and global environment
#'   is empty
#'
#' @rdname state
#' @export
interactions <- function (create_repository, clean_env, choose_commit, choose_branch) {
  if (missing(create_repository)) create_repository <- function() TRUE

  if (missing(clean_env)) clean_env <- function() {
    abort("global environment is not empty but there is no match in the history, will not attach")
  }

  if (missing(choose_commit)) choose_commit <- function (commits) {
    inform("global environment matched more than once in history, attaching to the most recent one")
    most_recent(commits)
  }

  if (missing(choose_branch)) choose_branch <- function (commits) {
    inform("repository contains more than one branch, choosing the most recent one")
    most_recent(commits)
  }

  most_recent <- function (commits) {
    nth(commits, last(order(map_int(commits, `[[`, i = 'time'))))
  }

  callbacks <- list(
    create_repository = create_repository,
    clean_env         = clean_env,
    choose_commit     = choose_commit,
    choose_branch     = choose_branch
  )

  structure(callbacks, class = 'interactions')
}

is_interactions <- function(x) inherits(x, 'interactions')


#' @rdname state
#' @export
start_tracking <- function (state, callback_name) {
  if (!is.na(state$task_callback_id)) {
    abort("task callback id found, tracking already started", call = TRUE)
  }

  state$task_callback_id <- addTaskCallback(task_callback, data = state, name = callback_name)

  # TODO see if there is a match for the current globalenv() to continue
  #      work from a given point; if not, ask the user if they want to
  #      rewind to a specific existing commit
}


#' @rdname state
#' @export
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
#' @param state created with [new_state], passed as the `data` argument
#'        to [addTaskCallback]
#'
#' @return A logical value indicating whether to keep this function in
#'         the list of active callbacks.
#'
#' @import grDevices
#' @import utilities
#'
task_callback <- function (expr, result, successful, printed, state) {
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
