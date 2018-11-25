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


#' @param x a [repository::repository] object or a directory path for
#'        the new/existing repository; or object to be tested.
#' @param int an [interactions] object.
#'
#' @importFrom rlang is_character
#' @rdname state
#' @export
open_repository <- function (state, x, int = interactions()) {
  # it's either a repository object or a path
  if (is_repository(x)) {
    state$repo <- x
    return(TRUE)
  }

  if (!is_character(x)) {
    abort("`x` is not repository object nor a path")
  }

  # if a path, see if needs and can be created
  if (file.exists(x)) {
    inform(glue("attaching to repository '{x}'"))
  } else {
    if (isTRUE(int$create_repository())) {
      inform(glue("no repository found, creating one under '{x}'"))
    } else {
      abort(glue("repository '{x}' not found, aborting"))
    }
  }

  state$repo <- repository(filesystem(x, create = TRUE))
  TRUE
}


#' @description `pick_branch` implements the logic of choosing the commit
#' to attach to.
#'
#' @param env [environment] used to find the branch to attach to.
#'
#' @rdname state
#' @export
pick_branch <- function (state, env, int = interactions()) {
  # TODO return commit id and contents, assign to env outside of this function
  stopifnot(is_interactions(int))

  # is there anything in the repository?
  n_co <- as_commits(state$repo) %>% summary(n = n()) %>% first
  n_en <- length(ls(env)) # ignore hidden objects in global environment

  # if repository is empty, it's quite simple
  if (identical(n_co, 0L)) {
    # if environment is empty simply return right away
    if (identical(n_en, 0L)) {
      inform("attached to an empty repository")
      return()
    }

    # otherwise, see if the user wants to initialize the repository with
    # the contents of the environment; if not, throw an error
    if (!int$create_first_commit()) {
      abort("repository is empty but session contains data")
    }

    # finally, initialize the repository and exit
    inform("creating the first commit in the repository")
    repository_update(state$repo, env, NULL, bquote())

    return(TRUE)
  }

  # if session environment is not empty, try to identify a matching
  # commit or ask the user to clean the session
  if (!identical(n_en, 0L)) {
    matching <- as_commits(state$repo) %>% filter(data_matches(data = as.list(env))) %>% read_commits
    n_ma <- length(matching)

    # if there are matches, pick the one to attach to
    if (!identical(n_ma, 0L)) {
      if (identical(n_ma, 1L)) {
        commit <- first(matching)
        cinform("only commit", green = shorten(commit$id), "matches the session")
      } else {
        # if there are multiple matches, ask user which attach to;
        # the default is the most recent one
        commit <- int$choose_commit(matching)
      }

      repository_rewind(state$repo, commit$id)
      return()
    }

    # if nothing matches, ask about removing session data
    if (!int$clean_env()) {
      abort("global environment is not empty but there is no match in the history, will not attach")
    }

    inform("session does not match any commits, removing session data")
    rm(list = ls(envir = env, all.names = TRUE), envir = env)
  }

  # now we are sure the session environment is empty: either because it
  # was from the beginning or because the user decided to remove all objects;
  # we can safely proceed to picking the branch to attach to - that is, one
  # from the commits that themselves have no child commits
  leaves <- as_commits(state$repo) %>% filter(no_children()) %>% read_commits
  if (!length(leaves)) {
    abort("no leaf commits, repository seems broken")
  }

  if (identical(length(leaves), 1L)) {
    commit <- first(leaves)
  } else {
    commit <- int$choose_branch(leaves)
  }

  cinform("attaching to commit", green = storage::shorten(commit$id))
  repository_rewind(state$repo, commit$id)
  commit_checkout(commit, env)
}


#' @description `interactions` creates an object with a number of callbacks
#' to be used when decision can be delegated to the user. It provides default
#' implementations which either make the most straightforwards decision or
#' abort the call with a descriptive user message.
#'
#' Provided callbacks:
#' * `create_first_commit` to decide whether load the contents or R session
#'   into an empty repository as its first commit
#' * `create_repository` to decide if a new repository should be created
#' * `clean_env` to decide if objects in the global environment should be
#'   removed in order to attach to repository
#' * `choose_commit` when there are multiple commits matching global environment
#' * `choose_branch` when there is more than one branch and global environment
#'   is empty
#'
#' @param create_first_commit callback function
#' @param create_repository callback function
#' @param clean_env callback function
#' @param choose_commit callback function
#' @param choose_branch callback function
#'
#' @rdname state
#' @export
interactions <- function (create_first_commit, create_repository, clean_env, choose_commit, choose_branch) {
  if (missing(create_first_commit)) create_first_commit <- function () FALSE

  if (missing(create_repository)) create_repository <- function() TRUE

  if (missing(clean_env)) clean_env <- function() FALSE

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


#' @param callback_name passed to [addTaskCallback] as `name`
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
