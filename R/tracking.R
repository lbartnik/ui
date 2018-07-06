# Track user's interactions with R session.

#' Global state of the tracker.
#'
#' \describe{
#'   \item{repo}{a [repository::repositort] object}
#'   \item{task_callback_id}{id of the callback passed to [addTaskCallback]}
#' }
#'
#' @rdname internal_state
#'
state <- new.env()


#' @description `initiate_state` assigns the default values to all
#' parameters of the global `state` object. By default it:
#' * creates or open a file-system-based [repository::repository]
#' * turns tracking on
#'
#' @rdname internal_state
#'
initiate_state <- function (state)
{
  state$repo             <- NULL
  state$task_callback_id <- NA
}


#' @rdname internal_state
#' @import repository
#' @import storage
#'
open_default_repo <- function (state, env, create = FALSE)
{
  path  <- file.path(getwd(), 'repository')
  store <- storage::filesystem(path, create = create)
  repo  <- repository::repository(store)

  if (isTRUE(getOption("ui.pick_branch", FALSE))) {
    pick_branch(repo, env)
  }

  state$repo <- repo
}


#' @rdname internal_state
#' @import repository
#'
pick_branch <- function (repo, env)
{
  hs <- repository::repository_history(repo)

  if (length(ls(env))) {
    m <- repository::filter(hs, data_matches(data = as.list(env)))
    if (!length(m)) {
      stop("global environment cannot be matched against history, cannot attach to repository",
           call. = FALSE)
    }

    if (length(m) > 1) {
      stop("global environment matched more than once against history, cannot attach to repository",
           call. = FALSE)
    }

    warning("global environment matches history, attaching to repository", call. = FALSE)
    repository::repository_rewind(repo, first(m)$id)

    return()
  }

  # if globalenv is empty try attaching to one of the "leaves"
  lv <- repository::filter(hs, branch_tip())
  if (!length(lv)) return()

  if (length(lv) == 1) {
    warning("attaching to repository, single branch present", call. = FALSE)

    lv <- first(lv)
    repository::repository_rewind(repo, lv$id)
    mapply(names(lv$data), lv$data, FUN = function (name, value) {
      assign(name, value, envir = env)
    })

    return()
  }

  warning("repository contains more than one branch, aborting")
}


#' @rdname internal_state
#'
start_tracking <- function (state)
{
  if (!is.na(state$task_callback_id)) {
    stop("task callback id found, tracking already started")
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
#'
task_callback <- function (expr, result, successful, printed)
{
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
        repository::repository_update(state$repo, globalenv(), plot, expr)
      }
    }
  )

  TRUE
}
