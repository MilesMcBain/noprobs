##' Capture errors warnings and messages in a tidy format
##'
##' Outputs a 1 row tibble containing columns: .errors, .warnings, .messages and
##' the name supplied in .result_col
##'
##' This is advantageous in tidy modeling workflows where you can use no_problem with map
##' 
##' @title no_problem
##' @param expr an expression to evaluate
##' @param .result_col the name of the result column 
##' @return a one row tibble with error data
##' @author Miles McBain
##' @export
no_problem <- function(expr, .result_col = ".result") {

  warnings <- list(NULL)
  messages <- list(NULL)
  errors <- list(NULL)
  result <- list(NULL)

  `%null|l%` <- function(l,r) {
    if (is.null(l[[1]])) list(r)
    else list(c(unlist(l),r))
  }

  handle_warning <- function(w) {
    warnings <<- warnings %null|l% w$message
    invokeRestart("muffleWarning")
  }

  handle_message <- function(m) {
    messages <<- messages %null|l% m$message
    invokeRestart("muffleMessage")
  }

  handle_error <- function(e) {
    errors <<- errors %null|l% e$message
  }

  result <- withCallingHandlers(
    tryCatch(
      expr,
      error = handle_error
    ),
    warning =  handle_warning,
    message = handle_message)

  tibble::tibble(!!result_col := list(result),
                 .warnings = warnings,
                 .messages = messages,
                 .errors = errors
                 )

}

##' Check if there are any problems in unnested error data
##'
##' .. content for \details{} ..
##' @title any_problems
##' @param  a_df 
##' @return a boolen indicating the presence of data in .warnings, .errors, or .messages
##' @author Miles McBain
any_problems <- function(a_df) {

  is_clean <-
    a_df[,c(".warnings", ".errors", ".messages")] %>%
    purrr::map_lgl(.f = function(x) all(purrr::map_lgl(x, is.null))) %>%
    all()

  !is_clean

}
