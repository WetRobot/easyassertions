


tests_to_report <- function(
  tests,
  fail_messages = "test was not TRUE: %%test%%",
  pass_messages = "test was TRUE: %%test%%",
  env = parent.frame(1L)
) {
  stopifnot(
    is.character(tests),
    length(tests) > 0L,
    !is.na(tests),

    is.character(fail_messages),
    length(fail_messages) %in% c(1L, length(tests)),

    is.character(pass_messages),
    length(pass_messages) %in% c(1L, length(tests)),

    is.environment(env)
  )

  if (length(fail_messages) == 1L) {
    fail_messages <- rep(fail_messages, length(tests))
  }
  fail_messages[is.na(fail_messages)] <- paste0(
    "test failed: ", tests[is.na(fail_messages)]
  )
  if (length(pass_messages) == 1L) {
    pass_messages <- rep(pass_messages, length(tests))
  }
  pass_messages[is.na(pass_messages)] <- paste0(
    "test passed: ", tests[is.na(pass_messages)]
  )

  eval_env <- new.env(parent = env)
  test_pos_set <- seq_along(tests)
  do.call(rbind, lapply(test_pos_set, function(test_pos) {
    test_string <- tests[test_pos]
    test_expr <- parse(text = test_string)[[1L]]

    result <- withCallingHandlers(
      eval(test_expr, envir = eval_env),
      error = function(e) paste0("ERROR: ", e[["message"]], collapse = "")
    )

    df <- data.frame(
      test = test_string,
      result = result,
      pass = isTRUE(all.equal(result, TRUE))
    )
    if (df[["pass"]]) {
      df[["message"]] <- interpolate(pass_messages[test_pos], env = eval_env)
    } else {
      df[["message"]] <- interpolate(fail_messages[test_pos], env = eval_env)
    }
    df[["message"]] <- gsub("\\Q%%test%%\\E", df[["test"]], df[["message"]])
    df[]
  }))
}



#' @title Assertions
#' @description
#' Collection of assertion functions which raise an error if the test does not
#' pass.
#' @name assertions
#' @param x R object to be tested (mandatory, no default)
#' @param x_nm `[NULL, character]` (optional, default `NULL`)
#'
#' the name of the object `x` to mention in possible error message.
#' - `NULL`: taken as `deparse(substitute(x))`
#' - `character`: the name as a string
#' @param lo `[number]` (mandatory, no default)
#' lower bound for `x`
#'
#' @param hi `[number]` (mandatory, no default)
#' upper bound for `x`
#' @importFrom data.table between


#' @rdname assertions
#' @export
#' @param fun_nms `[character]` (mandatory, no default)
#'
#' names of assertion funs to run
#' @param funs_arg_list `[list, NULL]` (optional, default `NULL`)
#'
#' - `NULL`: no additional arguments are passed to functions specified in
#'   `fun_nms`
#' - `list`: named elements of this list are matched to the names of arguments
#'   of each function specified in `fun_nms`, and matching arguments are
#'   used; e.g. if one fun has arg `y` and other has `z`, you may pass
#'   `list(y = my_y, z = my_z)`.
report_whether_is_one_of <- function(x, x_nm = NULL, fun_nms, funs_arg_list = NULL) {
  raise_internal_error_if_not(
    length(fun_nms) > 0,
    is.character(fun_nms),
    !is.na(fun_nms),
    vapply(fun_nms, function(fun_nm) {
      is.function(get(fun_nm))
    }, logical(1)),

    is.null(funs_arg_list) || inherits(funs_arg_list, "list")
  )

  funs <- lapply(fun_nms, match.fun)
  funs_arg_list <- as.list(funs_arg_list)
  tries <- lapply(funs, function(fun) {
    arg_list <- formals(fun)
    arg_list[c("x", "x_nm")] <- list(x = quote(x), x_nm = quote(x_nm))
    supplied_arg_nms <- intersect(names(arg_list), names(funs_arg_list))
    arg_list[supplied_arg_nms] <- funs_arg_list[supplied_arg_nms]
    tryCatch(
      do.call(fun, arg_list),
      error = function(e) {
        e
      }
    )
  })

  is_error <- vapply(tries, inherits, logical(1), what = "error")
  if (all(is_error)) {
    error_msgs <- vapply(tries, function(error_obj) {
      paste0(error_obj[["message"]], collapse = "")
    }, character(1))
    error_msgs <- paste0("    - ", fun_nms, ": ", error_msgs, collapse = "\n")
    stop("at least one of the following assertions must pass:\n", error_msgs)
  }

  return(report_df)
}



















