




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
#' @param set `[any vector]` (mandatory, no default)
#' set of values to compare to
#' @param required_class `[character]` (mandatory, no default)
#' class that object must have
#' @param expected_length `[integer]` (mandatory, no default)
#' length object must have
#' @param required_names `[character]` (mandatory, no default)
#' set of names object must have
#' @param classes `[character]` (mandatory, no default)
#' one or more classes; object must have at least one of these as class
#' @param expected_levels `[character]` (mandatory, no default)
#' set of levels factor is required to have
#'
NULL


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
assert_is_one_of <- function(x, x_nm = NULL, fun_nms, funs_arg_list = NULL) {
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

  invisible(NULL)
}




#' @title Between
#' @description
#' Test if elements are between upper and lower bounds.
#' @param x `[R object]` (mandatory, no default)
#' e.g. numeric vector; each element is tested for being between `lo` and `hi`
#' @param lo `[R object]` (mandatory, no default)
#' lower bound of length 1 or `length(x)`
#' @param hi `[R object]` (mandatory, no default)
#' upper bound of length 1 or `length(x)`
#' @param inclusive `[logical]` (optional, default `TRUE`)
#' if `TRUE`, test is `lo <= x & x <= hi` and else `lo < x & x < hi`.
#' @export
is_between <- function(x, lo, hi, inclusive = TRUE) {
  stopifnot(
    length(inclusive) == 1L,
    inclusive %in% c(TRUE, FALSE),
    length(lo) %in% c(1L, length(x)),
    length(hi) %in% c(1L, length(x))
  )
  if (inclusive) {
    lo <= x & x <= hi
  } else {
    lo < x & x < hi
  }
}

#' @describeIn is_between `is_between` with `inclusive = TRUE`
#' @export
is_between_inclusive <- function(x, lo, hi) {
  is_between(x, lo, hi, inclusive = TRUE)
}

#' @describeIn is_between `is_between` with `inclusive = FALSE`
#' @export
is_between_exclusive <- function(x, lo, hi) {
  is_between(x, lo, hi, inclusive = FALSE)
}









