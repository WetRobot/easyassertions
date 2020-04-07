




handle_x_nm_arg <- function(x_nm = NULL) {
  stopifnot(
    (is.character(x_nm) && length(x_nm) == 1L && !is.na(x_nm)) || is.null(x_nm)
  )
  if (is.null(x_nm)) {
    x_nm <- paste0(
      deparse(substitute(x, env = parent.frame(1L))),
      collapse = ""
    )
  }
  x_nm
}





raise_internal_error_if_not <- function(...) {
  test_exprs <- substitute(list(...))
  test_results <- list(...)
  lapply(seq_along(test_results), function(i) {
    test_result <- test_results[[i]]
    if (!is.logical(test_result)) {
      stop("test ", deparse(test_exprs[[i]]),
           " did not evaluate to logical values; ",
           "result had class(es) ", deparse(class(test_result)))
    } else if (!all(test_result)) {
      stop("not all were TRUE: ", deparse(test_exprs[[i]]))
    }
  })
  invisible(NULL)
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

#' @rdname assertions
#' @export
#' @param required_class `[character]` (mandatory, no default)
#'
#' string specifying class that `x` MUST have
assert_has_class <- function(x, x_nm = NULL, required_class) {
  raise_internal_error_if_not(
    length(required_class) == 1,
    is.character(required_class),
    !is.na(required_class)
  )
  x_nm <- handle_x_nm_arg(x_nm)

  if (!inherits(x, required_class)) {
    stop("expected ", deparse(x_nm), " to have class ", deparse(required_class),
         " but it had class(es) ", deparse(class(x)))
  }

  invisible(NULL)
}

#' @rdname assertions
#' @export
#' @param classes `[character]` (mandatory, no default)
#'
#' character vector specifying classes of which `x` MUST have at least one
assert_has_one_of_classes <- function(x, x_nm = NULL, classes) {
  stopifnot(
    length(classes) > 0,
    is.character(classes),
    !is.na(classes)
  )
  x_nm <- handle_x_nm_arg(x_nm)

  if (!inherits(x, classes)) {
    stop("expected ", deparse(x_nm), " to have one of classes ",
         deparse(classes),
         " but it had class(es) ", deparse(class(x)))
  }

  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_number <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!is.numeric(x)) {
    stop(deparse(x_nm), " is not a number")
  }
  invisible(NULL)
}
#' @rdname assertions
#' @export
assert_is_character <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!is.character(x)) {
    stop(deparse(x_nm), " is not a character string object ",
         "(see ?\"character\")")
  }
  invisible(NULL)
}
#' @rdname assertions
#' @export
assert_is_double <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!is.double(x)) {
    stop(deparse(x_nm), " is not a double object (see ?\"double\")")
  }
  invisible(NULL)
}
#' @rdname assertions
#' @export
assert_is_integer <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!is.integer(x)) {
    stop(deparse(x_nm), " is not integer (see ?\"integer\")")
  }
  invisible(NULL)
}
#' @rdname assertions
#' @export
assert_is_logical <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!is.logical(x)) {
    stop(deparse(x_nm), " is not logical (see ?\"logical\")")
  }
  invisible(NULL)
}
#' @rdname assertions
#' @export
assert_is_Date <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!inherits(x, "Date")) {
    stop(deparse(x_nm), " is not a Date object (see ?\"Date\")")
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_factor <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!inherits(x, "factor")) {
    stop(deparse(x_nm), " is not a factor object (see ?\"factor\")")
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
#' @importFrom utils head
assert_is_factor_with_levels <- function(x, x_nm = NULL, expected_levels) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_factor(x, x_nm)
  assert_is_character_nonNA_vector(expected_levels)
  levels <- levels(x)
  extra_levels <- setdiff(levels, expected_levels)
  if (length(extra_levels) > 0) {
    stop("factor ", deparse(x_nm), " had unexpected levels; ",
         "first five extra levels: ",
         deparse(utils::head(extra_levels, 5L)))
  }
  miss_levels <- setdiff(expected_levels, levels)
  if (length(miss_levels) > 0) {
    stop("factor ", deparse(x_nm), " did not have all expected levels; ",
         "first five missing levels: ",
         deparse(utils::head(miss_levels, 5L)))
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_atom <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (length(x) != 1) {
    stop(deparse(x_nm), " had length ", length(x), " but expected it to have ",
         "length 1")
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_vector <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!is.null(dim(x))) {
    stop(deparse(x_nm), " did not have NULL dim")
  }
  if (is.list(x)) {
    stop(deparse(x_nm), " is list-like, not a vector")
  }
  if (!is.vector(x)) {
    stop(deparse(x_nm), " does not pass is.vector(x) test")
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_matrix <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_has_class(x = x, x_nm = x_nm, required_class = "matrix")
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_data.frame <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_has_class(x = x, x_nm = x_nm, required_class = "data.frame")
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_data.table <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_has_class(x = x, x_nm = x_nm, required_class = "data.table")
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_nonNA <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  n_na <- sum(is.na(x))
  if (n_na > 0) {
    stop(deparse(x_nm), " should not have NA values; it had ", n_na,
         " NA values")
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_NULL <- function(
  x,
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (!is.null(x)) {
    stop(deparse(x_nm), " should be NULL; it had class(es) ",
         deparse(class(x)))
  }
}

#' @rdname assertions
#' @export
assert_is_list <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_has_class(x = x, x_nm = x_nm, required_class = "list")
}

#' @rdname assertions
#' @export
assert_is_data_table <- function(
  x,
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_has_class(x = x, x_nm = x_nm, required_class = "data.table")
}

#' @rdname assertions
#' @export
#' @param required_names `[character]` (mandatory, no default)
#' column names that `x` MUST have
assert_has_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_character_nonNA_vector(required_names)
  miss_nms <- setdiff(required_names, names(x))
  if (length(miss_nms)) {
    stop(deparse(x_nm), " did not have these required names: ",
         deparse(miss_nms))
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_named <- function(
  x,
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  if (is.null(names(x))) {
    stop(deparse(x_nm), " did not have names")
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
#' @importFrom utils head
assert_is_uniquely_named <- function(
  x,
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_named(x = x, x_nm = x_nm)
  n_unique_names <- length(unique(names(x)))
  n_x_elems <- length(x)
  if (n_x_elems != n_unique_names) {
    dup_nms <- unique(names(x)[duplicated(names(x))])
    stop(deparse(x_nm), " did not have as many unique names as it has ",
         "elements; it has ", n_x_elems, " elements and it had ",
         n_unique_names, " unique names; first five duplicate names: ",
         deparse(utils::head(dup_nms, 5)))
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
#' @importFrom utils head
#' @param set `[vector]` (mandatory, no default)
#'
#' vector (set, but uniqueness is not asserted) of allowed values
assert_atom_is_in_set <- function(
  x,
  x_nm = NULL,
  set
) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_atom(x = x, x_nm = x_nm)
  raise_internal_error_if_not(
    is.vector(set),
    !is.list(set)
  )
  if (!x %in% set) {
    stop(deparse(x_nm), " was not in set containing (first 10): ",
         deparse(utils::head(set, 10)))
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
#' @importFrom utils head
assert_vector_elems_are_in_set <- function(
  x,
  x_nm = NULL,
  set
) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_vector(x)
  raise_internal_error_if_not(
    is.vector(set),
    !is.list(set)
  )
  bad_levels <- setdiff(x, set)
  if (length(bad_levels) > 0) {
    stop(deparse(x_nm), " had unallowed values (first 10): ",
         deparse(utils::head(bad_levels, 10)), "; the following were allowed ",
         "(first 10): ",
         deparse(utils::head(set, 10)))
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_has_only_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_character_nonNA_vector(required_names)
  miss_nms <- setdiff(required_names, names(x))
  if (length(miss_nms)) {
    stop(deparse(x_nm), " did not have these required names: ",
         deparse(miss_nms))
  }
  extra_nms <- setdiff(names(x), required_names)
  if (length(extra_nms)) {
    stop(deparse(x_nm), " had unexpected names: ",
         deparse(extra_nms))
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
assert_is_data.table_with_required_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  assert_is_data_table_with_required_names(
    x = x,
    x_nm = x_nm,
    required_names = required_names
  )
}

#' @rdname assertions
#' @export
assert_is_data_table_with_required_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_data.table(x)
  assert_is_character_nonNA_vector(required_names)
  miss_nms <- setdiff(required_names, names(x))
  if (length(miss_nms)) {
    stop(deparse(x_nm), " did not have these required names: ",
         deparse(miss_nms))
  }
  invisible(NULL)
}

#' @rdname assertions
#' @export
#' @param required_length `[integer]` (mandatory, no default)
#' `x` MUST be of this length
assert_has_length <- function(x, x_nm = NULL, required_length) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_integer(required_length)
  if (length(x) != required_length) {
    stop(deparse(x_nm), " was not of length ", required_length)
  }
}

#' @rdname assertions
#' @export
#' @param lo `[number]` (mandatory, no default)
#' lower bound for `x`
#'
#' @param hi `[number]` (mandatory, no default)
#' upper bound for `x`
#' @importFrom data.table between
assert_is_between_inclusive <- function(x, x_nm = NULL, lo, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_number(x)
  stopifnot(
    length(lo) %in% c(1L, length(x)),
    length(hi) %in% c(1L, length(x))
  )
  n_not_between <- sum(!data.table::between(
    x = x, lower = lo, upper = hi, incbounds = TRUE
  ))
  if (n_not_between > 0) {
    stop(n_not_between, " values in ", deparse(x_nm), " were not between ",
         lo, " and ", hi, " (inclusive bounds [a, b])")
  }
}
#' @rdname assertions
#' @export
#' @importFrom data.table between
assert_is_between_exclusive <- function(x, x_nm = NULL, lo, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_number(x)
  n_not_between <- sum(!data.table::between(
    x = x, lower = lo, upper = hi, incbounds = FALSE
  ))
  if (n_not_between > 0) {
    stop(n_not_between, " values in ", deparse(x_nm), " were not between ",
         lo, " and ", hi, " (exclusive bounds ]a, b[)")
  }
}
#' @rdname assertions
#' @export
assert_is_gte <- function(x, x_nm = NULL, lo) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_number(x)
  n_not_pass <- sum(x < lo)
  if (n_not_pass > 0) {
    stop(n_not_pass, " values in ", deparse(x_nm), " were not >= ", lo)
  }
}
#' @rdname assertions
#' @export
assert_is_gt <- function(x, x_nm = NULL, lo) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_number(x)
  n_not_pass <- sum(x <= lo)
  if (n_not_pass > 0) {
    stop(n_not_pass, " values in ", deparse(x_nm), " were not > ", lo)
  }
}
#' @rdname assertions
#' @export
assert_is_lte <- function(x, x_nm = NULL, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_number(x)
  n_not_pass <- sum(x > hi)
  if (n_not_pass > 0) {
    stop(n_not_pass, " values in ", deparse(x_nm), " were not <= ", hi)
  }
}
#' @rdname assertions
#' @export
assert_is_lt <- function(x, x_nm = NULL, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_number(x)
  n_not_pass <- sum(x >= hi)
  if (n_not_pass > 0) {
    stop(n_not_pass, " values in ", deparse(x_nm), " were not < ", hi)
  }
}
#' @rdname assertions
#' @export
assert_is_ltezero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_lte(x = x, x_nm = x_nm, hi = 0.0)
}
#' @rdname assertions
#' @export
assert_is_ltzero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_lt(x = x, x_nm = x_nm, hi = 0.0)
}
#' @rdname assertions
#' @export
assert_is_gtezero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_gte(x = x, x_nm = x_nm, lo = 0.0)
}
#' @rdname assertions
#' @export
assert_is_gtzero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  assert_is_gte(x = x, x_nm = x_nm, lo = 0.0)
}


#' @title Generate Assertions
#' @description
#' Generate code for a bunch of assertions functions.
#' @importFrom data.table := CJ setkeyv
#' @param source_script `[character]` (mandatory, no default)
#'
#' path to R script where your assertion functions are
#' @param target_script `[character]` (mandatory, default `"assertions.R"`)
#'
#' path where generated code will be written
#' @param pad `[character]` (mandatory, default `rep("", 5)`)
#'
#' lines between generated function definitions; by default 5 empty lines
#' @export
#' @importFrom data.table CJ setkeyv .SD
generate_assertions <- function(
  source_script = "R/assertions.R",
  target_script = "R/generated_assertions.R",
  pad = rep("", 5)
) {
  lines <- readLines(source_script)

  lines <- pad

  levels <- list(
    c("double", "number", "integer", "Date", "character", "logical",  "factor"),
    "_",
    c("nonNA", ""),
    "_",
    c("gtezero", "gtzero", "ltezero", "ltzero", ""),
    "_",
    c("atom", "vector", "matrix")
  )

  fun_nm_dt <- do.call(data.table::CJ, levels)
  data.table::setkeyv(fun_nm_dt, names(fun_nm_dt))
  non_number_types <- c("character", "logical", "Date", "factor")
  fun_nm_dt <- fun_nm_dt[
    !(fun_nm_dt[["V1"]] %in% non_number_types &
        fun_nm_dt[["V5"]] != ""),
    ]
  fun_nms <- do.call(paste0, fun_nm_dt)
  fun_nms <- paste0("assert_is_", fun_nms)
  fun_nms <- gsub("_{1,}", "_", fun_nms)

  fun_nm_dt[, c("V2", "V4", "V6") := NULL]
  fun_nm_dt[, names(fun_nm_dt) := lapply(.SD, function(col) {
    fun_nms <- paste0("assert_is_", col)
    fun_calls <- paste0(fun_nms, "(x = x, x_nm = x_nm)")
    fun_calls[col == ""] <- ""
    fun_calls
  })]

  fun_definitions <- unlist(lapply(seq_along(fun_nms), function(i) {
    fun_nm <- fun_nms[i]
    def <- paste0(fun_nm, " <- function(x, x_nm = NULL) {")
    def <- c(
      def,
      "  x_nm <- handle_x_nm_arg(x_nm)",
      paste0("  ", setdiff(as.character(fun_nm_dt[i, ]), ""))
      )
    def <- c(def, "}", rep("", 1))
    def <- c(
      "#' @rdname assertions",
      "#' @export",
      def
    )
  }))

  lines <- c(lines, fun_definitions)

  writeLines(text = lines, con = target_script)
  invisible(NULL)
}
generate_assertions()



