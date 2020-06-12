


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
    !is.na(fail_messages),

    is.character(pass_messages),
    length(pass_messages) %in% c(1L, length(tests)),
    !is.na(pass_messages),

    is.environment(env)
  )

  if (length(fail_messages) == 1L) {
    fail_messages <- rep(fail_messages, length(tests))
  }
  if (length(pass_messages) == 1L) {
    pass_messages <- rep(pass_messages, length(tests))
  }

  test_pos_set <- seq_along(tests)
  do.call(rbind, lapply(test_pos_set, function(test_pos) {
    test_string <- tests[test_pos]
    test_expr <- parse(text = test_string)[[1L]]

    result <- tryCatch(
      eval(test_expr, envir = env),
      error = function(e) paste0("ERROR: ", e[["message"]], collapse = ""),
      warning = function(w) paste0("WARNING: ", w[["message"]], collapse = "")
    )

    df <- data.frame(
      test = test_string,
      result = result,
      pass = isTRUE(all.equal(result, TRUE))
    )
    if (df[["pass"]]) {
      df[["message"]] <- interpolate(pass_messages[test_pos], env = env)
    } else {
      df[["message"]] <- interpolate(fail_messages[test_pos], env = env)
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



#' @rdname assertions
#' @export
report_whether_dir_exists <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- report_whether_is_character_nonNA_atom(x, x_nm)
  if (report_df[["pass"]]) {
    report_df <- rbind(
      report_df,
      tests_to_report(
        tests = "dir.exists(x)",
        fail_messages = "No such directory: \"${x}\""
      )
    )
  }
  return(report_df)
}


#' @rdname assertions
#' @export
report_whether_file_exists <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- report_whether_is_character_nonNA_atom(x, x_nm)
  if (report_df[["pass"]]) {
    report_df <- rbind(
      report_df,
      tests_to_report(
        tests = "file.exists(x)",
        fail_messages = "No such file: \"${x}\""
      )
    )
  }
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_function <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_whether_has_class(x = x, x_nm = x_nm, required_class = "function")
}

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

#' @rdname assertions
#' @export
#' @param required_class `[character]` (mandatory, no default)
#'
#' string specifying class that `x` MUST have
report_whether_has_class <- function(x, x_nm = NULL, required_class) {
  raise_internal_error_if_not(
    length(required_class) == 1,
    is.character(required_class),
    !is.na(required_class)
  )
  x_nm <- handle_x_nm_arg(x_nm)

  report_df <- tests_to_report(
    tests = paste0("inherits(x, \"", required_class, "\")"),
    fail_messages = paste0(
      "expected \"${x_nm}\" to have class \"${required_class}\" ",
      "but it had class(es) ${deparse(class(x))}"
    )
  )

  return(report_df)
}

#' @rdname assertions
#' @export
#' @param classes `[character]` (mandatory, no default)
#'
#' character vector specifying classes of which `x` MUST have at least one
report_whether_has_one_of_classes <- function(x, x_nm = NULL, classes) {
  raise_internal_error_if_not(
    length(classes) > 0,
    is.character(classes),
    !is.na(classes)
  )
  x_nm <- handle_x_nm_arg(x_nm)

  report_df <- tests_to_report(
    tests = "inherits(x, classes)",
    fail_messages = paste0(
      "expected \"${x_nm}\" to have one of classes \"${classes}\" ",
      "but it had class(es) ${deparse(class(x))}"
    )
  )

  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_number <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "is.numeric(x)",
    fail_messages = "\"${x_nm}\" is not a number (float or integer)"
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_character <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "is.character(x)",
    fail_messages = "\"${x_nm}\" is not a character object; see ?character"
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_double <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "is.double(x)",
    fail_messages = "\"${x_nm}\" is not a double object; see ?double"
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_integer <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "is.integer(x)",
    fail_messages = "\"${x_nm}\" is not an integer object; see ?integer"
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_logical <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "is.integer(x)",
    fail_messages = "\"${x_nm}\" is not a logical object; see ?logical"
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_Date <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "inherits(x, \"Date\")",
    fail_messages = "\"${x_nm}\" is not a Date object; see ?Date / ?as.Date"
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_factor <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "is.factor(x)",
    fail_messages = "\"${x_nm}\" is not a factor object; see ?factor"
  )
  return(report_df)
}

#' @rdname assertions
#' @export
#' @importFrom utils head
#' @param expected_levels `[character]` (mandatory, no default)
#'
#' exact character string vector of levels that `x` must have; i.e.
#' `levels(x)` and `expected_levels` must be the same set and they must be
#' in the same order
report_whether_is_factor_with_levels <- function(x, x_nm = NULL, expected_levels) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- report_whether_is_factor(x, x_nm)
  if (report_df[["pass"]]) {
    report_df <- rbind(
      report_df,
      report_whether_is_character_nonNA_vector(expected_levels)
    )
  }

  if (report_df[["pass"]][nrow(report_df)]) {
    levels <- levels(x)
    extra_levels <- setdiff(levels, expected_levels)
    miss_levels <- setdiff(expected_levels, levels)
    report_df <- rbind(
      report_df,
      tests_to_report(
        tests = c(
          "length(extra_levels) > 0",
          "length(miss_levels) > 0",
          "!identical(as.character(levels), as.character(expected_levels))"
        ),
        fail_messages = c(
          paste0(
            "factor \"${x_nm}\" had unexpected levels; ",
            "first five extra levels: ${deparse(utils::head(extra_levels, 5L))}"
          ),
          paste0(
            "factor \"${x_nm}\" did not have all expected levels; ",
            "first five missing levels: ${deparse(utils::head(miss_levels, 5L))}"
          ),
          paste0(
            "factor \"${x_nm}\" levels were not in the expected oreder; ",
            "first five levels in \"${x_nm}\" in bad order: ",
            "${deparse(utils::head(levels[levels != expected_levels], 5L))}"
          )
        )
      )
    )
  }
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_atom <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- report_whether_has_length(
    x = x, x_nm = x_nm, required_length = 1L
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_vector <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = c("is.null(dim(x))", "!is.list(x)", "is.vector(x)"),
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_matrix <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- report_whether_has_class(
    x = x, x_nm = x_nm, required_class = "matrix"
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_data.frame <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- report_whether_has_class(
    x = x, x_nm = x_nm, required_class = "data.frame"
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_data.table <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- report_whether_has_class(
    x = x, x_nm = x_nm, required_class = "data.table"
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_nonNA <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "sum(is.na(x)) == 0L",
    fail_messages = paste0(
      "\"${x_nm}\" should not have NA values; it had ${sum(is.na(x))} ",
      "NA values out of ${length(x)} elements"
    )
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_NULL <- function(
  x,
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_whether_has_class(
    x = x,
    x_nm = x_nm,
    required_class = "NULL"
  )
}

#' @rdname assertions
#' @export
report_whether_is_list <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_whether_has_class(x = x, x_nm = x_nm, required_class = "list")
}

#' @rdname assertions
#' @export
report_whether_is_named_list <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- rbind(
    report_whether_has_class(x = x, x_nm = x_nm, required_class = "list"),
    report_whether_is_named(x, x_nm)
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_uniquely_named_list <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- rbind(
    report_whether_has_class(x = x, x_nm = x_nm, required_class = "list"),
    report_whether_is_uniquely_named(x, x_nm)
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_data_table <- function(
  x,
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_whether_has_class(x = x, x_nm = x_nm, required_class = "data.table")
}

#' @rdname assertions
#' @export
#' @param required_names `[character]` (mandatory, no default)
#' column names that `x` MUST have
report_whether_has_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(
    is.character(required_names),
    !is.na(required_names)
  )
  miss_nms <- setdiff(required_names, names(x))
  report_df <- tests_to_report(
    tests = "length(miss_nms) == 0L",
    fail_messages = paste0(
      "\"${x_nm}\" did not have these required names: ${deparse(miss_nms)}"
    )
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_named <- function(
  x,
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- tests_to_report(
    tests = "!is.null(names(x))",
    fail_messages = "\"${x_nm}\" did not have names"
  )
  return(report_df)
}

#' @rdname assertions
#' @export
#' @importFrom utils head
report_whether_is_uniquely_named <- function(
  x,
  x_nm = NULL
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- report_whether_is_named(x = x, x_nm = x_nm)
  if (report_df[["pass"]]) {
    n_unique_names <- length(unique(names(x)))
    n_x_elems <- length(x)
    dup_nms <- names(x)[duplicated(names(x))]
    report_df <- rbind(
      report_df,
      tests_to_report(
        tests = "n_x_elems == n_unique_names",
        fail_messages = paste0(
          "${deparse(x_nm)} did not have as many unique names as it has ",
          "elements; it has ${n_x_elems} elements and it had ",
          "${n_unique_names} unique names; first five duplicate names: ",
          "${deparse(utils::head(dup_nms, 5))}"
        )
      )
    )
  }
  return(report_df)
}

#' @rdname assertions
#' @export
#' @importFrom utils head
#' @param set `[vector]` (mandatory, no default)
#'
#' vector (set, but uniqueness is not asserted) of allowed values
report_whether_atom_is_in_set <- function(
  x,
  x_nm = NULL,
  set
) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(
    is.vector(set),
    !is.list(set)
  )
  report_df <- report_whether_is_atom(x = x, x_nm = x_nm)
  if (report_df[["pass"]]) {
    report_df <- rbind(
      report_df,
      tests_to_report(
        tests = "x %in% set",
        fail_messages = paste0(
          "${deparse(x_nm)} was not in set containing (first 10): ",
          "${deparse(utils::head(set, 10))}"
        )
      )
    )
  }
  return(report_df)
}

#' @rdname assertions
#' @export
#' @importFrom utils head
report_whether_vector_elems_are_in_set <- function(
  x,
  x_nm = NULL,
  set
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_whether_is_vector(x)
  raise_internal_error_if_not(
    is.vector(set),
    !is.list(set)
  )
  bad_levels <- setdiff(x, set)
  report_df <- tests_to_report(
    tests = "length(bad_levels) == 0L",
    fail_messages = paste0(
      "${deparse(x_nm)} had unallowed values (first 10): ",
      "${deparse(utils::head(bad_levels, 10))}; the following were allowed ",
      "(first 10): ${deparse(utils::head(set, 10))}"
    ),
    pass_messages = "all values of ${x_nm} were in the set of expected values"

  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_has_only_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(
    is.character(required_names),
    !is.na(required_names)
  )
  miss_nms <- setdiff(required_names, names(x))
  extra_nms <- setdiff(names(x), required_names)
  report_df <- tests_to_report(
    tests = c("length(miss_nms) == 0L", "length(extra_nms) == 0L"),
    fail_messages = c(
      paste0(
        deparse(x_nm), " did not have these required names: ",
        deparse(miss_nms)
      ),
     paste0(
       deparse(x_nm), " had unexpected names: ",
       deparse(extra_nms)
     )
    )
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_data.table_with_required_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  report_df <- report_whether_is_data_table_with_required_names(
    x = x,
    x_nm = x_nm,
    required_names = required_names
  )
  return(report_df)
}

#' @rdname assertions
#' @export
report_whether_is_data_table_with_required_names <- function(
  x,
  x_nm = NULL,
  required_names
) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_df <- rbind(
    report_whether_is_data.table(x = x, x_nm = x_nm),
    report_whether_has_names(
      x = x, x_nm = x_nm, required_names = required_names
    )
  )
  return(report_df)
}

#' @rdname assertions
#' @export
#' @param required_length `[integer]` (mandatory, no default)
#' `x` MUST be of this length
report_whether_has_length <- function(x, x_nm = NULL, required_length) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(
    is.integer(required_length),
    !is.na(required_length),
    length(required_length) == 1L
  )
  report_df <- tests_to_report(
    tests = "length(x) == required_length",
    fail_messages = "${deparse(x_nm)}, was not of length ${required_length}"
  )
  return(report_df)
}

#' @rdname assertions
#' @export
#' @param lo `[number]` (mandatory, no default)
#' lower bound for `x`
#'
#' @param hi `[number]` (mandatory, no default)
#' upper bound for `x`
#' @importFrom data.table between
report_whether_is_between_inclusive <- function(x, x_nm = NULL, lo, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(
    is.numeric(x),
    length(lo) %in% c(1L, length(x)),
    length(hi) %in% c(1L, length(x))
  )
  n_not_between <- sum(!data.table::between(
    x = x, lower = lo, upper = hi, incbounds = TRUE
  ))
  report_df <- tests_to_report(
    tests = "n_not_between == 0L",
    fail_messages = paste0(
      n_not_between, " values in ", deparse(x_nm), " were not between ",
      lo, " and ", hi, " (inclusive bounds [a, b])"
    )
  )
  return(report_df)
}

#' @rdname assertions
#' @export
#' @importFrom data.table between
report_whether_is_between_exclusive <- function(x, x_nm = NULL, lo, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  report_whether_is_number(x)
  n_not_between <- sum(!data.table::between(
    x = x, lower = lo, upper = hi, incbounds = FALSE
  ))
  report_df <- tests_to_report(
    tests = "n_not_between == 0L",
    fail_messages = paste0(
      n_not_between, " values in ", deparse(x_nm), " were not between ",
      lo, " and ", hi, " (exclusive bounds ]a, b[)"
    )
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_gte <- function(x, x_nm = NULL, lo) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(is.numeric(x))
  n_not_pass <- sum(x < lo)
  report_df <- tests_to_report(
    tests = "n_not_pass == 0L",
    fail_messages = paste0(
      n_not_pass, " values in ", deparse(x_nm), " were not >= ", lo
    )
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_gt <- function(x, x_nm = NULL, lo) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(is.numeric(x))
  n_not_pass <- sum(x <= lo)
  report_df <- tests_to_report(
    tests = "n_not_pass == 0L",
    fail_messages = paste0(
      n_not_pass, " values in ", deparse(x_nm), " were not > ", lo
    )
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_lte <- function(x, x_nm = NULL, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(is.numeric(x))
  n_not_pass <- sum(x > hi)
  report_df <- tests_to_report(
    tests = "n_not_pass == 0L",
    fail_messages = paste0(
      n_not_pass, " values in ", deparse(x_nm), " were not <= ", hi
    )
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_lt <- function(x, x_nm = NULL, hi) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(is.numeric(x))
  n_not_pass <- sum(x >= hi)
  report_df <- tests_to_report(
    tests = "n_not_pass == 0L",
    fail_messages = paste0(
      n_not_pass, " values in ", deparse(x_nm), " were not < ", hi
    )
  )
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_ltezero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(is.numeric(x))
  report_df <- report_whether_is_lte(x = x, x_nm = x_nm, hi = 0.0)
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_ltzero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(is.numeric(x))
  report_df <- report_whether_is_lt(x = x, x_nm = x_nm, hi = 0.0)
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_gtezero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(is.numeric(x))
  report_df <- report_whether_is_gte(x = x, x_nm = x_nm, lo = 0.0)
  return(report_df)
}
#' @rdname assertions
#' @export
report_whether_is_gtzero <- function(x, x_nm = NULL) {
  x_nm <- handle_x_nm_arg(x_nm)
  raise_internal_error_if_not(is.numeric(x))
  report_df <- report_whether_is_gte(x = x, x_nm = x_nm, lo = 0.0)
  return(report_df)
}



















