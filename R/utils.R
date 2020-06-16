



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



interpolate <- function(x, env = parent.frame(1L)) {
  stopifnot(
    is.character(x),
    is.environment(env)
  )
  m <- gregexpr(pattern = "\\Q${\\E[^{]+\\Q}\\E", text = x, perl = TRUE)
  expr_strings_by_x_elem <- regmatches(x = x, m = m)
  values <- lapply(expr_strings_by_x_elem, function(expr_string_vec) {
    expr_string_vec <- substr(expr_string_vec, 3L, nchar(expr_string_vec) - 1L)
    vapply(expr_string_vec, function(string) {
      as.character(eval(parse(text = string)[[1L]], envir = env))
    }, character(1L))
  })
  regmatches(x = x, m = m) <- values
  x
}



get_report_fun_specs <- function() {
  report_fun_specs
}



#' @importFrom stats aggregate
generate_base_report_funs <- function(
  target_script = "R/generated_base_report_funs.R"
) {
  specs_df <- get_report_fun_specs()
  raise_internal_error_if_not(
    is.data.frame(specs_df),
    c("test_set_nm", "call", "intra_function_action",
      "fail_message", "pass_message", "extra_arg_nm_set") %in% names(specs_df),

    is.character(target_script),
    length(target_script) == 1L
  )

  base_prefix <- "report_whether_"
  fun_df <- data.frame(suffix = sort(unique(specs_df[["test_set_nm"]])))
  fun_df[["nm"]] <- paste0(base_prefix, fun_df[["suffix"]])
  fun_df[["extra_arg_set"]] <- stats::aggregate(
    x = specs_df[["extra_arg_nm_set"]],
    by = list(specs_df[["test_set_nm"]]),
    FUN = function(x) {
      paste0(setdiff(unique(x), c(NA_character_, "")), collapse = ", ")
    }
  )[["x"]]
  fun_df[["test_set"]] <- stats::aggregate(
    x = specs_df[["call"]],
    by = list(specs_df[["test_set_nm"]]),
    FUN = function(x) {
      paste0(deparse(x), collapse = " ")
    }
  )[["x"]]
  fun_df[["fail_msg_set"]] <- stats::aggregate(
    x = specs_df[["fail_message"]],
    by = list(specs_df[["test_set_nm"]]),
    FUN = function(x) {
      paste0(deparse(x), collapse = " ")
    }
  )[["x"]]
  fun_df[["pass_msg_set"]] <- stats::aggregate(
    x = specs_df[["pass_message"]],
    by = list(specs_df[["test_set_nm"]]),
    FUN = function(x) {
      paste0(deparse(x), collapse = " ")
    }
  )[["x"]]
  fun_df[["body"]] <- lapply(1:nrow(fun_df), function(fun_no) {
    body <- paste0("  ", c(
      "x_nm <- handle_x_nm_arg(x_nm)",
      "fun_eval_env <- environment()",
      "test_set <- c(",
      paste0("  ", fun_df[["test_set"]][fun_no]),
      ")",
      "fail_msg_set <- c(",
      paste0("  ", fun_df[["fail_msg_set"]][fun_no]),
      ")",
      "pass_msg_set <- c(",
      paste0("  ", fun_df[["pass_msg_set"]][fun_no]),
      ")",
      "report_df <- tests_to_report(",
      "  tests = test_set,",
      "  fail_messages = fail_msg_set,",
      "  pass_messages = pass_msg_set,",
      "  env = fun_eval_env",
      ")",
      "return(report_df)"
    ))
    body
  })
  fun_df[["fun_def"]] <- lapply(1:nrow(fun_df), function(fun_no) {
    body <- fun_df[["body"]][[fun_no]]
    arg_set <- c("x", "x_nm = NULL")
    arg_set <- setdiff(
      c(arg_set, fun_df[["extra_arg_set"]][fun_no]),
      c(NA_character_, "")
    )
    arg_set <- paste0(arg_set, collapse = ", ")
    def <- c(
      "#' @rdname report_whether",
      "#' @export",
      paste0(fun_df[["nm"]][fun_no], " <- function(", arg_set, ") {"),
      body,
      "}"
    )
  })

  lines <- c(
    "# this script was generated automatically. do not edit by hand!",
    rep("", 5),
    unlist(lapply(fun_df[["fun_def"]], function(x) {
      c(
        "# this function was generated automatically. do not edit by hand!",
        x,
        rep("", 3)
      )
    }))
  )

  writeLines(text = lines, con = target_script)
}


generate_assertion_funs <- function(
  source_scripts = c(
    "R/generated_base_report_funs.R",
    "R/generated_report_fun_variants.R"
  ),
  target_script = "R/generated_assertion_funs.R"
) {
  fun_env <- new.env()
  invisible(lapply(source_scripts, function(script_path) {
    source(script_path, local = fun_env)
  }))

  obj_nms <- ls(envir = fun_env)
  is_report_fun_nm <- grepl(
    pattern = "^report_whether_[_.a-zA-Z0-9]+",
    x = obj_nms
  )
  report_fun_nms <- gsub("\\Q <- \\E.+", "", obj_nms[is_report_fun_nm])
  assert_fun_nms <- sub("^report_whether_", "assert_", report_fun_nms)

  fun_df <- data.frame(fun_nm = assert_fun_nms, report_fun_nm = report_fun_nms)

  fun_df[["body"]] <- lapply(1:nrow(fun_df), function(fun_no) {
    paste0("  ", c(
      "x_nm <- handle_x_nm_arg(x_nm)",
      paste0("report_fun_nm <- \"", fun_df[["report_fun_nm"]][fun_no], "\""),
      "arg_list <- mget(names(formals(report_fun_nm)))",
      "report_df <- do.call(report_fun_nm, arg_list)",
      "if (any(!report_df[[\"pass\"]])) {",
      "  wh_first_fail <- which(report_df[[\"pass\"]])[1L]",
      "  stop(report_df[[\"message\"]][wh_first_fail])",
      "}",
      "return(invisible(NULL))"
    ))
  })

  fun_df[["arg_set"]] <- lapply(report_fun_nms, function(report_fun_nm) {
    formals <- formals(fun_env[[report_fun_nm]])
    arg_set <- vapply(
      seq_along(formals),
      function(formal_no) {
        formal_nm <- names(formals)[formal_no]
        formal_default <- paste0(deparse(formals[[formal_no]]), collapse = "")
        ifelse(formal_default %in% c("", NA_character_), formal_nm,
               paste0(formal_nm, " = ", formal_default))
      },
      character(1L)
    )
    arg_set
  })

  fun_df[["def"]] <- lapply(1:nrow(fun_df), function(fun_no) {
    fun_nm <- fun_df[["fun_nm"]][fun_no]
    body <- fun_df[["body"]][[fun_no]]
    arg_set <- fun_df[["arg_set"]][[fun_no]]
    def <- c(
      paste0(fun_nm, " <- function("),
      paste0("  ", arg_set, c(rep(", ", length(arg_set) - 1L), "")),
      ") {",
      body,
      "}"
    )
    def
  })

  lines <- unlist(lapply(fun_df[["def"]], function(lines) {
    c(
      rep("", 5),
      lines
    )
  }))

  lines <- c(lines, rep("", 5))

  writeLines(lines, con = target_script)

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
generate_function_variants <- function(
  prefix = c("report_whether", "assert", "test")[1],
  target_script = "R/generated_report_fun_variants.R",
  pad = rep("", 5)
) {

  levels <- list(
    c("double", "number", "integer", "Date", "character", "logical",  "factor"),
    "_",
    c("nonNA", ""),
    "_",
    c("gtezero", "gtzero", "ltezero", "ltzero", ""),
    "_",
    c("atom", "vector", "matrix")
  )

  fun_def_prefix <- paste0(prefix, "_is_")

  fun_nm_dt <- do.call(data.table::CJ, levels)
  data.table::setkeyv(fun_nm_dt, names(fun_nm_dt))
  non_number_types <- c("character", "logical", "Date", "factor")
  fun_nm_dt <- fun_nm_dt[
    !(fun_nm_dt[["V1"]] %in% non_number_types &
        fun_nm_dt[["V5"]] != ""),
    ]
  fun_nms <- do.call(paste0, fun_nm_dt)
  fun_nms <- paste0(fun_def_prefix, fun_nms)
  fun_nms <- gsub("_{1,}", "_", fun_nms)

  fun_nm_dt[, c("V2", "V4", "V6") := NULL]
  fun_nm_dt[, names(fun_nm_dt) := lapply(.SD, function(col) {
    fun_nms <- paste0(fun_def_prefix, col)
    fun_calls <- paste0(fun_nms, "(x = x, x_nm = x_nm)")
    fun_calls[col == ""] <- ""
    fun_calls
  })]


  fun_definitions <- unlist(lapply(seq_along(fun_nms), function(i) {
    fun_nm <- fun_nms[i]
    def <- paste0(fun_nm, " <- function(x, x_nm = NULL) {")
    call_lines <- setdiff(as.character(fun_nm_dt[i, ]), "")
    line_ends <- c(rep(", ", length(call_lines) - 1L), "")
    def <- c(
      def,
      "  x_nm <- handle_x_nm_arg(x_nm)",
      "  out <- rbind(",
      paste0("    ", call_lines, line_ends),
      "  )",
      "  return(out)"
    )
    def <- c(def, "}", rep("", 1))
    def <- c(
      paste0("#' @rdname ", prefix),
      "#' @export",
      def
    )
  }))

  lines <- c(pad, fun_definitions)

  writeLines(text = lines, con = target_script)
  invisible(NULL)
}

