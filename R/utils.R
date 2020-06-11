



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
  fun_nms <- paste0("report_whether_is_", fun_nms)
  fun_nms <- gsub("_{1,}", "_", fun_nms)

  fun_nm_dt[, c("V2", "V4", "V6") := NULL]
  fun_nm_dt[, names(fun_nm_dt) := lapply(.SD, function(col) {
    fun_nms <- paste0("report_whether_is_", col)
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

