


#' @title Dataset Validation
#' @description
#' Functions to ensure that a dataset (data.frame) looks as it should.
#' @param x `[data.frame]` (mandatory, no default)
#' dataset to validate
#' @name dataset_validation
NULL

#' @rdname dataset_validation
#' @export
#' @param report_df `[data.frame]` (mandatory, no default)
#'
#' a report `data.frame` as produced by e.g. [tests_to_report]
#' @return
#' - `identify_invalid_observations`: returns a data.frame with columns
#'   `is_valid`, `fail_test_set`, and `fail_message_set`; new columns are
#'   populated based on `report_df` contents; the data.frame has `nrow(x)` rows
identify_invalid_observations <- function(
  x,
  report_df
) {
  assert_is_data.frame(x)
  assert_is_data.frame_with_required_names(
    report_df,
    required_names = c("pass", "wh_fail", "test", "message")
  )
  result_df <- data.frame(is_valid = rep(TRUE, nrow(x)))
  result_df[["fail_test_set"]] <- NA_character_
  result_df[["fail_message_set"]] <- NA_character_
  .__fun_env <- environment()
  invisible(lapply(1:nrow(report_df), function(test_no) {
    if (report_df[["pass"]][test_no]) {
      return(NULL)
    }
    wh_fail <- report_df[["wh_fail"]][[test_no]]
    result_df[["is_valid"]][wh_fail] <- FALSE

    fail_test <- report_df[["test"]][test_no]
    fail_test_set <- result_df[["fail_test_set"]][wh_fail]
    fail_test_set <- ifelse(
      is.na(fail_test_set),
      fail_test,
      paste0(fail_test_set, "; ", fail_test)
    )
    .__fun_env$result_df[["fail_test_set"]][wh_fail] <- fail_test_set

    fail_message <- report_df[["message"]][test_no]
    fail_message_set <- result_df[["fail_message_set"]][wh_fail]
    fail_message_set <- ifelse(
      is.na(fail_message_set),
      fail_message,
      paste0(fail_message_set, "; ", fail_message)
    )
    .__fun_env$result_df[["fail_message_set"]][wh_fail] <- fail_message_set
    NULL
  }))

  return(result_df[])
}






