

pkgload::load_all(export_all = TRUE)
generate_base_report_funs(
  target_script = "R/generated_base_report_funs.R"
)
generate_function_variants(
  "report_whether",
  source_script = "R/generated_base_report_funs.R",
  target_script = "R/generated_report_fun_variants.R"
)

generate_base_assertion_funs(
  target_script = "R/generated_base_assertion_funs.R"
)
generate_function_variants(
  "assert",
  source_script = "R/generated_base_assertion_funs.R",
  target_script = "R/generated_assertion_fun_variants.R"
)

generate_base_test_funs(
  target_script = "R/generated_base_test_funs.R"
)
generate_function_variants(
  "assert",
  source_script = "R/generated_base_test_funs.R",
  target_script = "R/generated_test_fun_variants.R"
)
