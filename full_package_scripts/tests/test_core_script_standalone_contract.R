#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

test_dir <- tryCatch({
  frames <- sys.frames()
  ofiles <- vapply(frames, function(f) if (!is.null(f$ofile)) as.character(f$ofile)[1] else NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) {
    dirname(normalizePath(ofiles[length(ofiles)], mustWork = FALSE))
  } else {
    file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)) else getwd()
  }
}, error = function(e) getwd())
bundle_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

results <- data.table(test = character(), status = character(), detail = character())
add_result <- function(test, ok, detail = "") {
  results <<- rbind(
    results,
    data.table(test = test, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = as.character(detail)),
    use.names = TRUE
  )
  if (!isTRUE(ok)) stop("FAILED: ", test, if (nzchar(detail)) paste0(" -- ", detail) else "")
  invisible(TRUE)
}

run_clean_r <- function(expr) {
  code <- paste0("setwd(", shQuote(bundle_dir), "); ", expr)
  out <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", "-e", shQuote(code)), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  list(ok = is.null(status) || identical(status, 0L), output = paste(out, collapse = "\n"))
}

hier_direct <- run_clean_r(paste(
  "library(data.table)",
  "source('hier_mmm.R')",
  "dt <- data.table(week=seq.Date(as.Date('2024-01-07'), by='week', length.out=16), geo='G1', entity='brand', tv=seq(10, 40, length.out=16), y=100+seq_len(16))",
  "md <- data.table(variable='tv', role='media', curve_type='hill', anchor_saturation=0.35, coef=0.05, coef_precision=25, coef_bound='pos')",
  "prep <- prepare_stan_data_hier_mmm(dt, md, dep_var_col='y', group_col='geo', time_col='week', entity_col='entity', intercept_type='flat', sample_curve_parameters='never')",
  "stopifnot(is.function(fit_hier_mmm), is.function(build_response_curves_hier_mmm), is.function(media_transform_hier_mmm))",
  "stopifnot(prep$metadata[variable=='tv', curve_type][1] == 'hill', prep$metadata[variable=='tv', isTRUE(cvalue_from_anchor)][1], prep$stan_data$curve_type[1] == 2L)",
  "cat('hier direct ok\\n')",
  sep = "; "
))
add_result("hier_mmm.R stands alone for Stan data and curve metadata", hier_direct$ok, hier_direct$output)

quasi_direct <- run_clean_r(paste(
  "source('quasi_geo_test.R')",
  "stopifnot(is.function(run_quasi_geo_test), is.function(qgt_build_stan_prior_handoff), is.function(qgt_apply_stan_prior_handoff))",
  "cat('quasi direct ok\\n')",
  sep = "; "
))
add_result("quasi_geo_test.R exposes geo evidence and Stan handoff without prior workflow", quasi_direct$ok, quasi_direct$output)

optimizer_direct <- run_clean_r(paste(
  "library(data.table)",
  "source('optimizer_scenario_planner.R')",
  "m <- seq(0, 2, by=.5)",
  "rc <- data.table(variable='tv', spend_multiplier=m, current_spend=100, support=1000*m, current_support=1000, contribution=50*(1-exp(-m)))",
  "out <- run_optimizer_scenario_planner(response_curves=rc, total_budget=100, multiplier_grid=m, scenario_multipliers=1, max_multiplier=2)",
  "stopifnot(out$inputs_used$engine_mode[1] == 'response_curve_table', nrow(out$saturation_headroom) == 1L, 'support' %in% names(out$response_curves))",
  "cat('optimizer direct ok\\n')",
  sep = "; "
))
add_result("optimizer_scenario_planner.R works from response curves without Stan", optimizer_direct$ok, optimizer_direct$output)

deck_direct <- run_clean_r(paste(
  "library(data.table)",
  "source('mmm_deck_output_builder.R')",
  "ld <- data.table(week=rep(seq.Date(as.Date('2024-01-07'), by='week', length.out=4), each=2), variable=rep(c('tv','baseline'), 4), contribution=c(10,90,12,91,11,92,13,93), y_actual=rep(c(100,103,103,106), each=2), pred=rep(c(100,103,103,106), each=2), residual=0)",
  "raw <- data.table(week=seq.Date(as.Date('2024-01-07'), by='week', length.out=4), tv_spend=c(100,100,120,120))",
  "tbl <- build_mmm_deck_tables(ld, raw_data=raw, time_col='week', media_variables='tv')",
  "stopifnot(is.data.table(tbl$funnel_summary), is.data.table(tbl$kpi_economics_by_channel), nrow(tbl$funnel_summary) > 0)",
  "cat('deck direct ok\\n')",
  sep = "; "
))
add_result("mmm_deck_output_builder.R builds core tables without prior workflow", deck_direct$ok, deck_direct$output)

workflow_direct <- run_clean_r(paste(
  "source('mmm_workflow.R')",
  "stopifnot(is.function(run_mmm_quasi_geo_test), is.function(run_mmm_optimizer_scenario_planner), is.function(run_mmm_reporting), is.function(fit_hier_mmm))",
  "manifest <- mmm_dependency_manifest()",
  "stopifnot('data.table' %in% manifest$core_required, 'cmdstanr' %in% manifest$modeling_required)",
  "cat('workflow aliases ok\\n')",
  sep = "; "
))
add_result("mmm_workflow.R wires core scripts and dependency manifest", workflow_direct$ok, workflow_direct$output)

dir.create(file.path(bundle_dir, "test_outputs"), showWarnings = FALSE)
fwrite(results, file.path(bundle_dir, "test_outputs", "core_script_standalone_contract_results.csv"))
message("\nCore script standalone contract results")
print(results)
message("\nSummary: ", sum(results$status == "PASS"), " passed, ", sum(results$status != "PASS"), " failed.")
invisible(results)
