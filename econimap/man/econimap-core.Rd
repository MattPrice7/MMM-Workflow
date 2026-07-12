\name{econimap-core}
\alias{econimap-core}
\alias{econimap_available_scripts}
\alias{econimap_call}
\alias{econimap_dependency_manifest}
\alias{econimap_get_function}
\alias{econimap_script_dir}
\alias{econimap_script_path}
\alias{econimap_stan_path}
\alias{load_econimap_scripts}
\alias{qgt_apply_stan_prior_handoff}
\alias{qgt_build_stan_prior_handoff}
\alias{fit_hier_mmm}
\alias{prepare_stan_data_hier_mmm}
\alias{variable_contribution_sum_hier_mmm}
\alias{build_roi_mroi_hier_mmm}
\alias{build_response_curves_hier_mmm}
\alias{build_response_curves_draws_hier_mmm}
\alias{build_frequency_response_curves_hier_mmm}
\alias{build_posterior_contribution_intervals_hier_mmm}
\alias{run_hier_mmm_prior_predictive}
\alias{run_hier_mmm_decisioning}
\alias{run_quasi_geo_test}
\alias{run_optimizer_scenario_planner}
\alias{run_mmm_optimizer}
\alias{run_mmm_scenario_planner}
\alias{run_mmm_deck_output_builder}
\alias{build_mmm_deck_tables}
\alias{build_mmm_excel_dashboard}
\alias{create_bau_response_curves}
\alias{build_bau_response_curves}
\alias{create_baseline_response_curves}
\alias{fit_parsimonious_total_media_root}
\alias{build_sequential_rollup_plan}
\alias{build_sequential_rollup_layer}
\alias{build_sequential_effectiveness_priors}
\alias{build_sequential_effectiveness_priors_from_parent_fit}
\alias{run_sequential_hierarchical_bayes}
\alias{continue_sequential_hierarchical_bayes}
\title{Core econimap workflow entry points}
\description{
Analyst-facing entry points for package-native MMM, quasi-geo, optimization,
scenario-planning, and reporting modules. Synchronized standalone scripts remain
available under \code{inst/scripts} for script-first workflows.
}
\value{
Workflow functions return the same objects as their underlying bundled scripts.
Path helpers return character paths, and \code{econimap_dependency_manifest()}
returns a data frame of required and optional package availability.
}
\details{
Stable internals are split into package-native modules under \code{R/}. Public
script APIs remain backward-compatible and the standalone files are synchronized
from the same tested implementation. Linear non-media treatments support explicit
counterfactual baselines and contribution, elasticity, standardized-effect, or
coefficient priors. Quasi-geo outputs include event estimates, placebo and donor
diagnostics, uncertainty-weighted curve evidence, and pooled treated-market reads.
Sequential hierarchical Bayes is opt-in: it uses a blocked-bootstrap
total-paid-media root to create conservative equal-effectiveness child priors
for the existing joint Stan child model. Analysts may directly hand off from
the root to any declared spend-based media-rollup depth; intermediate media
levels are optional rather than mandatory. A reviewed fitted numeric stage can
then pass its response-curve posterior draws to a deeper selected layer.
}
