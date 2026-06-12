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
\alias{fit_hier_mmm}
\alias{prepare_stan_data_hier_mmm}
\alias{variable_contribution_sum_hier_mmm}
\alias{build_roi_mroi_hier_mmm}
\alias{run_hier_mmm_decisioning}
\alias{run_quasi_geo_test}
\alias{run_optimizer_scenario_planner}
\alias{run_mmm_optimizer}
\alias{run_mmm_scenario_planner}
\alias{run_mmm_deck_output_builder}
\alias{build_mmm_deck_tables}
\alias{create_bau_response_curves}
\alias{build_bau_response_curves}
\alias{create_baseline_response_curves}
\title{Core econimap workflow entry points}
\description{
Light package wrappers around the bundled MMM Workflow scripts. The package
loads quickly and sources the active scripts lazily when a core function is
called. Full analyst scripts remain available under \code{inst/scripts}.
}
\value{
Workflow functions return the same objects as their underlying bundled scripts.
Path helpers return character paths, and \code{econimap_dependency_manifest()}
returns a data frame of required and optional package availability.
}
\details{
This is a conservative package shell. It does not yet split the large modeling
scripts into package-native modules; it preserves the current public script API
while making installation, loading, and testing cleaner.
}
