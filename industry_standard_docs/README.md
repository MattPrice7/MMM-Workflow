# Industry Standard Notes

Use `../full_package_scripts/INDUSTRY_ALIGNMENT.md` as the main source note.

Current design anchors:

- Google Meridian: geo-aware Bayesian MMM, adstock plus Hill saturation, ROI/calibration workflows, population-aware geo schema, and diagnostics/post-modeling workflow.
- Google Matched Markets and TBR: matched-market design, time-based regression fallback, market constraints, and minimum detectable effect thinking.
- Meta GeoLift: synthetic-control style counterfactuals, pre-fit quality, placebo/inference checks, power/MDE, and donor contamination checks.
- Robyn: spend/exposure separation, ridge-style regularization, response-curve guardrails, and calibration from external lift evidence.
- PyMC-Marketing and LightweightMMM: transparent Bayesian MMM implementations with inspectable priors, transformations, posterior diagnostics, and response curves.

Project rule:

Do not turn analyst-created heuristics into defaults unless they map cleanly to one of these sources or are clearly labeled as a local diagnostic/heuristic.

