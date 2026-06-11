// hier_mmm.stan
// FINAL BUNDLE VERSION: v18_fourier_default_ucm_final_2026_05_19
// =============================================================================
// hier_mmm.stan
//
// Deployment Stan model for hierarchical Bayesian MMM.
//
// NOTES:
// - log_lik is generated for training rows only, matching the likelihood and
//   avoiding holdout rows in LOO/WAIC calculations.
// - Variable-level curves only; never market/group-level curves.
// - Group-level hierarchical coefficients with optional group overrides.
// - Intercept/UCM structure is configured in the R wrapper or metadata role = "ucm":
//
//     intercept_type = "none" | "flat" | "fourier" | "ucm"
//     ucm_spec = list(
//       level = TRUE,
//       season = TRUE,
//       cycle = FALSE,
//       season_period = 52L,
//       season_harmonics = 2L,
//       cycle_period = 104L,
//       cycle_harmonics = 1L
//     )
//
// The R wrapper:
// - validates metadata
// - excludes variables not listed in metadata by default
// - sorts data by group_col then time_col
// - checks duplicate group_col + time_col rows
// - normalizes curve transforms using training rows only when holdout is used
// - builds seasonal / cycle Fourier matrices
// - R wrapper uses explicit index mapping before this Stan data is built
// =============================================================================

functions {
  real context_multiplier_hier_mmm(int n,
                                   int j,
                                   int K_context,
                                   matrix X_context,
                                   array[] int context_variable_idx,
                                   vector context_coef,
                                   real context_log_multiplier_bound) {
    real log_mult = 0;
    if (K_context > 0) {
      for (h in 1:K_context) {
        if (context_variable_idx[h] == j)
          log_mult += context_coef[h] * X_context[n, h];
      }
    }
    if (context_log_multiplier_bound > 0)
      log_mult = fmin(fmax(log_mult, -context_log_multiplier_bound), context_log_multiplier_bound);
    return exp(log_mult);
  }
}

data {
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1> G;
  int<lower=0> K_extra;

  vector[N] y;
  matrix[N, J] X;
  matrix[G, J] X_center_mean;
  matrix[N, K_extra] Z_extra;

  int<lower=1> N_train;
  array[N_train] int<lower=1, upper=N> train_idx;
  array[N] int<lower=0, upper=1> is_train;

  array[N] int<lower=1, upper=G> group_id;
  array[G] int<lower=1, upper=N> start_idx;
  array[G] int<lower=1, upper=N> end_idx;
  int<lower=1> K_coef_hierarchy_keys;
  array[G] int<lower=1, upper=K_coef_hierarchy_keys> group_coef_hierarchy_key_id;
  int<lower=0> N_state_innov;

  array[J] int<lower=0, upper=1> has_curve;
  int<lower=0> J_curve;
  int<lower=0> J_linear;
  array[J_curve] int<lower=1, upper=J> curve_idx;
  array[J_linear] int<lower=1, upper=J> linear_idx;
  // 1 = Weibull CDF-style saturation, 2 = Hill saturation.
  array[J_curve] int<lower=1, upper=2> curve_type;
  // For fixed-curve mode, R precomputes the full adstock + saturation +
  // train-only scaling path. This keeps fixed curves out of the autodiff graph.
  matrix[N, J_curve] X_curve_fixed;
  matrix[G, J_curve] X_curve_fixed_center;
  int<lower=0> K_context;
  matrix[N, K_context] X_context;
  array[K_context] int<lower=1, upper=J> context_variable_idx;
  vector[K_context] context_coef_mu;
  vector<lower=0>[K_context] context_coef_sd;
  array[K_context] int<lower=-1, upper=1> context_sign;
  int<lower=0> K_context_pos;
  int<lower=0> K_context_neg;
  int<lower=0> K_context_free;
  array[K_context_pos] int<lower=1, upper=K_context> context_pos_pos;
  array[K_context_neg] int<lower=1, upper=K_context> context_neg_pos;
  array[K_context_free] int<lower=1, upper=K_context> context_free_pos;
  real<lower=0> context_log_multiplier_bound;

  int<lower=0, upper=2> intercept_mode; // 0 none, 1 flat, 2 structured intercept
  int<lower=0, upper=1> intercept_use_level;
  int<lower=0, upper=1> intercept_use_season;
  int<lower=0, upper=1> intercept_use_cycle;
  int<lower=0> K_season;
  matrix[N, K_season] X_season;
  int<lower=0> K_cycle;
  matrix[N, K_cycle] X_cycle;

  int<lower=0> J_pos;
  int<lower=0> J_neg;
  int<lower=0> J_lower;
  int<lower=0> J_upper;
  int<lower=0> J_bounded;
  int<lower=0> J_free;

  array[J_pos] int<lower=1, upper=J> pos_idx;
  array[J_neg] int<lower=1, upper=J> neg_idx;
  array[J_lower] int<lower=1, upper=J> lower_idx;
  array[J_upper] int<lower=1, upper=J> upper_idx;
  array[J_bounded] int<lower=1, upper=J> bounded_idx;
  array[J_free] int<lower=1, upper=J> free_idx;

  vector[J_bounded] bounded_lower;
  vector[J_bounded] bounded_upper;
  vector[J_lower] lower_only_lower;
  vector[J_upper] upper_only_upper;

  vector[J_curve] rrate_raw_mu;
  vector<lower=0>[J_curve] rrate_raw_sd;
  vector[J_curve] cvalue_raw_mu;
  vector<lower=0>[J_curve] cvalue_raw_sd;
  vector[J_curve] dvalue_raw_mu;
  vector<lower=0>[J_curve] dvalue_raw_sd;
  array[J_curve] int<lower=0, upper=1> sample_curve_parameter;
  int<lower=0> J_curve_sampled;
  array[J_curve_sampled] int<lower=1, upper=J_curve> curve_sampled_pos;
  array[J_curve] int<lower=0, upper=1> use_observed_cvalue_prior;
  vector[J_curve] observed_cvalue_raw_mu;
  vector<lower=0>[J_curve] observed_cvalue_raw_sd;

  vector[J_curve] rrate_lower;
  vector[J_curve] rrate_upper;
  vector[J_curve] cvalue_lower;
  vector[J_curve] cvalue_upper;
  vector[J_curve] dvalue_lower;
  vector[J_curve] dvalue_upper;
  int<lower=0, upper=1> estimate_dvalue;
  int<lower=0, upper=1> normalize_curve_x;
  // 1 = use training rows with raw/current-period X > 0 for carry and
  // transformed-mean scaling. 0 = use all training rows.
  int<lower=0, upper=1> curve_normalization_active;
  int<lower=0, upper=1> center_predictors_for_sampling;
  // 1 = non-centered group intercepts, 2 = centered group intercepts, 3 = shared intercept.
  int<lower=1, upper=3> alpha_parameterization;
  int<lower=0> G_alpha;
  // 1 = non-centered UCM/state effects, 2 = centered UCM/state effects.
  int<lower=1, upper=2> ucm_parameterization;

  real alpha_mu_prior_mean;
  real<lower=0> alpha_mu_prior_sd;
  real<lower=0> alpha_sd_prior_sd;
  real level0_mu_prior_mean;
  real<lower=0> level0_mu_prior_sd;
  real<lower=0> level0_sd_prior_sd;
  real<lower=0> sigma_level_prior_sd;
  real<lower=0> season_sd_prior_sd;
  real<lower=0> cycle_sd_prior_sd;
  real<lower=0> gamma_prior_sd;
  real<lower=0> sigma_y_prior_sd;
  real<lower=0> sigma_y_floor;
  real<lower=0> sigma_y_upper;
  int<lower=1, upper=2> likelihood_family; // 1 = normal, 2 = Student-t
  real<lower=2> student_t_nu;

  vector[J_pos] coef_mu_pos_log;
  vector<lower=0>[J_pos] coef_sd_pos_log;
  vector<lower=0>[J_pos] tau_scale_pos;
  array[J_pos] int<lower=0, upper=1> sample_pos_hierarchy;
  array[J_pos] int<lower=0, upper=2> coef_hierarchy_mode_pos; // 0 none, 1 global, 2 keyed family
  array[J_pos] int<lower=0, upper=1> coef_centered_pos;
  int<lower=0> J_pos_hier;
  array[J_pos_hier] int<lower=1, upper=J_pos> pos_hier_pos;

  vector[J_neg] coef_mu_neg_log;
  vector<lower=0>[J_neg] coef_sd_neg_log;
  vector<lower=0>[J_neg] tau_scale_neg;
  array[J_neg] int<lower=0, upper=1> sample_neg_hierarchy;
  array[J_neg] int<lower=0, upper=2> coef_hierarchy_mode_neg;
  array[J_neg] int<lower=0, upper=1> coef_centered_neg;
  int<lower=0> J_neg_hier;
  array[J_neg_hier] int<lower=1, upper=J_neg> neg_hier_pos;

  vector[J_lower] coef_mu_lower_log;
  vector<lower=0>[J_lower] coef_sd_lower_log;
  vector<lower=0>[J_lower] tau_scale_lower;
  array[J_lower] int<lower=0, upper=1> sample_lower_hierarchy;
  array[J_lower] int<lower=0, upper=2> coef_hierarchy_mode_lower;
  array[J_lower] int<lower=0, upper=1> coef_centered_lower;
  int<lower=0> J_lower_hier;
  array[J_lower_hier] int<lower=1, upper=J_lower> lower_hier_pos;

  vector[J_upper] coef_mu_upper_log;
  vector<lower=0>[J_upper] coef_sd_upper_log;
  vector<lower=0>[J_upper] tau_scale_upper;
  array[J_upper] int<lower=0, upper=1> sample_upper_hierarchy;
  array[J_upper] int<lower=0, upper=2> coef_hierarchy_mode_upper;
  array[J_upper] int<lower=0, upper=1> coef_centered_upper;
  int<lower=0> J_upper_hier;
  array[J_upper_hier] int<lower=1, upper=J_upper> upper_hier_pos;

  vector[J_bounded] coef_raw_mu_bounded;
  vector<lower=0>[J_bounded] coef_raw_sd_bounded;
  vector<lower=0>[J_bounded] tau_scale_bounded;
  array[J_bounded] int<lower=0, upper=1> sample_bounded_hierarchy;
  array[J_bounded] int<lower=0, upper=2> coef_hierarchy_mode_bounded;
  array[J_bounded] int<lower=0, upper=1> coef_centered_bounded;
  int<lower=0> J_bounded_hier;
  array[J_bounded_hier] int<lower=1, upper=J_bounded> bounded_hier_pos;

  vector[J_free] coef_mu_free;
  vector<lower=0>[J_free] coef_sd_free;
  vector<lower=0>[J_free] tau_scale_free;
  array[J_free] int<lower=0, upper=1> sample_free_hierarchy;
  array[J_free] int<lower=0, upper=2> coef_hierarchy_mode_free;
  array[J_free] int<lower=0, upper=1> coef_centered_free;
  int<lower=0> J_free_hier;
  array[J_free_hier] int<lower=1, upper=J_free> free_hier_pos;

  int<lower=0, upper=1> use_coef_overrides;
  matrix[G, J] coef_override_mu;
  matrix<lower=0>[G, J] coef_override_sd;
}
parameters {
  vector[J_curve_sampled] rrate_raw;
  vector[J_curve_sampled] cvalue_raw;
  vector[J_curve_sampled] dvalue_raw;

  vector[J_pos] mu_log_pos;
  vector<lower=0>[J_pos_hier] tau_pos_key;
  matrix[K_coef_hierarchy_keys, J_pos_hier] z_pos_key;
  vector<lower=0>[J_pos_hier] tau_pos;
  matrix[G, J_pos_hier] z_pos;

  vector[J_neg] mu_log_neg;
  vector<lower=0>[J_neg_hier] tau_neg_key;
  matrix[K_coef_hierarchy_keys, J_neg_hier] z_neg_key;
  vector<lower=0>[J_neg_hier] tau_neg;
  matrix[G, J_neg_hier] z_neg;

  vector[J_lower] mu_log_lower;
  vector<lower=0>[J_lower_hier] tau_lower_key;
  matrix[K_coef_hierarchy_keys, J_lower_hier] z_lower_key;
  vector<lower=0>[J_lower_hier] tau_lower;
  matrix[G, J_lower_hier] z_lower;

  vector[J_upper] mu_log_upper;
  vector<lower=0>[J_upper_hier] tau_upper_key;
  matrix[K_coef_hierarchy_keys, J_upper_hier] z_upper_key;
  vector<lower=0>[J_upper_hier] tau_upper;
  matrix[G, J_upper_hier] z_upper;

  vector[J_bounded] mu_raw_bounded;
  vector<lower=0>[J_bounded_hier] tau_bounded_key;
  matrix[K_coef_hierarchy_keys, J_bounded_hier] z_bounded_key;
  vector<lower=0>[J_bounded_hier] tau_bounded;
  matrix[G, J_bounded_hier] z_bounded;

  vector[J_free] mu_free;
  vector<lower=0>[J_free_hier] tau_free_key;
  matrix[K_coef_hierarchy_keys, J_free_hier] z_free_key;
  vector<lower=0>[J_free_hier] tau_free;
  matrix[G, J_free_hier] z_free;

  real alpha_mu;
  real<lower=0> alpha_sd;
  // Reused across centered and non-centered intercept parameterizations.
  // Non-centered: alpha_flat = alpha_mu + alpha_sd * alpha_z.
  // Centered:     alpha_flat = alpha_z, with alpha_z ~ normal(alpha_mu, alpha_sd).
  vector[G_alpha] alpha_z;

  real level0_mu;
  real<lower=0> level0_sd;
  vector[G] level0_raw;
  real<lower=0> sigma_level;
  vector[N_state_innov] level_innov_raw;

  vector<lower=0>[K_season] season_sd;
  matrix[G, K_season] season_raw;

  vector<lower=0>[K_cycle] cycle_sd;
  matrix[G, K_cycle] cycle_raw;

  vector[K_extra] gamma;
  vector<lower=0>[K_context_pos] context_coef_pos;
  vector<upper=0>[K_context_neg] context_coef_neg;
  vector[K_context_free] context_coef_free;
  real<lower=sigma_y_floor, upper=sigma_y_upper> sigma_y;
}
transformed parameters {
  vector[J_curve] rrate;
  vector[J_curve] cvalue;
  vector[J_curve] dvalue;
  matrix[G, J] beta;
  vector[K_context] context_coef;
  vector[G] alpha_flat;
  vector[N] level_component;
  vector[N] season_component;
  vector[N] cycle_component;
  vector[N] level_state;
  vector[N] mu;

  if (J_curve > 0) {
    for (k in 1:J_curve) {
      rrate[k]  = rrate_lower[k]  + (rrate_upper[k]  - rrate_lower[k])  * inv_logit(rrate_raw_mu[k]);
      cvalue[k] = cvalue_lower[k] + (cvalue_upper[k] - cvalue_lower[k]) * inv_logit(cvalue_raw_mu[k]);
      dvalue[k] = dvalue_lower[k] + (dvalue_upper[k] - dvalue_lower[k]) * inv_logit(dvalue_raw_mu[k]);
    }
  }
  if (J_curve_sampled > 0) {
    for (h in 1:J_curve_sampled) {
      int k = curve_sampled_pos[h];
      rrate[k]  = rrate_lower[k]  + (rrate_upper[k]  - rrate_lower[k])  * inv_logit(rrate_raw[h]);
      cvalue[k] = cvalue_lower[k] + (cvalue_upper[k] - cvalue_lower[k]) * inv_logit(cvalue_raw[h]);
      dvalue[k] = dvalue_lower[k] + (dvalue_upper[k] - dvalue_lower[k]) * inv_logit(dvalue_raw[h]);
    }
  }

  beta = rep_matrix(0, G, J);

  if (J_pos > 0)
    for (k in 1:J_pos)
      for (g in 1:G) {
        beta[g, pos_idx[k]] = log1p_exp(mu_log_pos[k]);
      }
  if (J_pos_hier > 0)
    for (h in 1:J_pos_hier) {
      int k = pos_hier_pos[h];
      for (g in 1:G) {
        real raw;
        if (coef_hierarchy_mode_pos[k] == 2) {
          int key_id = group_coef_hierarchy_key_id[g];
          real key_raw = mu_log_pos[k] + tau_pos_key[h] * z_pos_key[key_id, h];
          raw = key_raw + tau_pos[h] * z_pos[g, h];
        } else {
          raw = coef_centered_pos[k] == 1 ? z_pos[g, h] : mu_log_pos[k] + tau_pos[h] * z_pos[g, h];
        }
        beta[g, pos_idx[k]] = log1p_exp(raw);
      }
    }

  if (J_neg > 0)
    for (k in 1:J_neg)
      for (g in 1:G) {
        beta[g, neg_idx[k]] = -log1p_exp(mu_log_neg[k]);
      }
  if (J_neg_hier > 0)
    for (h in 1:J_neg_hier) {
      int k = neg_hier_pos[h];
      for (g in 1:G) {
        real raw;
        if (coef_hierarchy_mode_neg[k] == 2) {
          int key_id = group_coef_hierarchy_key_id[g];
          real key_raw = mu_log_neg[k] + tau_neg_key[h] * z_neg_key[key_id, h];
          raw = key_raw + tau_neg[h] * z_neg[g, h];
        } else {
          raw = coef_centered_neg[k] == 1 ? z_neg[g, h] : mu_log_neg[k] + tau_neg[h] * z_neg[g, h];
        }
        beta[g, neg_idx[k]] = -log1p_exp(raw);
      }
    }

  if (J_lower > 0)
    for (k in 1:J_lower)
      for (g in 1:G) {
        beta[g, lower_idx[k]] = lower_only_lower[k] + log1p_exp(mu_log_lower[k]);
      }
  if (J_lower_hier > 0)
    for (h in 1:J_lower_hier) {
      int k = lower_hier_pos[h];
      for (g in 1:G) {
        real raw;
        if (coef_hierarchy_mode_lower[k] == 2) {
          int key_id = group_coef_hierarchy_key_id[g];
          real key_raw = mu_log_lower[k] + tau_lower_key[h] * z_lower_key[key_id, h];
          raw = key_raw + tau_lower[h] * z_lower[g, h];
        } else {
          raw = coef_centered_lower[k] == 1 ? z_lower[g, h] : mu_log_lower[k] + tau_lower[h] * z_lower[g, h];
        }
        beta[g, lower_idx[k]] = lower_only_lower[k] + log1p_exp(raw);
      }
    }

  if (J_upper > 0)
    for (k in 1:J_upper)
      for (g in 1:G) {
        beta[g, upper_idx[k]] = upper_only_upper[k] - log1p_exp(mu_log_upper[k]);
      }
  if (J_upper_hier > 0)
    for (h in 1:J_upper_hier) {
      int k = upper_hier_pos[h];
      for (g in 1:G) {
        real raw;
        if (coef_hierarchy_mode_upper[k] == 2) {
          int key_id = group_coef_hierarchy_key_id[g];
          real key_raw = mu_log_upper[k] + tau_upper_key[h] * z_upper_key[key_id, h];
          raw = key_raw + tau_upper[h] * z_upper[g, h];
        } else {
          raw = coef_centered_upper[k] == 1 ? z_upper[g, h] : mu_log_upper[k] + tau_upper[h] * z_upper[g, h];
        }
        beta[g, upper_idx[k]] = upper_only_upper[k] - log1p_exp(raw);
      }
    }

  if (J_bounded > 0)
    for (k in 1:J_bounded)
      for (g in 1:G) {
        beta[g, bounded_idx[k]] = bounded_lower[k] + (bounded_upper[k] - bounded_lower[k]) * inv_logit(mu_raw_bounded[k]);
      }
  if (J_bounded_hier > 0)
    for (h in 1:J_bounded_hier) {
      int k = bounded_hier_pos[h];
      for (g in 1:G) {
        real raw;
        if (coef_hierarchy_mode_bounded[k] == 2) {
          int key_id = group_coef_hierarchy_key_id[g];
          real key_raw = mu_raw_bounded[k] + tau_bounded_key[h] * z_bounded_key[key_id, h];
          raw = key_raw + tau_bounded[h] * z_bounded[g, h];
        } else {
          raw = coef_centered_bounded[k] == 1 ? z_bounded[g, h] : mu_raw_bounded[k] + tau_bounded[h] * z_bounded[g, h];
        }
        beta[g, bounded_idx[k]] = bounded_lower[k] + (bounded_upper[k] - bounded_lower[k]) * inv_logit(raw);
      }
    }

  if (J_free > 0)
    for (k in 1:J_free)
      for (g in 1:G) {
        beta[g, free_idx[k]] = mu_free[k];
      }
  if (J_free_hier > 0)
    for (h in 1:J_free_hier) {
      int k = free_hier_pos[h];
      for (g in 1:G) {
        real raw;
        if (coef_hierarchy_mode_free[k] == 2) {
          int key_id = group_coef_hierarchy_key_id[g];
          real key_raw = mu_free[k] + tau_free_key[h] * z_free_key[key_id, h];
          raw = key_raw + tau_free[h] * z_free[g, h];
        } else {
          raw = coef_centered_free[k] == 1 ? z_free[g, h] : mu_free[k] + tau_free[h] * z_free[g, h];
        }
        beta[g, free_idx[k]] = raw;
      }
    }

  context_coef = rep_vector(0, K_context);
  if (K_context > 0) {
    if (K_context_pos > 0)
      for (h in 1:K_context_pos)
        context_coef[context_pos_pos[h]] = context_coef_pos[h];
    if (K_context_neg > 0)
      for (h in 1:K_context_neg)
        context_coef[context_neg_pos[h]] = context_coef_neg[h];
    if (K_context_free > 0)
      for (h in 1:K_context_free)
        context_coef[context_free_pos[h]] = context_coef_free[h];
  }

  if (alpha_parameterization == 3) {
    alpha_flat = rep_vector(alpha_mu, G);
  } else if (alpha_parameterization == 2) {
    alpha_flat = alpha_z;
  } else {
    alpha_flat = alpha_mu + alpha_sd * alpha_z;
  }
  level_component = rep_vector(0, N);
  season_component = rep_vector(0, N);
  cycle_component = rep_vector(0, N);
  level_state = rep_vector(0, N);

  if (intercept_mode == 2 && intercept_use_level == 1) {
    int innov_pos = 1;
    for (g in 1:G) {
      int s = start_idx[g];
      int e = end_idx[g];
      if (ucm_parameterization == 2)
        level_component[s] = level0_raw[g];
      else
        level_component[s] = level0_mu + level0_sd * level0_raw[g];
      if (e >= s + 1) {
        for (n in (s + 1):e) {
          if (ucm_parameterization == 2)
            level_component[n] = level_component[n - 1] + level_innov_raw[innov_pos];
          else
            level_component[n] = level_component[n - 1] + sigma_level * level_innov_raw[innov_pos];
          innov_pos += 1;
        }
      }
    }
    level_state = level_component;
  }

  if (intercept_mode == 2 && intercept_use_season == 1 && K_season > 0) {
    matrix[G, K_season] season_beta;
    season_beta = rep_matrix(0, G, K_season);
    for (k in 1:K_season)
      for (g in 1:G)
        season_beta[g, k] = ucm_parameterization == 2 ? season_raw[g, k] : season_sd[k] * season_raw[g, k];

    for (n in 1:N)
      season_component[n] = dot_product(X_season[n], season_beta[group_id[n]]);
  }

  if (intercept_mode == 2 && intercept_use_cycle == 1 && K_cycle > 0) {
    matrix[G, K_cycle] cycle_beta;
    cycle_beta = rep_matrix(0, G, K_cycle);
    for (k in 1:K_cycle)
      for (g in 1:G)
        cycle_beta[g, k] = ucm_parameterization == 2 ? cycle_raw[g, k] : cycle_sd[k] * cycle_raw[g, k];

    for (n in 1:N)
      cycle_component[n] = dot_product(X_cycle[n], cycle_beta[group_id[n]]);
  }

  mu = rep_vector(0, N);
  if (K_extra > 0) mu += Z_extra * gamma;

  if (intercept_mode == 1) {
    for (n in 1:N) mu[n] += alpha_flat[group_id[n]];
  } else if (intercept_mode == 2) {
    if (intercept_use_level == 1) {
      mu += level_state;
    } else {
      // Fourier/cycle baselines still need a group-level constant baseline.
      for (n in 1:N) mu[n] += alpha_flat[group_id[n]];
    }
    if (intercept_use_season == 1) mu += season_component;
    if (intercept_use_cycle == 1) mu += cycle_component;
  }

  if (J_linear > 0) {
    for (n in 1:N) {
      real lin = 0;
      for (k in 1:J_linear)
        if (center_predictors_for_sampling == 1) {
          real context_mult = context_multiplier_hier_mmm(n, linear_idx[k], K_context, X_context, context_variable_idx, context_coef, context_log_multiplier_bound);
          lin += beta[group_id[n], linear_idx[k]]
                 * context_mult
                 * (X[n, linear_idx[k]] - X_center_mean[group_id[n], linear_idx[k]]);
        } else {
          real context_mult = context_multiplier_hier_mmm(n, linear_idx[k], K_context, X_context, context_variable_idx, context_coef, context_log_multiplier_bound);
          lin += beta[group_id[n], linear_idx[k]] * context_mult * X[n, linear_idx[k]];
        }
      mu[n] += lin;
    }
  }

  if (J_curve > 0) {
    for (g in 1:G) {
      for (k in 1:J_curve) {
        int j = curve_idx[k];
        if (sample_curve_parameter[k] == 0) {
          real center_value = center_predictors_for_sampling == 1 ? X_curve_fixed_center[g, k] : 0;
          for (n in start_idx[g]:end_idx[g]) {
            real context_mult = context_multiplier_hier_mmm(n, j, K_context, X_context, context_variable_idx, context_coef, context_log_multiplier_bound);
            mu[n] += beta[g, j] * context_mult * (X_curve_fixed[n, k] - center_value);
          }
        } else {
        real rr = rrate[k];
        real cv = cvalue[k];
        real dv = dvalue[k];
        real carry_scale = 1;
        real trans_mean = 1;

        if (normalize_curve_x == 1) {
          real carry_for_scale = 0;
          real carry_sum_all = 0;
          int carry_count_all = 0;
          real carry_sum_active = 0;
          int carry_count_active = 0;
          for (n in start_idx[g]:end_idx[g]) {
            real x = fmax(X[n, j], 0);
            carry_for_scale = x + rr * carry_for_scale;
            if (is_train[n] == 1) {
              carry_sum_all += carry_for_scale;
              carry_count_all += 1;
              if (curve_normalization_active == 1 && x > 0) {
                carry_sum_active += carry_for_scale;
                carry_count_active += 1;
              }
            }
          }
          if (curve_normalization_active == 1 && carry_count_active > 0)
            carry_scale = fmax(carry_sum_active / carry_count_active, 1e-8);
          else
            carry_scale = fmax(carry_sum_all / carry_count_all, 1e-8);
        }

        {
          real carry_mean = 0;
          real trans_sum_all = 0;
          int n_count_all = 0;
          real trans_sum_active = 0;
          int n_count_active = 0;
          for (n in start_idx[g]:end_idx[g]) {
            real x = fmax(X[n, j], 0);
            real carry_for_curve;
            real trans;
            carry_mean = x + rr * carry_mean;
            if (normalize_curve_x == 1)
              carry_for_curve = carry_mean / carry_scale;
            else
              carry_for_curve = carry_mean;
            {
              real z = pow(fmax(carry_for_curve * cv, 1e-12), dv);
              trans = curve_type[k] == 2 ? z / (1 + z) : 1 - exp(-z);
            }
            if (is_train[n] == 1) {
              trans_sum_all += trans;
              n_count_all += 1;
              if (curve_normalization_active == 1 && x > 0) {
                trans_sum_active += trans;
                n_count_active += 1;
              }
            }
          }
          if (curve_normalization_active == 1 && n_count_active > 0)
            trans_mean = fmax(trans_sum_active / n_count_active, 1e-8);
          else
            trans_mean = fmax(trans_sum_all / n_count_all, 1e-8);
        }

        {
          real carry = 0;
          for (n in start_idx[g]:end_idx[g]) {
            real x = fmax(X[n, j], 0);
            real carry_for_curve;
            real trans;
            real trans_model;
            real center_value = 0;
            carry = x + rr * carry;
            if (normalize_curve_x == 1)
              carry_for_curve = carry / carry_scale;
            else
              carry_for_curve = carry;
            {
              real z = pow(fmax(carry_for_curve * cv, 1e-12), dv);
              trans = curve_type[k] == 2 ? z / (1 + z) : 1 - exp(-z);
            }
            if (normalize_curve_x == 1) {
              trans_model = trans / trans_mean;
              if (center_predictors_for_sampling == 1) center_value = 1;
            } else {
              trans_model = trans;
              if (center_predictors_for_sampling == 1) center_value = trans_mean;
            }
            {
              real context_mult = context_multiplier_hier_mmm(n, j, K_context, X_context, context_variable_idx, context_coef, context_log_multiplier_bound);
              mu[n] += beta[g, j] * context_mult * (trans_model - center_value);
            }
          }
        }
        }
      }
    }
  }
}
model {
  if (J_curve_sampled > 0) {
    for (h in 1:J_curve_sampled) {
      int k = curve_sampled_pos[h];
      rrate_raw[h] ~ normal(rrate_raw_mu[k], rrate_raw_sd[k]);
      cvalue_raw[h] ~ normal(cvalue_raw_mu[k], cvalue_raw_sd[k]);
      if (use_observed_cvalue_prior[k] == 1)
        cvalue_raw[h] ~ normal(observed_cvalue_raw_mu[k], observed_cvalue_raw_sd[k]);
      if (estimate_dvalue == 1)
        dvalue_raw[h] ~ normal(dvalue_raw_mu[k], dvalue_raw_sd[k]);
      else
        dvalue_raw[h] ~ normal(dvalue_raw_mu[k], 0.05);
    }
  }

  if (K_context_pos > 0) {
    for (h in 1:K_context_pos) {
      int k = context_pos_pos[h];
      context_coef_pos[h] ~ normal(context_coef_mu[k], context_coef_sd[k]);
    }
  }
  if (K_context_neg > 0) {
    for (h in 1:K_context_neg) {
      int k = context_neg_pos[h];
      context_coef_neg[h] ~ normal(context_coef_mu[k], context_coef_sd[k]);
    }
  }
  if (K_context_free > 0) {
    for (h in 1:K_context_free) {
      int k = context_free_pos[h];
      context_coef_free[h] ~ normal(context_coef_mu[k], context_coef_sd[k]);
    }
  }

  if (J_pos > 0) {
    mu_log_pos ~ normal(coef_mu_pos_log, coef_sd_pos_log);
    if (J_pos_hier > 0) {
      for (h in 1:J_pos_hier) {
        int k = pos_hier_pos[h];
        real tau_scale = coef_hierarchy_mode_pos[k] == 2 ? fmax(tau_scale_pos[k] * 0.7071068, 1e-4) : fmax(tau_scale_pos[k], 1e-4);
        tau_pos_key[h] ~ normal(0, tau_scale);
        z_pos_key[, h] ~ std_normal();
        tau_pos[h] ~ normal(0, tau_scale);
        if (coef_hierarchy_mode_pos[k] == 2)
          z_pos[, h] ~ std_normal();
        else if (coef_centered_pos[k] == 1)
          z_pos[, h] ~ normal(mu_log_pos[k], fmax(tau_pos[h], 1e-6));
        else
          z_pos[, h] ~ std_normal();
      }
    }
  }
  if (J_neg > 0) {
    mu_log_neg ~ normal(coef_mu_neg_log, coef_sd_neg_log);
    if (J_neg_hier > 0) {
      for (h in 1:J_neg_hier) {
        int k = neg_hier_pos[h];
        real tau_scale = coef_hierarchy_mode_neg[k] == 2 ? fmax(tau_scale_neg[k] * 0.7071068, 1e-4) : fmax(tau_scale_neg[k], 1e-4);
        tau_neg_key[h] ~ normal(0, tau_scale);
        z_neg_key[, h] ~ std_normal();
        tau_neg[h] ~ normal(0, tau_scale);
        if (coef_hierarchy_mode_neg[k] == 2)
          z_neg[, h] ~ std_normal();
        else if (coef_centered_neg[k] == 1)
          z_neg[, h] ~ normal(mu_log_neg[k], fmax(tau_neg[h], 1e-6));
        else
          z_neg[, h] ~ std_normal();
      }
    }
  }
  if (J_lower > 0) {
    mu_log_lower ~ normal(coef_mu_lower_log, coef_sd_lower_log);
    if (J_lower_hier > 0) {
      for (h in 1:J_lower_hier) {
        int k = lower_hier_pos[h];
        real tau_scale = coef_hierarchy_mode_lower[k] == 2 ? fmax(tau_scale_lower[k] * 0.7071068, 1e-4) : fmax(tau_scale_lower[k], 1e-4);
        tau_lower_key[h] ~ normal(0, tau_scale);
        z_lower_key[, h] ~ std_normal();
        tau_lower[h] ~ normal(0, tau_scale);
        if (coef_hierarchy_mode_lower[k] == 2)
          z_lower[, h] ~ std_normal();
        else if (coef_centered_lower[k] == 1)
          z_lower[, h] ~ normal(mu_log_lower[k], fmax(tau_lower[h], 1e-6));
        else
          z_lower[, h] ~ std_normal();
      }
    }
  }
  if (J_upper > 0) {
    mu_log_upper ~ normal(coef_mu_upper_log, coef_sd_upper_log);
    if (J_upper_hier > 0) {
      for (h in 1:J_upper_hier) {
        int k = upper_hier_pos[h];
        real tau_scale = coef_hierarchy_mode_upper[k] == 2 ? fmax(tau_scale_upper[k] * 0.7071068, 1e-4) : fmax(tau_scale_upper[k], 1e-4);
        tau_upper_key[h] ~ normal(0, tau_scale);
        z_upper_key[, h] ~ std_normal();
        tau_upper[h] ~ normal(0, tau_scale);
        if (coef_hierarchy_mode_upper[k] == 2)
          z_upper[, h] ~ std_normal();
        else if (coef_centered_upper[k] == 1)
          z_upper[, h] ~ normal(mu_log_upper[k], fmax(tau_upper[h], 1e-6));
        else
          z_upper[, h] ~ std_normal();
      }
    }
  }
  if (J_bounded > 0) {
    mu_raw_bounded ~ normal(coef_raw_mu_bounded, coef_raw_sd_bounded);
    if (J_bounded_hier > 0) {
      for (h in 1:J_bounded_hier) {
        int k = bounded_hier_pos[h];
        real tau_scale = coef_hierarchy_mode_bounded[k] == 2 ? fmax(tau_scale_bounded[k] * 0.7071068, 1e-4) : fmax(tau_scale_bounded[k], 1e-4);
        tau_bounded_key[h] ~ normal(0, tau_scale);
        z_bounded_key[, h] ~ std_normal();
        tau_bounded[h] ~ normal(0, tau_scale);
        if (coef_hierarchy_mode_bounded[k] == 2)
          z_bounded[, h] ~ std_normal();
        else if (coef_centered_bounded[k] == 1)
          z_bounded[, h] ~ normal(mu_raw_bounded[k], fmax(tau_bounded[h], 1e-6));
        else
          z_bounded[, h] ~ std_normal();
      }
    }
  }
  if (J_free > 0) {
    mu_free ~ normal(coef_mu_free, coef_sd_free);
    if (J_free_hier > 0) {
      for (h in 1:J_free_hier) {
        int k = free_hier_pos[h];
        real tau_scale = coef_hierarchy_mode_free[k] == 2 ? fmax(tau_scale_free[k] * 0.7071068, 1e-4) : fmax(tau_scale_free[k], 1e-4);
        tau_free_key[h] ~ normal(0, tau_scale);
        z_free_key[, h] ~ std_normal();
        tau_free[h] ~ normal(0, tau_scale);
        if (coef_hierarchy_mode_free[k] == 2)
          z_free[, h] ~ std_normal();
        else if (coef_centered_free[k] == 1)
          z_free[, h] ~ normal(mu_free[k], fmax(tau_free[h], 1e-6));
        else
          z_free[, h] ~ std_normal();
      }
    }
  }

  if (use_coef_overrides == 1) {
    for (g in 1:G) {
      for (j in 1:J) {
        if (coef_override_sd[g, j] > 0)
          beta[g, j] ~ normal(coef_override_mu[g, j], coef_override_sd[g, j]);
      }
    }
  }

  alpha_mu ~ normal(alpha_mu_prior_mean, alpha_mu_prior_sd);
  alpha_sd ~ normal(0, alpha_sd_prior_sd);
  if (alpha_parameterization == 2)
    alpha_z ~ normal(alpha_mu, alpha_sd);
  else
    alpha_z ~ std_normal();

  level0_mu ~ normal(level0_mu_prior_mean, level0_mu_prior_sd);
  level0_sd ~ normal(0, level0_sd_prior_sd);
  if (ucm_parameterization == 2)
    level0_raw ~ normal(level0_mu, fmax(level0_sd, 1e-6));
  else
    level0_raw ~ std_normal();
  sigma_level ~ normal(0, sigma_level_prior_sd);
  if (ucm_parameterization == 2)
    level_innov_raw ~ normal(0, fmax(sigma_level, 1e-6));
  else
    level_innov_raw ~ std_normal();

  season_sd ~ normal(0, season_sd_prior_sd);
  if (K_season > 0) {
    for (k in 1:K_season) {
      if (ucm_parameterization == 2)
        season_raw[, k] ~ normal(0, fmax(season_sd[k], 1e-6));
      else
        season_raw[, k] ~ std_normal();
    }
  }

  cycle_sd ~ normal(0, cycle_sd_prior_sd);
  if (K_cycle > 0) {
    for (k in 1:K_cycle) {
      if (ucm_parameterization == 2)
        cycle_raw[, k] ~ normal(0, fmax(cycle_sd[k], 1e-6));
      else
        cycle_raw[, k] ~ std_normal();
    }
  }

  gamma ~ normal(0, gamma_prior_sd);
  sigma_y ~ normal(0, sigma_y_prior_sd);

  if (likelihood_family == 2) {
    for (i in 1:N_train) {
      int n = train_idx[i];
      y[n] ~ student_t(student_t_nu, mu[n], sigma_y);
    }
  } else {
    for (i in 1:N_train) {
      int n = train_idx[i];
      y[n] ~ normal(mu[n], sigma_y);
    }
  }
}
generated quantities {
  vector[N] y_hat = mu;
  vector[N_train] log_lik;
  for (i in 1:N_train) {
    int n = train_idx[i];
    if (likelihood_family == 2)
      log_lik[i] = student_t_lpdf(y[n] | student_t_nu, mu[n], sigma_y);
    else
      log_lik[i] = normal_lpdf(y[n] | mu[n], sigma_y);
  }
}
