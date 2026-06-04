# MMM_DIAGNOSTIC_BUILDER_VERSION: v21_total_curve_support_pairing_anchor_evidence_2026_05_19
# Clean, Stan-free marketing mix diagnostic builder.
# Core principle:
#   - Spend is comparable across channels and drives mix / funnel / intensity diagnostics.
#   - Support is not comparable across unlike units and is used only for curve scale + within-channel reliability.
#   - If modeled_x_col is support, cvalue anchors are built on that support scale, while spend gives business context.

if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")
library(data.table)

MMM_DIAGNOSTIC_BUILDER_VERSION <- "v21_total_curve_support_pairing_anchor_evidence_2026_05_19"
MMM_PACKAGE_PATCH_LEVEL <- MMM_DIAGNOSTIC_BUILDER_VERSION

`%||%` <- function(x, y) if (is.null(x)) y else x

mm_clip <- function(x, lower, upper) pmin(pmax(x, lower), upper)
mm_safe_div <- function(num, den) {
  out <- rep(NA_real_, max(length(num), length(den)))
  num <- rep(num, length.out = length(out))
  den <- rep(den, length.out = length(out))
  ok <- is.finite(num) & is.finite(den) & abs(den) > 1e-12
  out[ok] <- num[ok] / den[ok]
  out
}
mm_to_bool <- function(x, default = TRUE) {
  if (is.null(x)) return(default)
  if (is.logical(x)) return(ifelse(is.na(x), default, x))
  z <- tolower(trimws(as.character(x)))
  out <- z %in% c("true", "t", "1", "yes", "y")
  out[z %in% c("false", "f", "0", "no", "n")] <- FALSE
  out[is.na(z) | z == ""] <- default
  out
}

mm_normalize_funnel <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z[z %in% c("top", "tof", "tofu", "upper_funnel")] <- "upper"
  z[z %in% c("mid", "middle_funnel", "mid_funnel")] <- "middle"
  z[z %in% c("bottom", "bof", "bofu", "lower_funnel")] <- "lower"
  z
}

mm_read_data <- function(input_data = NULL, input_file = NULL, sheet = 1) {
  if (!is.null(input_data)) return(as.data.table(copy(input_data)))
  if (is.null(input_file)) stop("Provide input_data or input_file.")
  ext <- tolower(tools::file_ext(input_file))
  if (ext %in% c("csv", "txt")) return(data.table::fread(input_file))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required for Excel input.")
    return(as.data.table(readxl::read_excel(input_file, sheet = sheet)))
  }
  stop("Unsupported input_file extension: ", ext)
}

mm_parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  out <- suppressWarnings(as.Date(x))
  if (all(is.na(out))) out <- suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30"))
  out
}

mm_apply_holdout_filter <- function(dt,
                                    week_col,
                                    holdout_col = NULL,
                                    holdout_value = TRUE,
                                    holdout_last_n = 0L) {
  out <- as.data.table(copy(dt))
  is_holdout <- rep(FALSE, nrow(out))
  if (!is.null(holdout_col) && nzchar(as.character(holdout_col)[1])) {
    holdout_col <- as.character(holdout_col)[1]
    if (!holdout_col %in% names(out)) stop("holdout_col not found: ", holdout_col)
    hv <- out[[holdout_col]]
    if (is.logical(hv) && identical(holdout_value, TRUE)) {
      is_holdout <- is_holdout | (hv %in% TRUE)
    } else {
      is_holdout <- is_holdout | (as.character(hv) %in% as.character(holdout_value))
    }
  }
  holdout_last_n <- as.integer(holdout_last_n %||% 0L)[1]
  if (is.finite(holdout_last_n) && holdout_last_n > 0L) {
    d <- mm_parse_date(out[[week_col]])
    holdout_dates <- utils::tail(sort(unique(d[!is.na(d)])), holdout_last_n)
    is_holdout <- is_holdout | (d %in% holdout_dates)
  }
  out[, is_diagnostic_training__ := !is_holdout]
  if (!any(out$is_diagnostic_training__)) stop("All rows are holdout. At least one training row is required for diagnostics.")
  list(
    data = out[is_diagnostic_training__ == TRUE][, is_diagnostic_training__ := NULL][],
    audit = data.table(
      input_row_n = nrow(out),
      training_row_n = sum(out$is_diagnostic_training__),
      holdout_row_n = sum(!out$is_diagnostic_training__),
      holdout_col = as.character(holdout_col %||% NA_character_),
      holdout_value = paste(as.character(holdout_value), collapse = "|"),
      holdout_last_n = as.integer(holdout_last_n %||% 0L)
    )
  )
}

mm_force_numeric <- function(dt, col, allow_na = TRUE) {
  if (is.na(col) || !nzchar(col)) return(invisible(FALSE))
  if (!col %in% names(dt)) stop("Column not found: ", col)
  raw <- dt[[col]]
  if (is.numeric(raw)) return(invisible(TRUE))
  raw_chr <- trimws(as.character(raw))
  original_na <- is.na(raw) | raw_chr == ""
  z <- suppressWarnings(as.numeric(raw_chr))
  bad <- is.na(z) & !original_na
  if (any(bad)) {
    examples <- unique(raw_chr[bad])
    examples <- examples[!is.na(examples) & nzchar(examples)]
    stop("Column '", col, "' contains non-numeric values: ", paste(head(examples, 8), collapse = ", "))
  }
  if (!allow_na && any(is.na(z))) stop("Column '", col, "' contains NA values.")
  set(dt, j = col, value = z)
  invisible(TRUE)
}

mm_share_input <- function(x, allowed_keys = NULL, normalize_keys = identity, label = "share input") {
  if (is.null(x)) return(NULL)
  if (is.atomic(x) && !is.null(names(x))) {
    out <- data.table(.share_key = as.character(normalize_keys(names(x))), share = as.numeric(unname(x)))
  } else if (is.data.frame(x)) {
    tmp <- as.data.table(copy(x))
    if (all(c("key", "share") %in% names(tmp))) {
      out <- tmp[, .(.share_key = as.character(normalize_keys(get("key"))), share = as.numeric(get("share")))]
    } else if (all(c("key", "value") %in% names(tmp))) {
      out <- tmp[, .(.share_key = as.character(normalize_keys(get("key"))), share = as.numeric(get("value")))]
    } else {
      candidate_keys <- intersect(c("funnel_position", "channel", "variable", "name"), names(tmp))
      candidate_vals <- intersect(c("share", "value", "weight", "target_share"), names(tmp))
      if (!length(candidate_keys) || !length(candidate_vals)) stop(label, " data.frame needs key/share columns.")
      out <- tmp[, .(.share_key = as.character(normalize_keys(get(candidate_keys[1]))), share = as.numeric(get(candidate_vals[1])))]
    }
  } else {
    stop(label, " must be a named numeric vector or data.frame.")
  }
  if (anyDuplicated(out$.share_key)) stop(label, " has duplicate keys.")
  if (any(!is.finite(out$share))) stop(label, " contains non-finite shares.")
  if (any(out$share < 0)) stop(label, " contains negative shares.")
  if (!is.null(allowed_keys)) {
    missing <- setdiff(allowed_keys, out$.share_key)
    if (length(missing)) out <- rbind(out, data.table(.share_key = missing, share = 0), use.names = TRUE)
    extra <- setdiff(out$.share_key, allowed_keys)
    if (length(extra)) stop(label, " contains unknown keys: ", paste(extra, collapse = ", "))
    idx <- unname(match(allowed_keys, out$.share_key))
    out <- out[idx]
  }
  total <- sum(out$share, na.rm = TRUE)
  if (!is.finite(total) || total <= 0) stop(label, " shares must sum to a positive number.")
  out[, share := share / total]
  setnames(out, ".share_key", "key")
  out[]
}

mm_pacing_metrics <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- 0
  pos <- x[x > 0]
  total <- sum(pmax(x, 0), na.rm = TRUE)
  list(
    active_week_share = mean(x > 0, na.rm = TRUE),
    top_week_share = if (total > 0) max(pmax(x, 0), na.rm = TRUE) / total else NA_real_,
    top4_week_share = if (total > 0) sum(sort(pmax(x, 0), decreasing = TRUE)[seq_len(min(4, length(x)))], na.rm = TRUE) / total else NA_real_,
    effective_active_weeks = if (total > 0) 1 / sum((pmax(x, 0) / total)^2) else 0,
    has_negative_values = any(x < 0, na.rm = TRUE),
    nonzero_median = if (length(pos)) stats::median(pos, na.rm = TRUE) else NA_real_,
    total = total
  )
}

mm_cor <- function(a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  ok <- is.finite(a) & is.finite(b)
  if (sum(ok) < 8 || stats::sd(a[ok]) < 1e-12 || stats::sd(b[ok]) < 1e-12) return(NA_real_)
  as.numeric(stats::cor(a[ok], b[ok]))
}

mm_anchor_from_ir <- function(ir, gamma = 0.35) {
  ir <- mm_clip(ir, 0.05, 20)
  mm_clip(0.50 * (ir ^ gamma), 0.20, 0.85)
}

mm_authority <- function(target_media_intensity, desired_funnel_mix, desired_channel_mix_total, desired_channel_mix_within_funnel) {
  components <- c(
    target_media_intensity = !is.null(target_media_intensity),
    desired_funnel_mix = !is.null(desired_funnel_mix),
    desired_channel_mix_total = !is.null(desired_channel_mix_total),
    desired_channel_mix_within_funnel = !is.null(desired_channel_mix_within_funnel)
  )
  n <- sum(components)
  tier <- if (n >= 3) "high" else if (n >= 1) "medium" else "low"
  weight <- if (tier == "high") 0.80 else if (tier == "medium") 0.50 else 0.18
  list(tier = tier, weight = weight, components = paste(names(components)[components], collapse = ","))
}

mm_flag_band <- function(ratio, low = 0.80, high = 1.25, severe_low = 0.50, severe_high = 2.00) {
  out <- rep("reasonable", length(ratio))
  out[is.na(ratio)] <- "unknown"
  out[ratio < low] <- "below"
  out[ratio > high] <- "above"
  out[ratio < severe_low] <- "severely_below"
  out[ratio > severe_high] <- "severely_above"
  out
}

mm_parse_channel_map <- function(channel_map) {
  cm <- as.data.table(copy(channel_map))
  if (!"variable" %in% names(cm) && "channel" %in% names(cm)) cm[, variable := as.character(channel)]
  if (!"channel" %in% names(cm) && "variable" %in% names(cm)) cm[, channel := as.character(variable)]
  if (!all(c("variable", "channel", "funnel_position") %in% names(cm))) stop("channel_map must contain variable/channel and funnel_position.")
  cm[, variable := as.character(variable)]
  cm[, channel := as.character(channel)]
  cm[, funnel_position := mm_normalize_funnel(funnel_position)]
  if (anyDuplicated(cm$variable)) stop("channel_map has duplicate variable names.")
  if (any(!cm$funnel_position %in% c("upper", "middle", "lower"))) stop("Invalid funnel positions in channel_map.")
  for (nm in c("spend_col", "support_col", "modeled_x_col", "support_type", "modeled_x_basis")) if (!nm %in% names(cm)) cm[, (nm) := NA_character_]
  # Backward compatibility: old map used channel as the modeled/spend column.
  cm[is.na(spend_col) | spend_col == "", spend_col := channel]
  cm[is.na(modeled_x_col) | modeled_x_col == "", modeled_x_col := fifelse(!is.na(support_col) & nzchar(support_col), support_col, spend_col)]
  cm[is.na(modeled_x_basis) | modeled_x_basis == "", modeled_x_basis := fifelse(modeled_x_col == support_col, "support", fifelse(modeled_x_col == spend_col, "spend", "custom"))]
  cm[is.na(support_type) | support_type == "", support_type := NA_character_]
  if (!"include_in_mix_diagnostic" %in% names(cm)) cm[, include_in_mix_diagnostic := !is.na(spend_col) & nzchar(spend_col)]
  cm[, include_in_mix_diagnostic := mm_to_bool(include_in_mix_diagnostic, default = TRUE) & !is.na(spend_col) & nzchar(spend_col)]
  if (!"include_in_curve_anchor" %in% names(cm)) cm[, include_in_curve_anchor := TRUE]
  cm[, include_in_curve_anchor := mm_to_bool(include_in_curve_anchor, default = TRUE)]
  cm[]
}

mm_support_diag_one <- function(dt, variable, spend_col, support_col, modeled_x_col, support_type) {
  spend <- if (!is.na(spend_col) && nzchar(spend_col) && spend_col %in% names(dt)) as.numeric(dt[[spend_col]]) else rep(NA_real_, nrow(dt))
  support <- if (!is.na(support_col) && nzchar(support_col) && support_col %in% names(dt)) as.numeric(dt[[support_col]]) else rep(NA_real_, nrow(dt))
  modeled <- if (!is.na(modeled_x_col) && nzchar(modeled_x_col) && modeled_x_col %in% names(dt)) as.numeric(dt[[modeled_x_col]]) else rep(NA_real_, nrow(dt))
  has_spend <- any(is.finite(spend))
  has_support <- any(is.finite(support))
  input_class <- if (has_spend && has_support) "spend_and_support" else if (has_spend) "spend_only" else if (has_support) "support_only" else "missing_or_invalid"
  cps_week <- mm_safe_div(spend, support)
  cps_active <- cps_week[is.finite(cps_week) & spend > 0 & support > 0]
  spend_zero_support_pos <- mean(is.finite(spend) & spend > 0 & (!is.finite(support) | support <= 0), na.rm = TRUE)
  support_pos_spend_zero <- mean(is.finite(support) & support > 0 & (!is.finite(spend) | spend <= 0), na.rm = TRUE)
  cor_ss <- if (has_spend && has_support) mm_cor(spend, support) else NA_real_
  cps_index_iqr <- if (length(cps_active) >= 8 && is.finite(stats::median(cps_active)) && stats::median(cps_active) > 0) stats::IQR(cps_active) / stats::median(cps_active) else NA_real_
  mismatch_flag <- isTRUE(cor_ss < 0.30) || isTRUE(spend_zero_support_pos > 0.05) || isTRUE(support_pos_spend_zero > 0.05) || isTRUE(cps_index_iqr > 1.50)
  data.table(
    variable = variable,
    spend_col = spend_col,
    support_col = support_col,
    modeled_x_col = modeled_x_col,
    support_type = support_type,
    input_class = input_class,
    spend_total = sum(pmax(spend, 0), na.rm = TRUE),
    support_total = sum(pmax(support, 0), na.rm = TRUE),
    modeled_x_total = sum(pmax(modeled, 0), na.rm = TRUE),
    cost_per_support_median = if (length(cps_active)) stats::median(cps_active, na.rm = TRUE) else NA_real_,
    cost_per_support_iqr_to_median = cps_index_iqr,
    spend_support_cor = cor_ss,
    spend_positive_support_zero_share = spend_zero_support_pos,
    support_positive_spend_zero_share = support_pos_spend_zero,
    spend_support_mismatch_flag = mismatch_flag,
    support_comparability_note = "Support is only compared within variable or same support_type; never across unlike units."
  )
}

diagnose_marketing_mix <- function(input_data = NULL,
                                   input_file = NULL,
                                   sheet = 1,
                                   week_col = "week",
                                   sales_col = NULL,
                                   holdout_col = NULL,
                                   holdout_value = TRUE,
                                   holdout_last_n = 0L,
                                   channel_map,
                                   additional_support_map = NULL,
                                   target_media_intensity = NULL,
                                   desired_funnel_mix = NULL,
                                   desired_channel_mix_total = NULL,
                                   desired_channel_mix_within_funnel = NULL,
                                   curve_gamma = 0.35,
                                   neutral_anchor = 0.50,
                                   return_wrapper = TRUE,
                                   verbose = FALSE,
                                   anchor_authority_override = NULL,
                                   anchor_authority_weight_override = NULL,
                                   ...) {
  dt <- mm_read_data(input_data, input_file, sheet)
  if (!week_col %in% names(dt)) stop("week_col not found: ", week_col)
  holdout_info <- mm_apply_holdout_filter(
    dt = dt,
    week_col = week_col,
    holdout_col = holdout_col,
    holdout_value = holdout_value,
    holdout_last_n = holdout_last_n
  )
  dt <- holdout_info$data
  cm <- mm_parse_channel_map(channel_map)

  needed_cols <- unique(na.omit(c(cm$spend_col, cm$support_col, cm$modeled_x_col)))
  missing_needed <- setdiff(needed_cols, names(dt))
  if (length(missing_needed)) stop("Mapped columns missing from input data: ", paste(missing_needed, collapse = ", "))
  for (cc in needed_cols) mm_force_numeric(dt, cc, allow_na = TRUE)
  has_sales <- !is.null(sales_col) && sales_col %in% names(dt)
  if (has_sales) mm_force_numeric(dt, sales_col, allow_na = TRUE)
  brand_sales <- if (has_sales) sum(dt[[sales_col]], na.rm = TRUE) else NA_real_

  support_diag <- rbindlist(lapply(seq_len(nrow(cm)), function(i) mm_support_diag_one(dt, cm$variable[i], cm$spend_col[i], cm$support_col[i], cm$modeled_x_col[i], cm$support_type[i])), use.names = TRUE, fill = TRUE)

  if (!is.null(additional_support_map)) {
    asm <- as.data.table(copy(additional_support_map))
    if (!all(c("variable", "support_col") %in% names(asm))) stop("additional_support_map must contain variable and support_col.")
    if (!"support_type" %in% names(asm)) asm[, support_type := NA_character_]
    miss_add <- setdiff(asm$support_col, names(dt))
    if (length(miss_add)) stop("additional_support_map columns missing from input data: ", paste(miss_add, collapse = ", "))
    for (cc in unique(asm$support_col)) mm_force_numeric(dt, cc, allow_na = TRUE)
    additional_support_diagnostic <- rbindlist(lapply(seq_len(nrow(asm)), function(i) {
      v <- as.character(asm$variable[i]); scol <- as.character(asm$support_col[i])
      s <- as.numeric(dt[[scol]]); m <- mm_pacing_metrics(s)
      data.table(variable = v, support_col = scol, support_type = as.character(asm$support_type[i]), support_total = m$total,
                 support_active_week_share = m$active_week_share, support_top4_week_share = m$top4_week_share,
                 support_has_negative_values = m$has_negative_values,
                 note = "Auxiliary support metric; used for QA only unless selected as modeled_x_col.")
    }), use.names = TRUE, fill = TRUE)
  } else {
    additional_support_diagnostic <- data.table(variable = character(), support_col = character(), support_type = character())
  }

  spend_cm <- cm[include_in_mix_diagnostic == TRUE]
  if (!nrow(spend_cm)) stop("No channels have spend_col/include_in_mix_diagnostic=TRUE. Spend is required for mix/funnel allocation diagnostics.")
  variables <- spend_cm$variable
  channel_totals <- rbindlist(lapply(seq_len(nrow(spend_cm)), function(i) {
    data.table(variable = spend_cm$variable[i], channel = spend_cm$channel[i], funnel_position = spend_cm$funnel_position[i],
               channel_total = sum(pmax(as.numeric(dt[[spend_cm$spend_col[i]]]), 0), na.rm = TRUE),
               spend_col = spend_cm$spend_col[i], support_col = spend_cm$support_col[i], modeled_x_col = spend_cm$modeled_x_col[i], modeled_x_basis = spend_cm$modeled_x_basis[i])
  }), use.names = TRUE, fill = TRUE)
  total_media <- sum(channel_totals$channel_total, na.rm = TRUE)
  actual_media_intensity <- mm_safe_div(total_media, brand_sales)
  target_intensity <- target_media_intensity %||% actual_media_intensity
  if (!is.finite(target_intensity) || target_intensity <= 0) target_intensity <- actual_media_intensity
  if (!is.finite(target_intensity) || target_intensity <= 0) target_intensity <- 0.10

  funnel_keys <- c("upper", "middle", "lower")
  desired_funnel <- mm_share_input(desired_funnel_mix, allowed_keys = funnel_keys, normalize_keys = mm_normalize_funnel, label = "desired_funnel_mix")
  if (is.null(desired_funnel)) {
    desired_funnel <- data.table(.share_key = funnel_keys, share = rep(1 / 3, 3))
    setnames(desired_funnel, ".share_key", "key")
  }
  desired_total <- mm_share_input(desired_channel_mix_total, allowed_keys = variables, normalize_keys = as.character, label = "desired_channel_mix_total")
  if (is.null(desired_total)) {
    desired_total <- data.table(.share_key = variables, share = rep(1 / length(variables), length(variables)))
    setnames(desired_total, ".share_key", "key")
  }

  channel_long <- channel_totals[]
  channel_long[, actual_channel_share_total := mm_safe_div(channel_total, total_media)]
  funnel_actual <- channel_long[, .(funnel_total = sum(channel_total, na.rm = TRUE)), by = funnel_position]
  funnel_actual <- merge(data.table(funnel_position = funnel_keys), funnel_actual, by = "funnel_position", all.x = TRUE)
  funnel_actual[is.na(funnel_total), funnel_total := 0]
  funnel_actual[, actual_funnel_share := mm_safe_div(funnel_total, total_media)]
  funnel_actual <- merge(funnel_actual, desired_funnel[, .(funnel_position = key, desired_funnel_share = share)], by = "funnel_position", all.x = TRUE)
  funnel_actual[, target_funnel_spend := total_media * desired_funnel_share]
  funnel_actual[, funnel_spend_ratio := mm_safe_div(funnel_total, target_funnel_spend)]
  funnel_actual[, funnel_status := mm_flag_band(funnel_spend_ratio)]
  funnel_actual[, funnel_vs_brand_intensity := mm_safe_div(funnel_total, brand_sales)]

  channel_long <- merge(channel_long, desired_total[, .(variable = key, desired_channel_share_total = share)], by = "variable", all.x = TRUE)
  channel_long <- merge(channel_long, funnel_actual[, .(funnel_position, funnel_total, desired_funnel_share, target_funnel_spend)], by = "funnel_position", all.x = TRUE)
  channel_long[, channel_share_within_funnel := mm_safe_div(channel_total, funnel_total)]
  channel_long[, desired_channel_spend_total := total_media * desired_channel_share_total]
  channel_long[, channel_vs_total_ratio := mm_safe_div(channel_total, desired_channel_spend_total)]
  if (!is.null(desired_channel_mix_within_funnel)) {
    within <- as.data.table(copy(desired_channel_mix_within_funnel))
    if (!all(c("variable", "share") %in% names(within)) && all(c("channel", "share") %in% names(within))) setnames(within, "channel", "variable")
    if (!all(c("variable", "share") %in% names(within))) stop("desired_channel_mix_within_funnel must contain variable/channel and share.")
    within[, variable := as.character(variable)]
    within[, share := as.numeric(share)]
    if (!"funnel_position" %in% names(within)) within <- merge(within, cm[, .(variable, funnel_position)], by = "variable", all.x = TRUE)
    within[, funnel_position := mm_normalize_funnel(funnel_position)]
    if (any(!is.finite(within$share)) || any(within$share < 0)) stop("desired_channel_mix_within_funnel has invalid shares.")
    within[, desired_channel_share_within_funnel := share / sum(share, na.rm = TRUE), by = funnel_position]
    channel_long <- merge(channel_long, within[, .(variable, desired_channel_share_within_funnel)], by = "variable", all.x = TRUE)
    channel_long[is.na(desired_channel_share_within_funnel), desired_channel_share_within_funnel := 1 / .N, by = funnel_position]
  } else {
    channel_long[, desired_channel_share_within_funnel := 1 / .N, by = funnel_position]
  }
  channel_long[, desired_channel_spend_within_funnel := funnel_total * desired_channel_share_within_funnel]
  channel_long[, channel_within_funnel_ratio := mm_safe_div(channel_total, desired_channel_spend_within_funnel)]
  channel_long[, channel_total_status := mm_flag_band(channel_vs_total_ratio)]
  channel_long[, channel_within_funnel_status := mm_flag_band(channel_within_funnel_ratio)]
  channel_long[, channel_vs_brand_intensity := mm_safe_div(channel_total, brand_sales)]

  pacing <- rbindlist(lapply(seq_len(nrow(cm)), function(i) {
    xcol <- cm$modeled_x_col[i]
    m <- mm_pacing_metrics(dt[[xcol]])
    data.table(variable = cm$variable[i], modeled_x_active_week_share = m$active_week_share, modeled_x_top_week_share = m$top_week_share,
               modeled_x_top4_week_share = m$top4_week_share, modeled_x_effective_active_weeks = m$effective_active_weeks,
               modeled_x_has_negative_values = m$has_negative_values, modeled_x_nonzero_median = m$nonzero_median)
  }), use.names = TRUE, fill = TRUE)
  channel_long <- merge(channel_long, pacing, by = "variable", all.x = TRUE)
  channel_long <- merge(channel_long, support_diag, by = "variable", all.x = TRUE, suffixes = c("", "_supportdiag"))

  auth <- mm_authority(target_media_intensity, desired_funnel_mix, desired_channel_mix_total, desired_channel_mix_within_funnel)
  if (!is.null(anchor_authority_override)) auth$tier <- as.character(anchor_authority_override)
  if (!is.null(anchor_authority_weight_override)) auth$weight <- as.numeric(anchor_authority_weight_override)
  auth$weight <- mm_clip(auth$weight, 0, 0.95)

  channel_long[, ir_brand := mm_safe_div(channel_vs_brand_intensity, target_intensity * desired_channel_share_total)]
  channel_long[, ir_funnel := mm_safe_div(channel_total, desired_channel_spend_within_funnel)]
  channel_long[, raw_anchor_from_brand := mm_anchor_from_ir(ir_brand, curve_gamma)]
  channel_long[, raw_anchor_from_funnel := mm_anchor_from_ir(ir_funnel, curve_gamma)]
  channel_long[, raw_anchor_combined := rowMeans(cbind(raw_anchor_from_brand, raw_anchor_from_funnel), na.rm = TRUE)]
  channel_long[!is.finite(raw_anchor_combined), raw_anchor_combined := neutral_anchor]
  channel_long[, reliability := 1.0]
  channel_long[modeled_x_has_negative_values == TRUE, reliability := 0]
  channel_long[input_class == "support_only", reliability := pmin(reliability, 0.55)]
  channel_long[spend_support_mismatch_flag == TRUE, reliability := pmin(reliability, 0.55)]
  channel_long[modeled_x_active_week_share < 0.08, reliability := pmin(reliability, 0.25)]
  channel_long[modeled_x_active_week_share >= 0.08 & modeled_x_active_week_share < 0.20, reliability := pmin(reliability, 0.45)]
  channel_long[modeled_x_top4_week_share > 0.70, reliability := pmin(reliability, 0.35)]
  channel_long[modeled_x_top4_week_share > 0.50 & modeled_x_top4_week_share <= 0.70, reliability := pmin(reliability, 0.60)]
  channel_long[ir_brand > 5 | ir_brand < 0.20 | ir_funnel > 5 | ir_funnel < 0.20, reliability := pmin(reliability, 0.70)]
  channel_long[, anchor_weight_final := mm_clip(auth$weight * reliability, 0, 0.95)]
  channel_long[, anchor_saturation_recommended := neutral_anchor + anchor_weight_final * (raw_anchor_combined - neutral_anchor)]
  if (auth$tier == "low") channel_long[, anchor_saturation_recommended := mm_clip(anchor_saturation_recommended, 0.46, 0.54)]
  channel_long[, anchor_saturation_handoff := mm_clip(anchor_saturation_recommended, 0.20, 0.85)]
  channel_long[, cvalue_multiplier_handoff := mm_safe_div(-log(1 - anchor_saturation_handoff), -log(1 - neutral_anchor))]
  channel_long[modeled_x_has_negative_values == TRUE, `:=`(anchor_saturation_handoff = neutral_anchor, cvalue_multiplier_handoff = 1, anchor_weight_final = 0)]
  channel_long[, anchor_authority_tier := auth$tier]
  channel_long[, anchor_authority_weight := auth$weight]
  channel_long[, anchor_authority_components := auth$components]
  channel_long[, anchor_uncertainty_width_90 := mm_clip((1 - anchor_weight_final) * 0.40 + (1 - reliability) * 0.20, 0.05, 0.60)]
  channel_long[, anchor_lower_90 := mm_clip(anchor_saturation_handoff - anchor_uncertainty_width_90 / 2, 0.05, 0.95)]
  channel_long[, anchor_upper_90 := mm_clip(anchor_saturation_handoff + anchor_uncertainty_width_90 / 2, 0.05, 0.95)]
  channel_long[, anchor_actionability_tier := fifelse(anchor_weight_final >= 0.60, "actionable", fifelse(anchor_weight_final >= 0.30, "directional", "weak_or_neutral"))]
  channel_long[, anchor_should_drive_curve_prior := anchor_actionability_tier == "actionable" & reliability >= 0.60 & anchor_authority_tier %in% c("high", "medium")]
  channel_long[, anchor_defensibility_note := paste0("authority=", anchor_authority_tier, "; reliability=", round(reliability, 2), "; shrink_weight=", round(anchor_weight_final, 2), "; actionability=", anchor_actionability_tier, "; modeled_x=", modeled_x_col, "; input_class=", input_class)]

  total_media_diagnostic <- data.table(
    total_media = total_media,
    brand_sales = brand_sales,
    actual_media_intensity = actual_media_intensity,
    target_media_intensity = target_intensity,
    media_intensity_ratio = mm_safe_div(actual_media_intensity, target_intensity),
    media_intensity_status = mm_flag_band(mm_safe_div(actual_media_intensity, target_intensity)),
    anchor_authority_tier = auth$tier,
    anchor_authority_weight = auth$weight,
    anchor_authority_components = auth$components,
    mix_basis = "spend_only_for_cross_channel_comparability"
  )
  channel_diagnostic <- channel_long[, .(
    variable, channel, funnel_position, spend_col, support_col, support_type, modeled_x_col, modeled_x_basis,
    input_class, channel_total, actual_channel_share_total, desired_channel_share_total,
    channel_vs_total_ratio, channel_total_status, channel_share_within_funnel,
    desired_channel_share_within_funnel, channel_within_funnel_ratio, channel_within_funnel_status,
    channel_vs_brand_intensity, modeled_x_active_week_share, modeled_x_top_week_share, modeled_x_top4_week_share,
    modeled_x_effective_active_weeks, modeled_x_has_negative_values, reliability, anchor_actionability_tier, anchor_should_drive_curve_prior,
    spend_total, support_total, modeled_x_total, cost_per_support_median, cost_per_support_iqr_to_median,
    spend_support_cor, spend_support_mismatch_flag, support_comparability_note
  )]
  curve_prior_inputs <- channel_long[, .(
    variable, channel, funnel_position, spend_col, support_col, support_type, modeled_x_col, modeled_x_basis,
    input_class, anchor_saturation_handoff, cvalue_multiplier_handoff,
    anchor_saturation_recommended, raw_anchor_combined, anchor_lower_90, anchor_upper_90,
    anchor_uncertainty_width_90, anchor_authority_tier, anchor_authority_weight,
    anchor_authority_components, anchor_weight_final, reliability, anchor_actionability_tier,
    anchor_defensibility_note, modeled_x_active_week_share, modeled_x_top4_week_share, modeled_x_has_negative_values,
    spend_support_cor, spend_support_mismatch_flag, channel_within_funnel_ratio, channel_vs_total_ratio,
    ir_brand, ir_funnel, anchor_should_drive_curve_prior
  )]
  out <- list(
    total_media_diagnostic = total_media_diagnostic,
    funnel_diagnostic = funnel_actual[],
    channel_diagnostic = channel_diagnostic[],
    support_diagnostic = support_diag[],
    additional_support_diagnostic = additional_support_diagnostic[],
    curve_prior_inputs = curve_prior_inputs[],
    holdout_audit = holdout_info$audit,
    input_summary = data.table(n_rows = nrow(dt), n_channels = nrow(cm), n_spend_mix_channels = nrow(spend_cm), week_col = week_col, sales_col = sales_col %||% NA_character_),
    version = MMM_DIAGNOSTIC_BUILDER_VERSION
  )
  if (isTRUE(verbose)) print(total_media_diagnostic)
  validation_rows <- list()
  if (!has_sales) validation_rows[[length(validation_rows) + 1L]] <- data.table(severity = "info", check = "sales_col", message = "No usable sales_col was provided; intensity diagnostics use a fallback target intensity and should be treated as directional.")
  if (any(channel_long$modeled_x_has_negative_values, na.rm = TRUE)) validation_rows[[length(validation_rows) + 1L]] <- data.table(severity = "error", check = "modeled_x_negative", message = "At least one modeled_x_col contains negative values; curve anchors are reset to neutral for affected variables.")
  if (any(channel_long$spend_support_mismatch_flag, na.rm = TRUE)) validation_rows[[length(validation_rows) + 1L]] <- data.table(severity = "warning", check = "spend_support_mismatch", message = "At least one variable has weak spend/support pairing; anchor reliability was reduced.")
  if (any(channel_long$anchor_actionability_tier == "weak_or_neutral", na.rm = TRUE)) validation_rows[[length(validation_rows) + 1L]] <- data.table(severity = "info", check = "weak_anchor", message = "At least one curve anchor is weak/neutral and should not heavily drive the curve prior.")
  validation <- if (length(validation_rows)) rbindlist(validation_rows, use.names = TRUE, fill = TRUE) else data.table(severity = "ok", check = "diagnostic", message = "No high-level diagnostic warnings were triggered.")

  if (isTRUE(return_wrapper)) return(list(out = out, validation = validation[]))
  out
}

enhance_marketing_mix_diagnostic <- function(diagnostic_output, ...) {
  if (is.list(diagnostic_output) && !is.null(diagnostic_output$out)) diagnostic_output$out else diagnostic_output
}

make_curve_anchors_from_diagnostic <- function(diagnostic_output,
                                               variable_col = "variable",
                                               anchor_col = "anchor_saturation_handoff",
                                               multiplier_col = "cvalue_multiplier_handoff") {
  obj <- if (is.list(diagnostic_output) && !is.null(diagnostic_output$out)) diagnostic_output$out else diagnostic_output
  cpi <- if (is.data.frame(obj)) as.data.table(copy(obj)) else as.data.table(copy(obj$curve_prior_inputs))
  required <- c(variable_col, anchor_col, multiplier_col)
  missing <- setdiff(required, names(cpi))
  if (length(missing)) stop("Diagnostic curve anchor table missing columns: ", paste(missing, collapse = ", "))
  out <- cpi[, .(
    variable = as.character(get(variable_col)),
    anchor_saturation = as.numeric(get(anchor_col)),
    anchor_saturation_handoff = as.numeric(get(anchor_col)),
    cvalue_multiplier = as.numeric(get(multiplier_col)),
    cvalue_multiplier_handoff = as.numeric(get(multiplier_col)),
    modeled_x_col = if ("modeled_x_col" %in% names(cpi)) as.character(modeled_x_col) else NA_character_,
    modeled_x_basis = if ("modeled_x_basis" %in% names(cpi)) as.character(modeled_x_basis) else NA_character_,
    support_type = if ("support_type" %in% names(cpi)) as.character(support_type) else NA_character_,
    anchor_authority_tier = if ("anchor_authority_tier" %in% names(cpi)) as.character(anchor_authority_tier) else NA_character_,
    anchor_actionability_tier = if ("anchor_actionability_tier" %in% names(cpi)) as.character(anchor_actionability_tier) else NA_character_,
    anchor_should_drive_curve_prior = if ("anchor_should_drive_curve_prior" %in% names(cpi)) as.logical(anchor_should_drive_curve_prior) else NA,
    anchor_weight_final = if ("anchor_weight_final" %in% names(cpi)) as.numeric(anchor_weight_final) else NA_real_,
    reliability = if ("reliability" %in% names(cpi)) as.numeric(reliability) else NA_real_,
    anchor_uncertainty_width_90 = if ("anchor_uncertainty_width_90" %in% names(cpi)) as.numeric(anchor_uncertainty_width_90) else NA_real_,
    anchor_lower_90 = if ("anchor_lower_90" %in% names(cpi)) as.numeric(anchor_lower_90) else NA_real_,
    anchor_upper_90 = if ("anchor_upper_90" %in% names(cpi)) as.numeric(anchor_upper_90) else NA_real_,
    spend_support_mismatch_flag = if ("spend_support_mismatch_flag" %in% names(cpi)) as.logical(spend_support_mismatch_flag) else NA
  )]
  out[]
}
