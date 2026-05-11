# --- TIMSS SES & Learning Gradients (Europe + Balkans) -------------------------
library(dplyr)
library(tidyr)
library(survey)
library(broom)  
library(lavaan)
library(pandoc)
library(Hmisc)  # for weighted percentiles
library(ggplot2)

# --- Config ---------------------------------------------------
years <- c(2019, 2023)
include_country_percentiles <- FALSE

# add parental investment index and tertile labels (1=Low, 2=Medium, 3=High) as independent variables
indep_vars <- c("parentB_high_edu", "ASDHEDUP_binary", "SES_index", "parental_investment_index", "PI_labels")
indep_labels <- c(
  parentB_high_edu          = "ParentB_HighEdu",
  ASDHEDUP_binary           = "HighestEdu",
  SES_index                 = "SES",
  parental_investment_index = "Parental_Invest",
  PI_labels                 = "PI_labels"
)

# parental investment items and anchor
# To exclude items with high missing data, add them to exclude_items (e.g., exclude_items = c("ASBH01C"))
parental_items_all <- sprintf("ASBH01%s", LETTERS[1:18])  # ASBH01A..ASBH01R
exclude_items <- c()  # Items to exclude from the model (e.g., c("ASBH01C"))
parental_items <- setdiff(parental_items_all, exclude_items)
parental_anchor <- "ASBH01E"

outcomes <- list(
  math              = list(code = "MMAT", label = "Math"),
  math_reasoning    = list(code = "MREA", label = "Math reasoning"),
  science           = list(code = "SSCI", label = "Science"),
  science_reasoning = list(code = "SREA", label = "Science reasoning")
)

# Weighted quantile: uses Hmisc::wtd.quantile with weights w; returns NA if too few valid (x,w) pairs.
safe_wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & (w > 0)
  if (sum(ok) < 2) return(rep(NA_real_, length(probs)))
  Hmisc::wtd.quantile(x[ok], weights = w[ok], probs = probs, na.rm = TRUE)
}

# Country names and ISO3/Is_Balkan come from the master table; country_lookup built below.

# ---- Latent parental investment (CFA + percentiles + tertiles) ----
# WHAT: One-factor CFA on ASBH01A–R (anchor ASBH01E=1), then latent scores → regional percentiles
#       and tertiles (PI_labels 1=Low, 2=Medium, 3=High). HOW: lavaan::cfa(ordered=items),
#       lavPredict for factor scores; weighted quantiles (TOTWGT) for percentile/tertile cutpoints.
# Model: I =~ 1*ASBH01E + ASBH01A + ASBH01B + ... (anchor ASBH01E with loading fixed to 1)
# Intercept of anchor fixed to 0 for identification
# Missing data: Uses pairwise deletion (default for ordered categorical with WLSMV estimator)
# This retains observations with partial data by using all available variable pairs
get_latent_variable <- function(df_year) {
  # Find which parental investment items are available in the data
  items <- parental_items[parental_items %in% names(df_year)]
  if (!(parental_anchor %in% items)) {
    df_year$parental_investment_index <- NA_real_
    df_year$parental_investment_score <- NA_real_
    df_year$PI_labels <- NA_integer_
    return(df_year)
  }
  # Track missing data patterns before fitting
  n_total <- nrow(df_year)
  complete_mask <- complete.cases(df_year[, items, drop = FALSE])
  n_complete <- sum(complete_mask)
  n_missing <- n_total - n_complete
  # Count missing per item - check actual coding in data
  # Common patterns: 9/99=Omitted/invalid, NA/Sysmis=Not administered
  missing_by_item <- sapply(items, function(item) {
    sum(is.na(df_year[[item]]) | df_year[[item]] %in% c(9, 99, 999, 9999))
  })
  # Build model string: anchor has loading 1, other items load freely
  other_items <- setdiff(items, parental_anchor)
  PI_model <- paste0(
    "I =~ 1*", parental_anchor,
    if (length(other_items) > 0) paste0(" + ", paste(other_items, collapse = " + ")) else "",
    "\n", parental_anchor, " ~ 0*1\n"
  )
  # Fit CFA model for ordered categorical items
  # Note: For ordered categorical variables, lavaan uses WLSMV estimator by default,
  # which handles missing data via pairwise deletion (uses all available pairs of variables).
  fit <- tryCatch(
    cfa(PI_model, data = df_year, ordered = items, std.lv = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    df_year$parental_investment_index <- NA_real_
    df_year$parental_investment_score <- NA_real_
    df_year$PI_labels <- NA_integer_
    message("Warning: CFA model failed to fit. All scores set to NA.")
    return(df_year)
  }
  # Predict latent factor scores
  # For ordered categorical with missing data, lavPredict handles missingness
  scores_raw <- tryCatch({
    pred <- lavPredict(fit, type = "lv")
    if (is.matrix(pred)) {
      if ("I" %in% colnames(pred)) pred[, "I"] else pred[, 1]
    } else {
      pred
    }
  }, error = function(e) NULL)
  if (is.null(scores_raw)) {
    df_year$parental_investment_index <- NA_real_
    df_year$parental_investment_score <- NA_real_
    df_year$PI_labels <- NA_integer_
    message("Warning: Score prediction failed. All scores set to NA.")
    return(df_year)
  }
  # Initialize score column with NAs
  df_year$parental_investment_score <- NA_real_
  # lavPredict should return scores in the same order as input data
  # If length matches, assign directly; otherwise assign to complete cases
  if (length(scores_raw) == n_total) {
    df_year$parental_investment_score <- as.numeric(scores_raw)
  } else if (length(scores_raw) == n_complete) {
    # If only complete cases returned, assign to those rows
    df_year$parental_investment_score[complete_mask] <- as.numeric(scores_raw)
  } else {
    # Unexpected length - warn and try to assign what we can
    message(sprintf("Warning: Score length (%d) != expected (%d or %d). Some scores may be missing.",
                    length(scores_raw), n_total, n_complete))
    n_assign <- min(length(scores_raw), n_total)
    df_year$parental_investment_score[1:n_assign] <- as.numeric(scores_raw[1:n_assign])
  }
  # Report missing data statistics
  n_with_scores <- sum(!is.na(df_year$parental_investment_score))
  if (n_missing > 0) {
    message(sprintf("Parental Investment Latent Factor: %d/%d observations have scores (%.1f%%). %d complete cases, %d with partial data.",
                    n_with_scores, n_total, 100*n_with_scores/n_total, n_complete, n_missing))
    if (any(missing_by_item > 0)) {
      items_with_missing <- names(missing_by_item)[missing_by_item > 0]
      max_missing_item <- names(missing_by_item)[which.max(missing_by_item)]
      message(sprintf("  Item with most missing: %s (%d missing, %.1f%%)",
                      max_missing_item, max(missing_by_item), 100*max(missing_by_item)/n_total))
    }
  }
  # Convert scores to regional percentiles (1-100) using weighted quantiles for this year
  # Only use non-missing scores for percentile calculation
  probs <- seq(0, 1, 0.01)
  q <- safe_wtd_quantile(df_year$parental_investment_score, df_year$TOTWGT, probs)
  q <- unique(q)
  if (length(q) < 2) {
    df_year$parental_investment_index <- NA_integer_
  } else {
    df_year$parental_investment_index <- cut(
      df_year$parental_investment_score,
      breaks = q,
      labels = FALSE,
      include.lowest = TRUE
    )
  }
  # Tertiles (1=Low bottom 33%, 2=Medium 33–66%, 3=High top 33%) for PI_labels use in regressions
  q33_66 <- safe_wtd_quantile(df_year$parental_investment_score, df_year$TOTWGT, c(1/3, 2/3))
  df_year$PI_labels <- NA_integer_
  ok <- is.finite(df_year$parental_investment_score) & is.finite(df_year$TOTWGT) & df_year$TOTWGT > 0
  if (sum(ok) >= 2 && all(is.finite(q33_66))) {
    tert <- cut(
      df_year$parental_investment_score[ok],
      breaks = c(-Inf, q33_66[1], q33_66[2], Inf),
      labels = 1:3,
      include.lowest = TRUE,
      right = TRUE
    )
    df_year$PI_labels[ok] <- as.integer(as.character(tert))
  }
  df_year
}

# Plot and save distribution of parental investment latent factor (for inspection of shape).
plot_parental_investment_distribution <- function(df_year, year, base_dir) {
  if (!"parental_investment_score" %in% names(df_year)) return(invisible(NULL))
  x <- df_year$parental_investment_score
  x <- x[is.finite(x)]
  if (length(x) < 10) return(invisible(NULL))
  fig_dir <- file.path(base_dir, "output", "figures")
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  p <- ggplot(data.frame(score = x), aes(x = score)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, fill = wb_colors$blue_light, colour = "white", linewidth = 0.2) +
    geom_density(colour = wb_colors$blue_dark, linewidth = 1) +
    labs(
      title = sprintf("Distribution of parental investment latent factor (Grade 4, %d)", year),
      x = "Latent factor score",
      y = "Density"
    ) +
    theme_minimal(base_size = 11)
  path <- file.path(fig_dir, sprintf("parental_investment_score_dist_%d.png", year))
  ggsave(path, plot = p, width = 7, height = 4, dpi = 150)
  cat("Saved distribution plot:", path, "\n")
  invisible(path)
}

# ---- Plausible values, jackknife, and Rubin's rules ----
# Outcomes are PV percentile variables (e.g. ASMMAT01_ptile_region) built in BUILD_MASTER_TABLES.R
# from TIMSS-provided PVs. We run one regression per PV (5 regressions); each uses JK2 replicate
# weights (from JKZONE, JKREP) for sampling SE. Rubin's rules then combine the 5 estimates and SEs
# into one point estimate and total SE (see combine_pv_results: mean estimate, SE = sqrt(W + (1+1/M)*B)).

# WHAT: Returns PV base name (e.g. "ASMMAT") if all 5 PVs exist. HOW: paste0("AS", code), check names(df).
get_pv_base <- function(df, outcome_code) {
  base <- paste0("AS", outcome_code)
  pv_vars <- sprintf("%s%02d", base, 1:5)
  if (!all(pv_vars %in% names(df))) return(NULL)
  base
}

# ---- JK2 replicate weights (add_rwgt_to_data) ----
# Jackknife variance = "leave one group out, see how the estimate changes." TIMSS gives JKZONE
# (which PSU/zone) and JKREP (0 or 1). For each zone i there are 2 replicates:
#   - Replicate where zone i is "out": units in zone i get weight 0; others keep TOTWGT.
#   - Replicate where zone i is "in": units in zone i get 2*TOTWGT (double) to compensate so the
#     two replicate totals are balanced; others keep TOTWGT.
# So for rwgt_i: if JKZONE==i & JKREP==1 → 2*TOTWGT; if JKZONE==i & JKREP==0 → 0; else TOTWGT.
# The factor 2 (n_rep) is the JK2 convention so that the mean of the replicate estimates is
# (approximately) the full-sample estimate and their variance estimates sampling variance.
# survey::svrepdesign(..., type="JK2") then uses these columns to compute SE (refits with each
# rwgt_* in turn and combines the variability).
add_rwgt_to_data <- function(data) {
  need <- c("TOTWGT", "JKZONE", "JKREP")
  if (!all(need %in% names(data))) return(data)
  n_rep <- 2L
  njk <- max(data$JKZONE, na.rm = TRUE)
  if (!is.finite(njk) || njk < 2) return(data)
  for (i in 1:njk) {
    data[[paste0("rwgt_", i)]] <- ifelse(
      data$JKZONE == i & data$JKREP == 1,
      n_rep * data$TOTWGT,
      ifelse(data$JKZONE == i & data$JKREP == 0, 0, data$TOTWGT)
    )
  }
  data
}

# One svyrep.design (JK2) for the full year data. HOW: svrepdesign(weights=TOTWGT, repweights=rwgt_*).
# Lets us subset by country/group and run svyglm without rebuilding weights each time.
# (suppressWarnings: survey warns that scale/rscales are ignored for JK2—we are not passing them; harmless.)
build_year_design <- function(data) {
  rwgt_vars <- grep("^rwgt_[0-9]+$", names(data), value = TRUE)
  if (length(rwgt_vars) < 2) return(NULL)
  ok <- complete.cases(data[, c("TOTWGT", "JKZONE", "JKREP", rwgt_vars[1])])
  if (sum(ok) < 10) return(NULL)
  d <- data[ok, ]
  suppressWarnings(
    svrepdesign(weights = ~TOTWGT, repweights = d[rwgt_vars], type = "JK2", combined.weights = TRUE, data = d)
  )
}

# ---- Single regression with JK2 SE (run_regression_jk_design) ----
# Fit dep_var ~ indep_var with survey weights. Point estimate uses TOTWGT (full sample).
# SE is computed by the survey package using the replicate weights (rwgt_1, rwgt_2, ...): it
# refits the model with each replicate weight in turn and uses the spread of those estimates
# to estimate sampling variance. So we get one beta and one SE per call (e.g. per PV, per country).
# HOW: Subset design to rows with non-missing dep_var and indep_var; svyglm(form, design=dsub);
# return the coefficient and std.error for the slope (indep_var).
# If svyglm fails (e.g. NA/infinite in a replicate estimate), return NULL so that country/PV gets NA and the run continues.
run_regression_jk_design <- function(design, dep_var, indep_var) {
  if (!all(c(dep_var, indep_var) %in% names(design$variables))) return(NULL)
  idx <- complete.cases(design$variables[, c(dep_var, indep_var)])
  if (sum(idx) < 10) return(NULL)
  dsub <- design[idx, ]
  form <- as.formula(paste(dep_var, "~", indep_var))
  fit <- tryCatch(svyglm(form, design = dsub), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  out <- broom::tidy(fit) %>% filter(term == indep_var) %>% select(estimate, std.error)
  if (nrow(out) == 0 || !all(is.finite(out$estimate)) || !all(is.finite(out$std.error))) return(NULL)
  out
}

# Run one regression per PV; returns long table (estimate, std.error, PV). Design-only path.
run_pv_regressions_design <- function(design, indep_var, pv_vars) {
  bind_rows(lapply(pv_vars, function(pv) {
    coef <- run_regression_jk_design(design, pv, indep_var)
    if (is.null(coef)) return(NULL)
    mutate(coef, PV = pv)
  }))
}

# WHAT: Rubin's rules: one point estimate and total SE from M PV results. HOW: beta = mean(estimate);
#       W = mean(std.error^2), B = var(estimate); se_total = sqrt(W + (1 + 1/M)*B). Returns tibble(parameter, beta, se_total).
combine_pv_results <- function(df, param_name) {
  if (nrow(df) == 0) return(tibble(parameter = param_name, beta = NA_real_, se_total = NA_real_))
  m <- df %>% summarise(mean_est = mean(estimate, na.rm = TRUE), W = mean(std.error^2, na.rm = TRUE), B = var(estimate, na.rm = TRUE), .groups = "drop")
  m_pv <- n_distinct(df$PV)
  se <- sqrt(m$W + (1 + 1 / m_pv) * m$B)
  tibble(parameter = param_name, beta = m$mean_est, se_total = se)
}

# WHAT: Per-country combined results (beta, se_total per IDCNTRY). Subsets design by country, run_pv_regressions_design, combine_pv_results.
run_country_results_design <- function(design, indep_var, pv_vars) {
  if (!"IDCNTRY" %in% names(design$variables)) return(tibble())
  countries <- sort(unique(design$variables$IDCNTRY))
  bind_rows(lapply(countries, function(id) {
    dsub <- design[design$variables$IDCNTRY == id, ]
    if (nrow(dsub$variables) < 10) return(tibble(parameter = indep_var, beta = NA_real_, se_total = NA_real_, IDCNTRY = id))
    res <- run_pv_regressions_design(dsub, indep_var, pv_vars)
    out <- combine_pv_results(res, indep_var)
    out$IDCNTRY <- id
    out
  }))
}

# WHAT: One combined (beta, se_total) for Europe (Is_Balkan==0) and one for Balkans (Is_Balkan==1). Subsets design by Is_Balkan.
run_eu_balkan_contrast_design <- function(design, indep_var, pv_vars) {
  if (!"Is_Balkan" %in% names(design$variables)) return(tibble())
  bind_rows(
    lapply(list(Europe = 0, Balkan = 1), function(b) {
      dsub <- design[design$variables$Is_Balkan == b, ]
      if (nrow(dsub$variables) < 10) return(tibble(parameter = indep_var, beta = NA_real_, se_total = NA_real_))
      combine_pv_results(run_pv_regressions_design(dsub, indep_var, pv_vars), indep_var)
    }),
    .id = "group"
  )
}

# WHAT: Warn if binary indep (e.g. parentB_high_edu) has lower weighted mean outcome for value 1 than 0.
check_binary_direction <- function(data, indep_var, pv_var) {
  if (!(indep_var %in% c("parentB_high_edu", "ASDHEDUP_binary"))) return()
  if (!(indep_var %in% names(data)) || !(pv_var %in% names(data))) return()
  d <- data %>% filter(!is.na(.data[[indep_var]]), !is.na(.data[[pv_var]]), !is.na(TOTWGT))
  if (nrow(d) == 0) return()
  means <- d %>% group_by(.data[[indep_var]]) %>% summarise(m = weighted.mean(.data[[pv_var]], w = TOTWGT, na.rm = TRUE), .groups = "drop")
  if (nrow(means) == 2 && means$m[means[[1]] == 1] < means$m[means[[1]] == 0]) {
    message("Warning: ", indep_var, " has lower mean for value 1 in this slice.")
  }
}

# Format "beta (se)" with significance stars from z = beta/se. Used in HTML/display tables.
format_beta_se <- function(beta, se) {
  if (is.na(beta) || is.na(se)) return("")
  z <- beta / se
  p <- 2 * pnorm(-abs(z))
  stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
  sprintf("%.3f%s\n(%.3f)", beta, stars, se)
}

# World Bank Color Palette
wb_colors <- list(
  blue_dark = "#002244",
  blue_medium = "#0071BC",
  blue_light = "#009FDA",
  orange = "#F05023",
  yellow = "#FDB714",
  green = "#00AB51",
  red = "#EB1C2D",
  purple = "#872B90",
  gray_dark = "#414042",
  gray_medium = "#808285",
  gray_light = "#BCBEC0",
  gray_lighter = "#E6E7E8",
  balkans = "#F05023",
  europe = "#0071BC"
)

# WHAT: Write results to Excel (numeric) and HTML (formatted with stars). HOW: openxlsx/writexl for xlsx;
#       kableExtra for HTML table, save_kable to file.
save_summary_table <- function(df, index, title, base_path) {
  # 1) Save Excel with numeric-friendly structure (beta and sd in separate columns)
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(df, paste0(base_path, ".xlsx"), overwrite = TRUE)
  } else if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(df, paste0(base_path, ".xlsx"))
  }

  # 2) Build a display version: combine beta + sd into one cell per variable
  disp <- df
  if (ncol(df) > 1) {
    num_cols <- names(df)[-1]
    stat_pattern <- "_(beta|sd)$"
    has_stat <- grepl(stat_pattern, num_cols)
    if (any(has_stat)) {
      prefixes <- unique(sub(stat_pattern, "", num_cols[has_stat]))
      disp <- df[1]  # keep first column (country)
      for (pref in prefixes) {
        bcol <- paste0(pref, "_beta")
        scol <- paste0(pref, "_sd")
        if (bcol %in% names(df) && scol %in% names(df)) {
          disp[[pref]] <- mapply(format_beta_se, df[[bcol]], df[[scol]])
        } else if (bcol %in% names(df)) {
          vals <- df[[bcol]]
          disp[[pref]] <- ifelse(is.na(vals), "", sprintf("%.3f", vals))
        }}}}


  # 3) World Bank–styled HTML table for viewer (no pandoc, manual HTML wrapper)
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    d_html <- disp
    d_html[] <- lapply(d_html, function(x) {
      x <- as.character(x)
      # convert line breaks to <br> for HTML rendering
      gsub("\n", "<br>", x, fixed = TRUE)})

    n_cols <- ncol(d_html)
    align_vec <- c("l", rep("c", n_cols - 1))

    k <- kableExtra::kbl(
      d_html,
      caption = title,
      align = align_vec,
      escape = FALSE,
      format = "html"
    ) %>%
      kableExtra::kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "hover", "condensed"),
        font_size = 11,
        htmltable_class = "table"
      ) %>%
      kableExtra::row_spec(0, bold = TRUE, color = "white", background = wb_colors$blue_dark) %>%
      kableExtra::column_spec(1, bold = TRUE) %>%
      kableExtra::footnote(
        general = "Cells show beta coefficient with standard error in parentheses. Significance: *** p<0.01, ** p<0.05, * p<0.1.",
        general_title = "Note: ",
        footnote_as_chunk = TRUE
      )

    if (!is.null(index)) {
      k <- kableExtra::pack_rows(k, index = index)
    }

    # Save as a standalone HTML *file* in a reproducible way.
    # Use self_contained = FALSE to avoid requiring pandoc; this will create a small `lib/` folder.
    html_path <- paste0(base_path, ".html")
    kableExtra::save_kable(k, file = html_path, self_contained = FALSE)
    cat("Saved table HTML:", html_path, "\n")}}

# WHAT: For one year and one outcome (math/science/etc.), run regressions for each indep_var (SES, PI, etc.),
#       per country and Europe vs Balkans; return list(country=..., eu=...). HOW: get PV percentile vars,
#       loop indep_vars; use design if available else reg_fn; combine_pv_results; join country names.
# WHAT: For one year and one outcome, run regressions (design path only) per country and Europe vs Balkans.

run_one_year_outcome <- function(year_data, yr, outcome_name, outcome, method_label, cl, design) {
  if (is.null(design) || !inherits(design, "svyrep.design")) {
    stop("Survey design required. Build with add_rwgt_to_data(g4) then build_year_design(g4).")
  }
  pv_base <- get_pv_base(year_data, outcome$code)
  if (is.null(pv_base)) return(list(country = tibble(), eu = tibble()))

  pv_region <- sprintf("%s%02d_ptile_region", pv_base, 1:5)
  pv_country <- sprintf("%s%02d_ptile_country", pv_base, 1:5)
  if (!all(pv_region %in% names(year_data)) || (include_country_percentiles && !all(pv_country %in% names(year_data)))) {
    return(list(country = tibble(), eu = tibble()))
  }

  out_country <- list()
  out_eu <- list()

  for (indep_var in indep_vars) {
    if (!indep_var %in% names(year_data)) next

    check_binary_direction(year_data, indep_var, pv_region[1])
    reg_region <- run_country_results_design(design, indep_var, pv_region)
    reg_country <- if (include_country_percentiles) {
      run_country_results_design(design, indep_var, pv_country)
    } else tibble()

    combined <- if (include_country_percentiles) {
      full_join(
        reg_region %>% rename(beta_region = beta, se_region = se_total),
        reg_country %>% rename(beta_country = beta, se_country = se_total),
          by = "IDCNTRY"
        ) %>%
        mutate(parameter = dplyr::coalesce(parameter.x, parameter.y)) %>%
        left_join(cl, by = "IDCNTRY") %>%
        select(CountryName, parameter, beta_region, se_region, beta_country, se_country) %>%
        mutate(year = yr, outcome = outcome$label, method = method_label)
    } else {
      reg_region %>%
        rename(beta_region = beta, se_region = se_total) %>%
        mutate(beta_country = NA_real_, se_country = NA_real_) %>%
        left_join(cl, by = "IDCNTRY") %>%
        select(CountryName, parameter, beta_region, se_region, beta_country, se_country) %>%
        mutate(year = yr, outcome = outcome$label, method = method_label)
    }

    out_country[[length(out_country) + 1]] <- combined %>% mutate(indep_var = indep_var)

    contrast <- run_eu_balkan_contrast_design(design, indep_var, pv_region)
      if (nrow(contrast) > 0) {
      out_eu[[length(out_eu) + 1]] <- contrast %>%
        mutate(
          CountryName = ifelse(group == "Europe", "Europe", "Balkans"),
          indep_var = indep_var,
          method = method_label,
          outcome = outcome$label,
          year = yr
        ) %>%
        select(CountryName, parameter, beta, se_total, indep_var, method, outcome, year)
    }
  }

  list(
    country = bind_rows(out_country),
    eu = bind_rows(out_eu)
  )
}

# WHAT: Pivot country results to wide (one row per country, columns = indep_label_beta, indep_label_sd).
#       Returns list(df=wide, index=NULL, title=...).
build_country_table <- function(df, yr, outcome_label) {
  long <- df %>%
    mutate(indep_label = indep_labels[indep_var]) %>%
    select(CountryName, indep_label, beta_region, se_region)

  long_beta <- long %>%
    transmute(CountryName, indep_label, stat = "beta", value = beta_region)
  long_sd <- long %>%
    transmute(CountryName, indep_label, stat = "sd", value = se_region)

  long_both <- bind_rows(long_beta, long_sd)

  wide <- long_both %>%
    tidyr::pivot_wider(
      names_from = c(indep_label, stat),
      names_sep = "_",
      values_from = value
    ) %>%
    arrange(CountryName)

  list(df = wide, index = NULL, title = paste0(outcome_label, " (", yr, ")"))
}

# WHAT: Same as build_country_table but for Europe vs Balkans contrast (beta, se_total by group).
build_eu_table <- function(df, yr, outcome_label) {
  long <- df %>%
    mutate(indep_label = indep_labels[indep_var]) %>%
    select(CountryName, indep_label, beta, se_total)

  long_beta <- long %>%
    transmute(CountryName, indep_label, stat = "beta", value = beta)
  long_sd <- long %>%
    transmute(CountryName, indep_label, stat = "sd", value = se_total)

  long_both <- bind_rows(long_beta, long_sd)

  wide <- long_both %>%
    tidyr::pivot_wider(
      names_from = c(indep_label, stat),
      names_sep = "_",
      values_from = value
    ) %>%
    arrange(CountryName)

  list(df = wide, index = NULL, title = paste0("EU vs Balkans - ", outcome_label, " (", yr, ")"))
}

# --- Main --------------------------------------------------------------------
# Note: Run this script from the WB_TIMMS RProject root directory
run_wb_analysis <- function() {
  wd <- getwd()
  base_dir <- if (basename(wd) == "WB_TIMMS") "." else if (dir.exists("WB_TIMMS")) "WB_TIMMS" else "."

  master_dir <- file.path(base_dir, "data", "processed_data", "master")
  tables_dir <- file.path(base_dir, "output", "tables")
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

  cat("Starting WB_TIMMS analysis from base_dir =", base_dir, "\n")

  master_rdata_g4 <- file.path(master_dir, "master_table_grade_4.RData")
  if (!file.exists(master_rdata_g4)) {
    stop(
      "Missing master file: ", master_rdata_g4,
      "\nCurrent working directory: ", wd,
      "\nTip: run from the WB_TIMMS RProject root (WB_TIMMS/)."
    )
  }
  load(master_rdata_g4)
  if (!exists("master_table_grade_4")) stop("Object not found in master_table_grade_4.RData")
  master_table_grade_4 <- master_table_grade_4 %>% mutate(IDCNTRY = as.numeric(IDCNTRY))

  cat("Loaded master_table_grade_4 with", nrow(master_table_grade_4), "rows for years:",
      paste(sort(unique(master_table_grade_4$year)), collapse = ", "), "\n")

  if (!"year" %in% names(master_table_grade_4)) {
    stop("Master tables must include a 'year' column. Rebuild with BUILD_MASTER_TABLES.R.")
  }

  # Use CountryName from master table (built in BUILD_MASTER_TABLES.R)
  country_lookup <- master_table_grade_4 %>%
    distinct(IDCNTRY, .keep_all = TRUE) %>%
    select(IDCNTRY, CountryName, any_of(c("ISO3", "Is_Balkan")))

  for (yr in years) {
    g4 <- master_table_grade_4 %>% filter(year == yr)
    if (nrow(g4) == 0) next

    g4 <- get_latent_variable(g4)

    # Pre-build JK replicate weights and survey design once per year (required; no fallback).
    g4 <- add_rwgt_to_data(g4)
    design_yr <- build_year_design(g4)
    if (is.null(design_yr)) {
      stop("build_year_design returned NULL for year ", yr, ". Check TOTWGT, JKZONE, JKREP in master table. Run test_run_regression_jk_design() to debug.")
    }
    cat("Year", yr, ": using survey design for regressions.\n")

    # Distribution of parental investment latent factor (saved to output/figures/)
    plot_parental_investment_distribution(g4, yr, base_dir)

    # Save master table with all variables used in regressions (latent, PI_labels, PV percentiles, etc.)
    master_analysis_path <- file.path(base_dir, "output", "master_table_analysis", sprintf("master_table_analysis_%d.RData", yr))
    dir.create(dirname(master_analysis_path), showWarnings = FALSE, recursive = TRUE)
    save(g4, file = master_analysis_path)
    cat("Saved analysis master table:", master_analysis_path, "\n")

    for (outcome_name in names(outcomes)) {
      outcome <- outcomes[[outcome_name]]
      cat("Year", yr, "-", outcome$label, "\n")
      res_jk <- run_one_year_outcome(g4, yr, outcome_name, outcome, "jk", country_lookup, design_yr)

      country_all <- bind_rows(res_jk$country)
      eu_all <- bind_rows(res_jk$eu)

      if (nrow(country_all) > 0) {
        tbl <- build_country_table(country_all, yr, outcome$label)
        base_path <- file.path(tables_dir, sprintf("summary_country_%s_%d", outcome_name, yr))
        save_summary_table(tbl$df, tbl$index, tbl$title, base_path)}
      
      if (nrow(eu_all) > 0) {
        tbl_eu <- build_eu_table(eu_all, yr, outcome$label)
        base_path <- file.path(tables_dir, sprintf("summary_eu_balkan_%s_%d", outcome_name, yr))
        save_summary_table(tbl_eu$df, tbl_eu$index, tbl_eu$title, base_path)}}
    rm(g4)
    gc(verbose = FALSE)}}

  cat("WB_TIMMS analysis complete. Tables written to", tables_dir, "\n")

if (sys.nframe() == 0) {run_wb_analysis()}