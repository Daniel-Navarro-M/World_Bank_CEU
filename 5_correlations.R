# =============================================================================
# 5. Correlations – regression coefficients across countries
# (1) Edu binary -> Math vs Edu binary -> PI (from Beta_SE tables)
# (2) SES_binary vs PI_index on math (from reg_coefficients CSVs)
# Saves tables and plots as HTML, CSV, and PNG.
# =============================================================================

rm(list = ls())
library(dplyr)
source("R/world_bank_theme.R")
source("R/rubin_pv_helpers.R")

out_dir <- "output/tables"
plots_dir <- "output/figures"
dir.create(out_dir, showWarnings = FALSE)
dir.create(plots_dir, showWarnings = FALSE)

# ---- Parse "beta (se)" from Beta_SE column (e.g. "2.503 (0.275)" or "22.061*** (1.921)") ----
parse_beta_se <- function(x) {
  x <- as.character(x)
  beta_str <- sub("\\s*\\(.*", "", x)
  beta_str <- gsub("\\*", "", beta_str)
  beta <- as.numeric(trimws(beta_str))
  se_str <- sub(".*\\(([^)]+)\\).*", "\\1", x)
  se <- as.numeric(trimws(se_str))
  data.frame(beta = beta, se = se, stringsAsFactors = FALSE)
}

z_std <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

wtd_cor <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  if (length(x) < 3) return(NA_real_)
  cw <- stats::cov.wt(cbind(x, y), wt = w / sum(w), method = "ML")
  cw$cov[1, 2] / sqrt(cw$cov[1, 1] * cw$cov[2, 2])
}

# ---- Correlation 1: Edu binary -> Math vs Edu binary -> PI (all years) ----
# Load tables that have Beta_SE column (format "beta (se)")
coef_PI_2019 <- read.csv(file.path(out_dir, "reg_pi_index_parent_edu_binary_2019.csv"), stringsAsFactors = FALSE)
coef_PI_2023 <- read.csv(file.path(out_dir, "reg_pi_index_parent_edu_binary_2023.csv"), stringsAsFactors = FALSE)
coef_math_2019 <- read.csv(file.path(out_dir, "reg_math_parent_edu_binary_2019.csv"), stringsAsFactors = FALSE)
coef_math_2023 <- read.csv(file.path(out_dir, "reg_math_parent_edu_binary_2023.csv"), stringsAsFactors = FALSE)
has_2024 <- file.exists(file.path(out_dir, "reg_pi_index_parent_edu_binary_2024.csv")) &&
  file.exists(file.path(out_dir, "reg_math_parent_edu_binary_2024.csv"))
if (has_2024) {
  coef_PI_2024 <- read.csv(file.path(out_dir, "reg_pi_index_parent_edu_binary_2024.csv"), stringsAsFactors = FALSE)
  coef_math_2024 <- read.csv(file.path(out_dir, "reg_math_parent_edu_binary_2024.csv"), stringsAsFactors = FALSE)
}

# Parse Beta_SE into numeric beta (and se)
stopifnot("Beta_SE" %in% names(coef_PI_2019), "Beta_SE" %in% names(coef_math_2019))
coef_PI_2019 <- coef_PI_2019 %>% bind_cols(parse_beta_se(coef_PI_2019$Beta_SE)) %>% rename(beta_PI = beta, se_PI = se)
coef_PI_2023 <- coef_PI_2023 %>% bind_cols(parse_beta_se(coef_PI_2023$Beta_SE)) %>% rename(beta_PI = beta, se_PI = se)
coef_math_2019 <- coef_math_2019 %>% bind_cols(parse_beta_se(coef_math_2019$Beta_SE)) %>% rename(beta_math = beta, se_math = se)
coef_math_2023 <- coef_math_2023 %>% bind_cols(parse_beta_se(coef_math_2023$Beta_SE)) %>% rename(beta_math = beta, se_math = se)
if (has_2024) {
  coef_PI_2024 <- coef_PI_2024 %>% bind_cols(parse_beta_se(coef_PI_2024$Beta_SE)) %>% rename(beta_PI = beta, se_PI = se)
  coef_math_2024 <- coef_math_2024 %>% bind_cols(parse_beta_se(coef_math_2024$Beta_SE)) %>% rename(beta_math = beta, se_math = se)
}

# Country-level only (exclude Europe, Balkans)
countries_only <- function(d) d %>% filter(!.data$Group %in% c("Europe", "Balkans"))
coef_PI_2019 <- countries_only(coef_PI_2019)
coef_PI_2023 <- countries_only(coef_PI_2023)
coef_math_2019 <- countries_only(coef_math_2019)
coef_math_2023 <- countries_only(coef_math_2023)
if (has_2024) { coef_PI_2024 <- countries_only(coef_PI_2024); coef_math_2024 <- countries_only(coef_math_2024) }

# Merge Math and PI by Group for each year
edu_2019 <- coef_math_2019 %>% select(Group, beta_math, se_math) %>%
  left_join(coef_PI_2019 %>% select(Group, beta_PI, se_PI), by = "Group")
edu_2023 <- coef_math_2023 %>% select(Group, beta_math, se_math) %>%
  left_join(coef_PI_2023 %>% select(Group, beta_PI, se_PI), by = "Group")
if (has_2024) edu_2024 <- coef_math_2024 %>% select(Group, beta_math, se_math) %>%
  left_join(coef_PI_2024 %>% select(Group, beta_PI, se_PI), by = "Group")

# Correlation: gradient(edu -> math) vs gradient(edu -> PI)
edu_2019_clean <- edu_2019 %>% filter(!is.na(beta_math), !is.na(beta_PI))
edu_2023_clean <- edu_2023 %>% filter(!is.na(beta_math), !is.na(beta_PI))
r_edu_2019 <- cor(z_std(edu_2019_clean$beta_math), z_std(edu_2019_clean$beta_PI), use = "complete.obs")
r_edu_2023 <- cor(z_std(edu_2023_clean$beta_math), z_std(edu_2023_clean$beta_PI), use = "complete.obs")
test_edu_2019 <- cor.test(edu_2019_clean$beta_math, edu_2019_clean$beta_PI)
test_edu_2023 <- cor.test(edu_2023_clean$beta_math, edu_2023_clean$beta_PI)
cor_edu_tbl <- data.frame(
  Year = c(2019, 2023),
  r = c(r_edu_2019, r_edu_2023),
  p_value = c(test_edu_2019$p.value, test_edu_2023$p.value),
  n_countries = c(nrow(edu_2019_clean), nrow(edu_2023_clean))
)
if (has_2024) {
  edu_2024_clean <- edu_2024 %>% filter(!is.na(beta_math), !is.na(beta_PI))
  r_edu_2024 <- cor(z_std(edu_2024_clean$beta_math), z_std(edu_2024_clean$beta_PI), use = "complete.obs")
  test_edu_2024 <- cor.test(edu_2024_clean$beta_math, edu_2024_clean$beta_PI)
  cor_edu_tbl <- rbind(cor_edu_tbl, data.frame(Year = 2024, r = r_edu_2024, p_value = test_edu_2024$p.value, n_countries = nrow(edu_2024_clean)))
}

write.csv(cor_edu_tbl, file.path(out_dir, "cor_edu_math_vs_edu_PI.csv"), row.names = FALSE)

print(cor_edu_tbl)

# World Bank palette and scatter_theme from R/world_bank_theme.R (already sourced)
sig_star_cor <- function(p) if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else ""

# Plot: coefficient (parent_edu_binary -> Math) vs coefficient (parent_edu_binary -> PI)
make_cor_edu_plot <- function(corr_df, r_val, p_val, n_val, yr, outfile) {
  # Same x/y limits across 2019, 2023, 2024 for comparability
  p <- ggplot2::ggplot(corr_df, ggplot2::aes(x = beta_math, y = beta_PI)) +
    ggplot2::geom_point(size = 4, alpha = 0.7, color = wb_colors$blue_medium) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = wb_colors$purple, fill = wb_colors$purple, alpha = 0.2, linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    ggplot2::coord_cartesian(xlim = c(8, 35), ylim = c(-0.4, 5), expand = FALSE, clip = "off") +
    ggplot2::labs(
      title    = paste0("Correlation: Gradient of Parent Edu to Math vs Gradient of Parent Edu to PI (", yr, ")"),
      subtitle = sprintf("x = Math ~ parent_edu_binary; y = PI_index ~ parent_edu_binary | R = %.3f%s | p = %.4f | n = %d",
                         r_val, sig_star_cor(p_val), p_val, n_val),
      x        = "Gradient: Math ~ parent education (binary)",
      y        = "Gradient: PI index ~ parent education (binary)",
      caption  = "Each point is a country. Fixed axes: x 8–35, y −0.4–4.5 (all years). Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1"
    ) +
    scatter_theme
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = Group), size = 3.5, fontface = "bold", max.overlaps = 20)
  } else {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = Group), hjust = -0.1, vjust = 0.5, size = 3)
  }
  ggplot2::ggsave(outfile, p, width = 12, height = 9, dpi = 300, bg = "white")
  invisible(p)
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  make_cor_edu_plot(edu_2019_clean, r_edu_2019, test_edu_2019$p.value, nrow(edu_2019_clean), 2019,
                   file.path(plots_dir, "cor_edu_math_vs_edu_PI_2019.png"))
  make_cor_edu_plot(edu_2023_clean, r_edu_2023, test_edu_2023$p.value, nrow(edu_2023_clean), 2023,
                   file.path(plots_dir, "cor_edu_math_vs_edu_PI_2023.png"))
  if (has_2024 && exists("edu_2024_clean") && nrow(edu_2024_clean) >= 3)
    make_cor_edu_plot(edu_2024_clean, r_edu_2024, test_edu_2024$p.value, nrow(edu_2024_clean), 2024,
                     file.path(plots_dir, "cor_edu_math_vs_edu_PI_2024.png"))
  message("Saved cor_edu_math_vs_edu_PI 2019, 2023", if (has_2024) ", 2024" else "", " plots")
}


###################################
## PI vs Math correlations (student-level, weighted)
## PI_index, PI_math vs Math percentile; same vs raw math (mean of 5 PVs)
###################################
load("data/processed_data/master/master_processed.RData")
dat_pi <- master_processed %>% filter(CountryName != "Netherlands", !is.na(TOTWGT) & TOTWGT > 0)

pv_math <- sprintf("ASMMAT%02d", 1:5)
pv_ptile <- paste0("ASMMAT", sprintf("%02d", 1:5), "_ptile_region")
has_pv   <- all(pv_math %in% names(dat_pi))
has_ptile <- all(pv_ptile %in% names(dat_pi))

if (has_pv) dat_pi$math_raw <- rowMeans(dat_pi[, pv_math], na.rm = TRUE)
if (has_ptile) dat_pi$math_ptile <- rowMeans(dat_pi[, pv_ptile], na.rm = TRUE)

dat_pi <- dat_pi %>%
  filter(!(PI_index %in% c(998, 999)), !(PI_math %in% c(998, 999))) %>%
  mutate(PI_index = as.numeric(PI_index), PI_math = as.numeric(PI_math))

# Rubin-style combination of correlations over 5 PVs
run_pi_math_cor <- function(df, x_var, pv_vars, yr = NULL) {
  # Rubin-style pooling across PVs:
  # We have M=5 plausible values (PVs). For each PV j we:
  #   1) compute a weighted correlation r_j
  #   2) approximate an SE for r_j from its sample size n_j
  #      using a Fisher-z approximation:
  #        z = atanh(r),  SE(z) = 1/sqrt(n - 3)
  #        and delta-method gives SE(r) ≈ (1 - r^2) * SE(z)
  #   3) pool the {r_j} across PVs with Rubin's rules:
  #        q_bar = mean(r_j)
  #        W = mean(SE(r_j)^2)        (within-PV uncertainty)
  #        B = var(r_j)              (between-PV variability)
  #        T = W + (1 + 1/M) * B
  #        pooled SE = sqrt(T)
  #      Then we compute a p-value using z = pooled_r / pooled_SE.
  d <- if (!is.null(yr)) df %>% filter(year == yr) else df
  m <- length(pv_vars)
  r_j <- se_j <- n_j <- rep(NA_real_, m)
  for (j in seq_len(m)) {
    ok <- complete.cases(d[[x_var]], d[[pv_vars[j]]], d$TOTWGT) & d$TOTWGT > 0
    x <- z_std(d[[x_var]][ok]); y <- z_std(d[[pv_vars[j]]][ok]); w <- d$TOTWGT[ok]
    ok2 <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
    n_j[j] <- sum(ok2)
    if (n_j[j] > 3) {
      r_j[j] <- wtd_cor(x[ok2], y[ok2], w[ok2])
      # Fisher-z delta-method SE for Pearson's r:
      #   SE(r) ≈ (1 - r^2) / sqrt(n - 3)
      se_j[j] <- sqrt((1 - r_j[j]^2)^2 / (n_j[j] - 3))
    }
  }
  ok <- is.finite(r_j) & is.finite(se_j)
  if (!any(ok)) return(list(r = NA_real_, p = NA_real_, n = max(n_j, na.rm = TRUE)))
  cmb <- rubin_combine(r_j[ok], se_j[ok])  # uses effective M = length(est)
  z <- cmb$beta / cmb$se
  p_val <- 2 * pnorm(-abs(z))
  list(r = cmb$beta, p = p_val, n = max(n_j[ok], na.rm = TRUE))
}

cat("\n--- PI vs Math correlations (weighted, student-level) ---\n")
years_pi <- c(2019, 2023)
if (any(dat_pi$year == 2024, na.rm = TRUE)) years_pi <- c(years_pi, 2024)

cor_rows <- list()
if (has_ptile) {
  for (yr in years_pi) {
    r1 <- run_pi_math_cor(dat_pi, "PI_index", pv_ptile, yr)
    r2 <- run_pi_math_cor(dat_pi, "PI_math", pv_ptile, yr)
    cat(sprintf("  %d: PI_index vs Math percentile:  r = %.3f  p = %.3g  n = %d\n", yr, r1$r, r1$p, r1$n))
    cat(sprintf("  %d: PI_math vs Math percentile:   r = %.3f  p = %.3g  n = %d\n", yr, r2$r, r2$p, r2$n))
    cor_rows[[length(cor_rows) + 1]] <- data.frame(year = yr, x_var = "PI_index", y_var = "math_ptile_region", r = r1$r, p = r1$p, n = r1$n, stringsAsFactors = FALSE)
    cor_rows[[length(cor_rows) + 1]] <- data.frame(year = yr, x_var = "PI_math",  y_var = "math_ptile_region", r = r2$r, p = r2$p, n = r2$n, stringsAsFactors = FALSE)
  }
}
if (has_pv) {
  for (yr in years_pi) {
    r1 <- run_pi_math_cor(dat_pi, "PI_index", pv_math, yr)
    r2 <- run_pi_math_cor(dat_pi, "PI_math", pv_math, yr)
    cor_rows[[length(cor_rows) + 1]] <- data.frame(year = yr, x_var = "PI_index", y_var = "math_raw_pv_mean", r = r1$r, p = r1$p, n = r1$n, stringsAsFactors = FALSE)
    cor_rows[[length(cor_rows) + 1]] <- data.frame(year = yr, x_var = "PI_math",  y_var = "math_raw_pv_mean", r = r2$r, p = r2$p, n = r2$n, stringsAsFactors = FALSE)
  }
}

cor_pi_math_tbl <- bind_rows(cor_rows)
write.csv(cor_pi_math_tbl, file.path(out_dir, "cor_PI_vs_Math_student_level.csv"), row.names = FALSE)
print(cor_pi_math_tbl)

cat("\n========== Done ==========\n")

###################################
## Extra PI correlation tests (longitudinal 2023/2024)
###################################
pi_corr_dir <- "output/PI_correlation_test"
dir.create(pi_corr_dir, showWarnings = FALSE, recursive = TRUE)

load("data/processed_data/master/master_longitudinal_processed.RData")
dlong <- master_longitudinal_processed %>%
  filter(year %in% c(2023, 2024), !is.na(TOTWGT), TOTWGT > 0) %>%
  mutate(IDCNTRY = as.numeric(haven::zap_labels(IDCNTRY)))

# Recode special missing for analysis
to_na_998_999 <- function(x) { x <- as.numeric(x); x[x %in% c(998, 999)] <- NA_real_; x }
dlong$children_books_count <- to_na_998_999(dlong$children_books_count)  # children books at home
dlong$PI_read <- to_na_998_999(dlong$PI_read)
dlong$PI_math <- to_na_998_999(dlong$PI_math)
dlong$PI_index <- to_na_998_999(dlong$PI_index)

fmt_rp <- function(r, p) ifelse(is.na(r), NA_character_, sprintf("%.3f (p=%s)", r, formatC(p, format = "e", digits = 2)))

# ------------------------------
# HOME_RESOURCES
# ------------------------------
home_resource_vars <- c("home_books_count","children_books_count","resources_computer","resources_tablet","resources_internet")
home_resource_vars <- home_resource_vars[home_resource_vars %in% names(dlong)]
for (v in home_resource_vars) dlong[[v]] <- to_na_998_999(dlong[[v]])

home_cor_rows <- list()
for (yy in c(2023, 2024)) {
  d_y <- dlong %>% filter(year == yy)
  for (v in home_resource_vars) {
    ok <- complete.cases(d_y[[v]], d_y$PI_index, d_y$TOTWGT) & d_y$TOTWGT > 0
    x <- z_std(d_y[[v]][ok]); y <- z_std(d_y$PI_index[ok]); w <- d_y$TOTWGT[ok]
    ok2 <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
    n <- sum(ok2); r <- p <- NA_real_
    if (n > 30) {
      r <- wtd_cor(x[ok2], y[ok2], w[ok2])
      t_stat <- r * sqrt((n - 2) / max(1e-12, 1 - r^2))
      p <- 2 * stats::pt(-abs(t_stat), n - 2)
    }
    home_cor_rows[[length(home_cor_rows) + 1]] <- data.frame(year = yy, variable = v, r = r, p = p, n = n, stringsAsFactors = FALSE)
  }
}
tbl_cor_home_resources <- bind_rows(home_cor_rows)
cat("\nPI_index vs HOME_RESOURCES (scaled)\n")
print(tbl_cor_home_resources)
write.csv(tbl_cor_home_resources, file.path(pi_corr_dir, "COR_HOME_RESOURCES.csv"), row.names = FALSE)

home_reg_rows <- list()
for (yy in c(2023, 2024)) {
  d_y <- dlong %>% filter(year == yy)
  for (v in home_resource_vars) {
    d_fit <- d_y %>% mutate(PI_index_z = z_std(PI_index), v_raw = .data[[v]], v_z = z_std(.data[[v]])) %>% filter(!is.na(PI_index), !is.na(v_raw), !is.na(IDCNTRY), !is.na(TOTWGT), TOTWGT > 0)
    if (nrow(d_fit) < 30) next
    m_raw <- fixest::feols(PI_index ~ v_raw | IDCNTRY, data = d_fit, weights = ~TOTWGT)
    cf_raw <- as.data.frame(summary(m_raw)$coeftable)
    home_reg_rows[[length(home_reg_rows) + 1]] <- data.frame(year = yy, variable = v, model = "raw", beta = if ("v_raw" %in% rownames(cf_raw)) cf_raw["v_raw", "Estimate"] else NA_real_, se = if ("v_raw" %in% rownames(cf_raw)) cf_raw["v_raw", "Std. Error"] else NA_real_, p = if ("v_raw" %in% rownames(cf_raw)) cf_raw["v_raw", "Pr(>|t|)"] else NA_real_, n = nobs(m_raw), stringsAsFactors = FALSE)
    d_fit_z <- d_fit %>% filter(!is.na(PI_index_z), !is.na(v_z))
    if (nrow(d_fit_z) >= 30) {
      m_z <- fixest::feols(PI_index_z ~ v_z | IDCNTRY, data = d_fit_z, weights = ~TOTWGT)
      cf_z <- as.data.frame(summary(m_z)$coeftable)
      home_reg_rows[[length(home_reg_rows) + 1]] <- data.frame(year = yy, variable = v, model = "scaled_z", beta = if ("v_z" %in% rownames(cf_z)) cf_z["v_z", "Estimate"] else NA_real_, se = if ("v_z" %in% rownames(cf_z)) cf_z["v_z", "Std. Error"] else NA_real_, p = if ("v_z" %in% rownames(cf_z)) cf_z["v_z", "Pr(>|t|)"] else NA_real_, n = nobs(m_z), stringsAsFactors = FALSE)
    }
  }
}
tbl_reg_home_resources <- bind_rows(home_reg_rows)
cat("\nPI_index vs HOME_RESOURCES (country FE regressions)\n")
print(tbl_reg_home_resources)
write.csv(tbl_reg_home_resources, file.path(pi_corr_dir, "COR_HOME_RESOURCES_regressions.csv"), row.names = FALSE)

# ==========================================
# ## ZOOM IN PARENT AGREES ASBLH Correlation ##
# ==========================================
agree_vars <- c("gen_agree_included_rev","gen_agree_safe_env_rev","gen_agree_cares_progress_rev","gen_agree_keeps_informed_rev","gen_agree_promotes_standards_rev","gen_agree_helps_reading_rev","gen_agree_helps_math_rev","gen_agree_helps_science_rev")
agree_vars <- agree_vars[agree_vars %in% names(dlong)]
for (v in agree_vars) dlong[[v]] <- to_na_998_999(dlong[[v]])
if (length(agree_vars) > 0) {
  agree_mat <- as.matrix(dlong[, agree_vars, drop = FALSE])
  dlong$PI_agree_index <- rowSums(agree_mat, na.rm = TRUE)
  dlong$PI_agree_index[rowSums(!is.na(agree_mat)) == 0] <- NA_real_
}

agree_cor_rows <- list()
for (yy in c(2023, 2024)) {
  d_y <- dlong %>% filter(year == yy)
  for (v in c(agree_vars, "PI_agree_index")) {
    if (!v %in% names(d_y)) next
    ok <- complete.cases(d_y[[v]], d_y$PI_index, d_y$TOTWGT) & d_y$TOTWGT > 0
    x <- z_std(d_y[[v]][ok]); y <- z_std(d_y$PI_index[ok]); w <- d_y$TOTWGT[ok]
    ok2 <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
    n <- sum(ok2); r <- p <- NA_real_
    if (n > 30) {
      r <- wtd_cor(x[ok2], y[ok2], w[ok2])
      t_stat <- r * sqrt((n - 2) / max(1e-12, 1 - r^2))
      p <- 2 * stats::pt(-abs(t_stat), n - 2)
    }
    agree_cor_rows[[length(agree_cor_rows) + 1]] <- data.frame(year = yy, variable = v, r = r, p = p, n = n, stringsAsFactors = FALSE)
  }
}
tbl_cor_pi_agree <- bind_rows(agree_cor_rows)
cat("\nPI_index vs GEN AGREE (scaled)\n")
print(tbl_cor_pi_agree)
write.csv(tbl_cor_pi_agree, file.path(pi_corr_dir, "COR_GEN_AGREES.csv"), row.names = FALSE)

agree_reg_rows <- list()
for (yy in c(2023, 2024)) {
  d_y <- dlong %>% filter(year == yy)
  for (v in c(agree_vars, "PI_agree_index")) {
    if (!v %in% names(d_y)) next
    d_fit <- d_y %>% mutate(PI_index_z = z_std(PI_index), v_raw = .data[[v]], v_z = z_std(.data[[v]])) %>% filter(!is.na(PI_index), !is.na(v_raw), !is.na(IDCNTRY), !is.na(TOTWGT), TOTWGT > 0)
    if (nrow(d_fit) < 30) next
    m_raw <- fixest::feols(PI_index ~ v_raw | IDCNTRY, data = d_fit, weights = ~TOTWGT)
    cf_raw <- as.data.frame(summary(m_raw)$coeftable)
    agree_reg_rows[[length(agree_reg_rows) + 1]] <- data.frame(year = yy, variable = v, model = "raw", beta = if ("v_raw" %in% rownames(cf_raw)) cf_raw["v_raw", "Estimate"] else NA_real_, se = if ("v_raw" %in% rownames(cf_raw)) cf_raw["v_raw", "Std. Error"] else NA_real_, p = if ("v_raw" %in% rownames(cf_raw)) cf_raw["v_raw", "Pr(>|t|)"] else NA_real_, n = nobs(m_raw), stringsAsFactors = FALSE)
    d_fit_z <- d_fit %>% filter(!is.na(PI_index_z), !is.na(v_z))
    if (nrow(d_fit_z) >= 30) {
      m_z <- fixest::feols(PI_index_z ~ v_z | IDCNTRY, data = d_fit_z, weights = ~TOTWGT)
      cf_z <- as.data.frame(summary(m_z)$coeftable)
      agree_reg_rows[[length(agree_reg_rows) + 1]] <- data.frame(year = yy, variable = v, model = "scaled_z", beta = if ("v_z" %in% rownames(cf_z)) cf_z["v_z", "Estimate"] else NA_real_, se = if ("v_z" %in% rownames(cf_z)) cf_z["v_z", "Std. Error"] else NA_real_, p = if ("v_z" %in% rownames(cf_z)) cf_z["v_z", "Pr(>|t|)"] else NA_real_, n = nobs(m_z), stringsAsFactors = FALSE)
    }
  }
}
tbl_reg_pi_agree <- bind_rows(agree_reg_rows)
cat("\nPI_index vs GEN AGREE (country FE regressions)\n")
print(tbl_reg_pi_agree)
write.csv(tbl_reg_pi_agree, file.path(pi_corr_dir, "COR_GEN_AGREES_regressions.csv"), row.names = FALSE)

message("Saved PI correlation test tables to ", pi_corr_dir)

