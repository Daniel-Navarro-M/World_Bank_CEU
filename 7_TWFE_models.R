load("data/processed_data/master/master_longitudinal_processed.RData")
library(fixest)
library(modelsummary)
library(gt)
library(webshot2)
library(tidyverse)
library(dplyr)
# Increase browser timeout for gt/webshot rendering.
options(chromote.timeout = 240000)
source("R/rubin_pv_helpers.R")
#### 0. Load & Clean Data ####

master <- master_longitudinal_processed |>
  mutate(
    IDCNTRY = as.numeric(haven::zap_labels(IDCNTRY)),
    year = as.numeric(haven::zap_labels(year)),
    IDSTUD = as.character(IDSTUD),
    TOTWGT = as.numeric(haven::zap_labels(TOTWGT))
  )
haven::as_factor(master$IDCNTRY)
attributes(master$IDCNTRY)
# Check data
unique(master$CountryName)
names(master)
master |>
  filter(IDCNTRY == 705) |>
  count(year)

# Select just panel data — keep only students observed in both waves
df_panel <- master |>
  filter(year %in% c(2023, 2024)) |>
  group_by(IDSTUD, IDCNTRY) |>
  filter(n_distinct(year) == 2) |>
  arrange(year) |>
  mutate(
    student_id        = paste(IDCNTRY, IDSTUD, sep = "_"),
    parent_edu_binary = ifelse(parent_edu_binary %in% c(9, 99, 998, 999), NA, parent_edu_binary),
    PI_index          = ifelse(PI_index == 999, NA, PI_index),
    PI_binary         = ifelse(PI_binary %in% c(9, 99, 998, 999), NA, PI_binary),
    PI_read           = ifelse(PI_read%in% c(9, 99, 998, 999), NA, PI_read),
    PI_math           = ifelse(PI_math%in% c(9, 99, 998, 999), NA, PI_math),
    low_edu           = 1 - parent_edu_binary,
    country_year      = paste(IDCNTRY, year, sep = "_")
  ) |>
  mutate(
    parent_edu_binary = ifelse(
      is.na(parent_edu_binary) & year == 2024,
      parent_edu_binary[year == 2023][1],
      parent_edu_binary
    ),
    low_edu = 1 - parent_edu_binary   # recalculate after fill
  ) |>
  ungroup()

# ── Sanity checks ─────────────────────────────────────────────
n_distinct(df_panel$student_id)
nrow(df_panel)
table(df_panel$year)

# Parental Investment is held constant from 2023
# Parent Edu should have very low levels of change
df_panel |>
  group_by(student_id) |>
  summarise(
    edu_varies = n_distinct(parent_edu_binary, na.rm = TRUE) > 1,
    PI_varies  = n_distinct(PI_index, na.rm = TRUE) > 1
  ) |>
  summarise(
    students_edu_varies = sum(edu_varies),
    students_PI_varies  = sum(PI_varies)
  )

# Checks for missing outcome variables
df_panel |>
  group_by(year) |>
  summarise(
    across(ASMMAT01:ASMMAT05, list(missing = ~sum(is.na(.)), mean = ~mean(., na.rm = TRUE))),
    n = n()
  ) |>
  print(width = Inf)

# ── Subsamples ────────────────────────────────────────────────
# Balkans  = Kosovo, Montenegro, North Macedonia
# European = Italy, Sweden, Slovenia
# ── Subsamples ────────────────────────────────────────────────
# Balkans  = Kosovo (411), Montenegro (499), North Macedonia (807)
# European = Italy (380), Slovenia (705), Sweden (752)
df_panel <- df_panel |>
  mutate(
    Is_balkan = if_else(IDCNTRY %in% c(411, 499, 807), 1, 0)
  )

# verify
df_panel |> count(CountryName, Is_balkan)

df_balkan   <- df_panel |> filter(Is_balkan == 1)
df_european <- df_panel |> filter(Is_balkan == 0)

# Check country counts — report in paper
cat("European countries:", n_distinct(df_european$IDCNTRY), "\n")
cat("Balkan countries:",   n_distinct(df_balkan$IDCNTRY),   "\n")
unique(df_panel$CountryName)

# ── Rebuild subsamples ──────────────────────────────────────────
df_balkan   <- df_panel |> filter(Is_balkan == 1)
df_european <- df_panel |> filter(Is_balkan == 0)

# ── Verify both fixes ───────────────────────────────────────────
df_panel |>
  group_by(year) |>
  summarise(
    n                      = n(),
    home_resources_valid   = sum(!is.na(home_resources_learning_rev)),
    home_resources_mean    = round(mean(home_resources_learning_rev, na.rm = TRUE), 3),
    teacher_pi_valid       = sum(!is.na(teacher_parental_involvement_rev)),
    teacher_pi_mean        = round(mean(teacher_parental_involvement_rev, na.rm = TRUE), 3)
  ) |>
  print(width = Inf)


# ── Shared setup ──────────────────────────────────────────────
pv_vars <- paste0("ASMMAT0", 1:5)

# Always run all student FE models and all tables in one pass.
RUN_STUD_MODELS <- TRUE

fit_pv_model <- function(rhs, dataset) {
  mods <- lapply(pv_vars, function(pv) lm(as.formula(paste(pv, "~", rhs)), data = dataset, weights = dataset$TOTWGT))
  terms_all <- unique(unlist(lapply(mods, function(m) names(coef(m)))))
  est <- do.call(cbind, lapply(mods, function(m) {x <- rep(NA_real_, length(terms_all)); names(x) <- terms_all; x[names(coef(m))] <- coef(m); x}))
  ses <- do.call(cbind, lapply(mods, function(m) {cf <- summary(m)$coefficients; x <- rep(NA_real_, length(terms_all)); names(x) <- terms_all; x[rownames(cf)] <- cf[, "Std. Error"]; x}))
  beta <- rowMeans(est, na.rm = TRUE); se <- rowMeans(ses, na.rm = TRUE)
  structure(list(
    tidy = data.frame(term = terms_all, estimate = beta, std.error = se, statistic = beta / se, p.value = 2 * stats::pnorm(-abs(beta / se)), row.names = NULL),
    nobs = mean(sapply(mods, nobs), na.rm = TRUE),
    r.squared = mean(sapply(mods, function(m) summary(m)$r.squared), na.rm = TRUE),
    adj.r.squared = mean(sapply(mods, function(m) summary(m)$adj.r.squared), na.rm = TRUE)
  ), class = "rubin_lm")
}

# Student FE models are estimated inline using a two-step setup:
# - run feols per country and plausible value with IDSTUD + year FE
# - average coefficients across plausible values within each country
# - average country-level coefficients across countries in each region
# - save one model object per region

tidy.rubin_lm <- function(x, ...) x$tidy
glance.rubin_lm <- function(x, ...) data.frame(nobs = x$nobs, r.squared = x$r.squared, adj.r.squared = x$adj.r.squared)
nobs.rubin_lm <- function(object, ...) object$nobs


#### 1. Panel Models without Scale Correction ####
# Students FE commented out 

#####MODEL 1 PI Binary Alone ####
m1_panel_pv_ctry    <- fit_pv_model("PI_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m1_panel_pv_schl    <- fit_pv_model("PI_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m1_balkan_pv_ctry   <- fit_pv_model("PI_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m1_balkan_pv_schl   <- fit_pv_model("PI_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m1_european_pv_ctry <- fit_pv_model("PI_binary + as.factor(IDCNTRY) + as.factor(year)", df_european)
m1_european_pv_schl <- fit_pv_model("PI_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european)

#####MODEL 2 PI + Parental Education ####
m2_panel_pv_ctry    <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m2_panel_pv_schl    <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m2_balkan_pv_ctry   <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m2_balkan_pv_schl   <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m2_european_pv_ctry <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european)
m2_european_pv_schl <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european)

#####MODEL 3 PI x Parental Education ####
m3_panel_pv_ctry    <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m3_panel_pv_schl    <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m3_balkan_pv_ctry   <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m3_balkan_pv_schl   <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m3_european_pv_ctry <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european)
m3_european_pv_schl <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european)

#####MODEL 4 PI x Parental Education x Is_Balkan ####
m4_panel_pv_ctry    <- fit_pv_model("PI_binary * parent_edu_binary * Is_balkan + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m4_panel_pv_schl    <- fit_pv_model("PI_binary * parent_edu_binary * Is_balkan + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

#####MODEL 5 Parental Education + PI Read + PI Math #####
m5_panel_pv_ctry    <- fit_pv_model("parent_edu_binary + PI_read + PI_math + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m5_panel_pv_schl    <- fit_pv_model("parent_edu_binary + PI_read + PI_math + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m5_balkan_pv_ctry   <- fit_pv_model("parent_edu_binary + PI_read + PI_math + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m5_balkan_pv_schl   <- fit_pv_model("parent_edu_binary + PI_read + PI_math + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m5_european_pv_ctry <- fit_pv_model("parent_edu_binary + PI_read + PI_math + as.factor(IDCNTRY) + as.factor(year)", df_european)
m5_european_pv_schl <- fit_pv_model("parent_edu_binary + PI_read + PI_math + as.factor(IDSCHOOL) + as.factor(year)", df_european)

#####MODEL 6 Parental Education x PI_Read + Parental Education x PI_Math ####
m6_panel_pv_ctry    <- fit_pv_model("parent_edu_binary * PI_read + parent_edu_binary * PI_math + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m6_panel_pv_schl    <- fit_pv_model("parent_edu_binary * PI_read + parent_edu_binary * PI_math + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m6_balkan_pv_ctry   <- fit_pv_model("parent_edu_binary * PI_read + parent_edu_binary * PI_math + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m6_balkan_pv_schl   <- fit_pv_model("parent_edu_binary * PI_read + parent_edu_binary * PI_math + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m6_european_pv_ctry <- fit_pv_model("parent_edu_binary * PI_read + parent_edu_binary * PI_math + as.factor(IDCNTRY) + as.factor(year)", df_european)
m6_european_pv_schl <- fit_pv_model("parent_edu_binary * PI_read + parent_edu_binary * PI_math + as.factor(IDSCHOOL) + as.factor(year)", df_european)

#####MODEL 7 PI x Low Education ####
m7_panel_pv_ctry    <- fit_pv_model("PI_binary * low_edu + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m7_panel_pv_schl    <- fit_pv_model("PI_binary * low_edu + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m7_balkan_pv_ctry   <- fit_pv_model("PI_binary * low_edu + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m7_balkan_pv_schl   <- fit_pv_model("PI_binary * low_edu + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m7_european_pv_ctry <- fit_pv_model("PI_binary * low_edu + as.factor(IDCNTRY) + as.factor(year)", df_european)
m7_european_pv_schl <- fit_pv_model("PI_binary * low_edu + as.factor(IDSCHOOL) + as.factor(year)", df_european)


##### 1.1 NEW MODEL SETUP ####
df_panel <- df_panel |>
  mutate(
    env_discuss = ifelse(env_discuss_rev %in% c(998, 999), NA, env_discuss_rev),
    books_score           = ifelse(books_score %in% c(998, 999), NA, books_score),
    children_books_score  = ifelse(children_books_score %in% c(998, 999), NA, children_books_score),
    home_resources_learning_rev = ifelse(
      home_resources_learning_rev %in% c(998, 999), NA, home_resources_learning_rev
    ),
    teacher_parental_involvement_rev = ifelse(
      teacher_parental_involvement_rev %in% c(9, 99, 998, 999), NA, teacher_parental_involvement_rev
    )
  )
# Rebuild subsamples to pick up cleaned variables
df_balkan   <- df_panel |> filter(Is_balkan == 1)
df_european <- df_panel |> filter(Is_balkan == 0)

# Sanity check: confirm env_discuss has variation in both waves
df_panel |>
  group_by(year) |>
  summarise(
    n         = n(),
    n_missing = sum(is.na(env_discuss)),
    mean      = mean(env_discuss, na.rm = TRUE),
    sd        = sd(env_discuss,   na.rm = TRUE)
  )

# Sanity check: confirm books_score is not all-NA in 2024
df_panel |>
  group_by(year) |>
  summarise(
    books_missing          = sum(is.na(books_score)),
    children_books_missing = sum(is.na(children_books_score))
  )


#####MODEL 8 Books at home + Parental Education ####
m8_panel_pv_ctry    <- fit_pv_model("books_score + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m8_panel_pv_schl    <- fit_pv_model("books_score + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m8_balkan_pv_ctry   <- fit_pv_model("books_score + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m8_balkan_pv_schl   <- fit_pv_model("books_score + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m8_european_pv_ctry <- fit_pv_model("books_score + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european)
m8_european_pv_schl <- fit_pv_model("books_score + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european)

#####MODEL 9 Childrens Books + Parental Education ####
m9_panel_pv_ctry    <- fit_pv_model("children_books_score + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m9_panel_pv_schl    <- fit_pv_model("children_books_score + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m9_balkan_pv_ctry   <- fit_pv_model("children_books_score + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m9_balkan_pv_schl   <- fit_pv_model("children_books_score + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m9_european_pv_ctry <- fit_pv_model("children_books_score + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european)
m9_european_pv_schl <- fit_pv_model("children_books_score + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european)

#####MODEL 10 Home Resources + Parental Education ####

m10_panel_pv_ctry    <- fit_pv_model("home_resources_learning_rev + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m10_panel_pv_schl    <- fit_pv_model("home_resources_learning_rev + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m10_balkan_pv_ctry   <- fit_pv_model("home_resources_learning_rev + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m10_balkan_pv_schl   <- fit_pv_model("home_resources_learning_rev + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m10_european_pv_ctry <- fit_pv_model("home_resources_learning_rev + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european)
m10_european_pv_schl <- fit_pv_model("home_resources_learning_rev + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european)

#####MODEL 11 Discuss Environment + Parental Education ####
m11_panel_pv_ctry    <- fit_pv_model("env_discuss + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m11_panel_pv_schl    <- fit_pv_model("env_discuss + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m11_balkan_pv_ctry   <- fit_pv_model("env_discuss + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m11_balkan_pv_schl   <- fit_pv_model("env_discuss + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m11_european_pv_ctry <- fit_pv_model("env_discuss + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european)
m11_european_pv_schl <- fit_pv_model("env_discuss + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european)


#####MODEL 12 Teacher Perception PI + Parental Education ####
m12_panel_pv_ctry    <- fit_pv_model("teacher_parental_involvement_rev + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m12_panel_pv_schl    <- fit_pv_model("teacher_parental_involvement_rev + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m12_balkan_pv_ctry   <- fit_pv_model("teacher_parental_involvement_rev + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan)
m12_balkan_pv_schl   <- fit_pv_model("teacher_parental_involvement_rev + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan)

m12_european_pv_ctry <- fit_pv_model("teacher_parental_involvement_rev + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european)
m12_european_pv_schl <- fit_pv_model("teacher_parental_involvement_rev + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european)

## ---- CHUNKED STUDENT FE MODELS ----##
stud_out_dir <- file.path("output", "descriptive_stats_panel", "chunked_student_models")
dir.create(stud_out_dir, recursive = TRUE, showWarnings = FALSE)

stud_rhs_raw <- c(
  "books_score + home_resources_learning_rev + env_discuss",
  "children_books_score + env_discuss + teacher_parental_involvement_rev",
  "home_resources_learning_rev + teacher_parental_involvement_rev",
  "env_discuss + parent_edu_binary",
  "teacher_parental_involvement_rev + parent_edu_binary"
)

# Student FE helper:
# - runs FE models by country and plausible value
# - averages estimates within country across PVs
# - averages country-level estimates across countries
fit_pv_country_fe <- function(rhs, dataset, pv_list = pv_vars) {
  # Very simple loop flow:
  # for each PV -> for each country -> run FE -> append coef rows.
  coef_rows <- list()
  fit_rows <- list()

  for (pv_var in pv_list) {
    for (country in sort(unique(dataset[["IDCNTRY"]]))) {
      dat_country <- dataset |>
        filter(.data$IDCNTRY == country)

      mod <- fixest::feols(
        as.formula(paste(pv_var, "~", rhs, "| IDSTUD + year")),
        data = dat_country,
        weights = ~TOTWGT
      )

      cf <- as.data.frame(summary(mod)$coeftable)
      coef_rows[[length(coef_rows) + 1]] <- data.frame(
        country = country,
        pv = pv_var,
        term = rownames(cf),
        estimate = cf[, "Estimate"],
        std.error = cf[, "Std. Error"],
        stringsAsFactors = FALSE
      )

      fit_rows[[length(fit_rows) + 1]] <- data.frame(
        country = country,
        pv = pv_var,
        nobs = stats::nobs(mod),
        r2 = as.numeric(fixest::fitstat(mod, "r2"))[1],
        ar2 = as.numeric(fixest::fitstat(mod, "ar2"))[1],
        stringsAsFactors = FALSE
      )
    }
  }

  coef_df <- dplyr::bind_rows(coef_rows)
  fit_df <- dplyr::bind_rows(fit_rows)

  # 1) Average PVs within each country
  coef_country <- coef_df |>
    dplyr::group_by(.data$country, .data$term) |>
    dplyr::summarise(
      estimate = mean(.data$estimate, na.rm = TRUE),
      std.error = mean(.data$std.error, na.rm = TRUE),
      .groups = "drop"
    )

  # 2) Average countries within region
  coef_region <- coef_country |>
    dplyr::group_by(.data$term) |>
    dplyr::summarise(
      estimate = mean(.data$estimate, na.rm = TRUE),
      std.error = mean(.data$std.error, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      statistic = .data$estimate / .data$std.error,
      p.value = 2 * stats::pnorm(-abs(.data$statistic))
    )

  structure(
    list(
      tidy = as.data.frame(coef_region),
      nobs = mean(fit_df$nobs, na.rm = TRUE),
      r.squared = mean(fit_df$r2, na.rm = TRUE),
      adj.r.squared = mean(fit_df$ar2, na.rm = TRUE)
    ),
    class = "rubin_lm"
  )
}

if (RUN_STUD_MODELS) {
  for (model_id in seq_along(stud_rhs_raw)) {
    cat(sprintf("Running raw student-FE model %d\n", model_id))
    for (region_name in c("panel", "balkan", "european")) {
      cat(sprintf("  Region: %s\n", region_name))
      dat_region <- get(paste0("df_", region_name))
      m_obj <- fit_pv_country_fe(stud_rhs_raw[model_id], dat_region, pv_list = pv_vars)
      saveRDS(m_obj, file = file.path(stud_out_dir, sprintf("m%02d_stud_raw_%s.rds", model_id, region_name)))
    }
  }
}

# SAVE
save(list = ls(pattern = "_pv_ctry$|_pv_schl$"), file = "models_ctry_schl_all.RData")
cat("All models saved.\n")

#####1.2 TABLES #####

library(modelsummary)
library(gt)
library(webshot2)
library(knitr)
filter <- dplyr::filter

out_dir  <- "output/descriptive_stats_panel"
out_file <- file.path(out_dir, "output.md")
writeLines("# TIMSS Regression Tables\n", out_file)

# ── Helper ─────────────────────────────────────────────────────
make_md_table <- function(model_list, model_title, coef_keep, filename, png_name, spanners = NULL) {
  
  gt_tbl <- modelsummary(
    model_list,
    coef_omit   = "Intercept|as\\.factor\\(IDCNTRY\\)|as\\.factor\\(IDSCHOOL\\)|as\\.factor\\(IDSTUD\\)|as\\.factor\\(year\\)",
    coef_rename = coef_keep,
    statistic   = "p.value",
    fmt         = 3,
    stars       = TRUE,
    gof_map     = c("nobs", "r.squared", "adj.r.squared"),
    output      = "gt",
    notes       = paste(
      "* p < 0.05, ** p < 0.01, *** p < 0.001.",
      "Country/School FE models average across 5 plausible values.",
      "Student FE models are estimated per country with IDSTUD + year FE, averaged across PVs within country, then averaged across countries.",
      "Weighted by TOTWGT."
    )
  ) |>
    tab_header(title = model_title) |>
    opt_table_font(font = "Times New Roman") |>
    tab_options(
      table.width               = px(900),
      heading.align             = "left",
      column_labels.font.weight = "bold"
    )
  
  if (!is.null(spanners)) {
    for (s in spanners) gt_tbl <- gt_tbl |> tab_spanner(label = s$label, columns = s$columns)
  }
  
  gt_tbl |> gtsave(png_name)
  
  tbl <- modelsummary(
    model_list,
    coef_omit   = "Intercept|as\\.factor\\(IDCNTRY\\)|as\\.factor\\(IDSCHOOL\\)|as\\.factor\\(IDSTUD\\)|as\\.factor\\(year\\)",
    coef_rename = coef_keep,
    statistic   = "p.value",
    fmt         = 3,
    stars       = TRUE,
    gof_map     = c("nobs", "r.squared", "adj.r.squared"),
    output      = "dataframe"
  )
  
  md_text <- knitr::kable(tbl, format = "markdown")
  cat(paste0("\n## ", model_title, "\n\n"), file = filename, append = TRUE)
  cat(md_text, sep = "\n", file = filename, append = TRUE)
  cat("\n* p < 0.05, ** p < 0.01, *** p < 0.001\n\n", file = filename, append = TRUE)

  rm(gt_tbl, tbl, md_text)
  gc()
}

std_spanners <- list(
  list(label = "Country FE", columns = 2:4),
  list(label = "School FE",  columns = 5:7)
)

# ── Models 1–7 ─────────────────────────────────────

make_md_table(
  model_list  = list("Ctry: Full" = m1_panel_pv_ctry, "Ctry: Balkan" = m1_balkan_pv_ctry,
                     "Ctry: European" = m1_european_pv_ctry, "Schl: Full" = m1_panel_pv_schl,
                     "Schl: Balkan" = m1_balkan_pv_schl, "Schl: European" = m1_european_pv_schl),
  model_title = "Model 1: PI Binary Alone",
  coef_keep   = c("PI_binary" = "PI (Binary)"),
  filename    = out_file, png_name = file.path(out_dir, "table_m1.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m2_panel_pv_ctry, "Ctry: Balkan" = m2_balkan_pv_ctry,
                     "Ctry: European" = m2_european_pv_ctry, "Schl: Full" = m2_panel_pv_schl,
                     "Schl: Balkan" = m2_balkan_pv_schl, "Schl: European" = m2_european_pv_schl),
  model_title = "Model 2: PI + Parental Education",
  coef_keep   = c("PI_binary" = "PI (Binary)", "parent_edu_binary" = "Parental Education"),
  filename    = out_file, png_name = file.path(out_dir, "table_m2.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m3_panel_pv_ctry, "Ctry: Balkan" = m3_balkan_pv_ctry,
                     "Ctry: European" = m3_european_pv_ctry, "Schl: Full" = m3_panel_pv_schl,
                     "Schl: Balkan" = m3_balkan_pv_schl, "Schl: European" = m3_european_pv_schl),
  model_title = "Model 3: PI × Parental Education",
  coef_keep   = c("PI_binary" = "PI (Binary)", "parent_edu_binary" = "Parental Education",
                  "PI_binary:parent_edu_binary" = "PI × Parental Education"),
  filename    = out_file, png_name = file.path(out_dir, "table_m3.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m4_panel_pv_ctry, "Schl: Full" = m4_panel_pv_schl),
  model_title = "Model 4: PI × Parental Education × Is Balkan (Full Sample Only)",
  coef_keep   = c("PI_binary" = "PI (Binary)", "parent_edu_binary" = "Parental Education",
                  "Is_balkan" = "Is Balkan", "PI_binary:parent_edu_binary" = "PI × Parental Education",
                  "PI_binary:Is_balkan" = "PI × Is Balkan",
                  "parent_edu_binary:Is_balkan" = "Parental Education × Is Balkan",
                  "PI_binary:parent_edu_binary:Is_balkan" = "PI × Parental Education × Is Balkan"),
  filename    = out_file, png_name = file.path(out_dir, "table_m4.png"),
  spanners    = list(list(label = "Country FE", columns = 2), list(label = "School FE", columns = 3))
)

make_md_table(
  model_list  = list("Ctry: Full" = m5_panel_pv_ctry, "Ctry: Balkan" = m5_balkan_pv_ctry,
                     "Ctry: European" = m5_european_pv_ctry, "Schl: Full" = m5_panel_pv_schl,
                     "Schl: Balkan" = m5_balkan_pv_schl, "Schl: European" = m5_european_pv_schl),
  model_title = "Model 5: Parental Education + PI (Reading) + PI (Math)",
  coef_keep   = c("parent_edu_binary" = "Parental Education",
                  "PI_read" = "PI (Reading)", "PI_math" = "PI (Math)"),
  filename    = out_file, png_name = file.path(out_dir, "table_m5.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m6_panel_pv_ctry, "Ctry: Balkan" = m6_balkan_pv_ctry,
                     "Ctry: European" = m6_european_pv_ctry, "Schl: Full" = m6_panel_pv_schl,
                     "Schl: Balkan" = m6_balkan_pv_schl, "Schl: European" = m6_european_pv_schl),
  model_title = "Model 6: Parental Education × PI (Reading) + Parental Education × PI (Math)",
  coef_keep   = c("parent_edu_binary" = "Parental Education",
                  "PI_read" = "PI (Reading)", "PI_math" = "PI (Math)",
                  "parent_edu_binary:PI_read" = "Parental Education × PI (Reading)",
                  "parent_edu_binary:PI_math" = "Parental Education × PI (Math)"),
  filename    = out_file, png_name = file.path(out_dir, "table_m6.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m7_panel_pv_ctry, "Ctry: Balkan" = m7_balkan_pv_ctry,
                     "Ctry: European" = m7_european_pv_ctry, "Schl: Full" = m7_panel_pv_schl,
                     "Schl: Balkan" = m7_balkan_pv_schl, "Schl: European" = m7_european_pv_schl),
  model_title = "Model 7: PI × Low Education",
  coef_keep   = c("PI_binary" = "PI (Binary)", "low_edu" = "Low Education",
                  "PI_binary:low_edu" = "PI × Low Education"),
  filename    = out_file, png_name = file.path(out_dir, "table_m7.png"), spanners = std_spanners
)

# ── Models 8–12 ────────────────────────────────────────────────

make_md_table(
  model_list  = list("Ctry: Full" = m8_panel_pv_ctry, "Ctry: Balkan" = m8_balkan_pv_ctry,
                     "Ctry: European" = m8_european_pv_ctry, "Schl: Full" = m8_panel_pv_schl,
                     "Schl: Balkan" = m8_balkan_pv_schl, "Schl: European" = m8_european_pv_schl),
  model_title = "Model 8: Books in House + Parental Education",
  coef_keep   = c("books_score" = "Books in House", "parent_edu_binary" = "Parental Education"),
  filename    = out_file, png_name = file.path(out_dir, "table_m8.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m9_panel_pv_ctry, "Ctry: Balkan" = m9_balkan_pv_ctry,
                     "Ctry: European" = m9_european_pv_ctry, "Schl: Full" = m9_panel_pv_schl,
                     "Schl: Balkan" = m9_balkan_pv_schl, "Schl: European" = m9_european_pv_schl),
  model_title = "Model 9: Children's Books + Parental Education",
  coef_keep   = c("children_books_score" = "Children's Books",
                  "parent_edu_binary" = "Parental Education"),
  filename    = out_file, png_name = file.path(out_dir, "table_m9.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m10_panel_pv_ctry, "Ctry: Balkan" = m10_balkan_pv_ctry,
                     "Ctry: European" = m10_european_pv_ctry, "Schl: Full" = m10_panel_pv_schl,
                     "Schl: Balkan" = m10_balkan_pv_schl, "Schl: European" = m10_european_pv_schl),
  model_title = "Model 10: Home Resources for Learning + Parental Education",
  coef_keep   = c("home_resources_learning_rev" = "Home Resources (Learning)",
                  "parent_edu_binary" = "Parental Education"),
  filename    = out_file, png_name = file.path(out_dir, "table_m10.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m11_panel_pv_ctry, "Ctry: Balkan" = m11_balkan_pv_ctry,
                     "Ctry: European" = m11_european_pv_ctry, "Schl: Full" = m11_panel_pv_schl,
                     "Schl: Balkan" = m11_balkan_pv_schl, "Schl: European" = m11_european_pv_schl),
  model_title = "Model 11: Environmental Discussion (PI) + Parental Education",
  coef_keep   = c("env_discuss" = "Discuss Environment (PI)",
                  "parent_edu_binary" = "Parental Education"),
  filename    = out_file, png_name = file.path(out_dir, "table_m11.png"), spanners = std_spanners
)

make_md_table(
  model_list  = list("Ctry: Full" = m12_panel_pv_ctry, "Ctry: Balkan" = m12_balkan_pv_ctry,
                     "Ctry: European" = m12_european_pv_ctry, "Schl: Full" = m12_panel_pv_schl,
                     "Schl: Balkan" = m12_balkan_pv_schl, "Schl: European" = m12_european_pv_schl),
  model_title = "Model 12: Teacher Perception of PI + Parental Education",
  coef_keep   = c("teacher_parental_involvement_rev" = "Teacher Perception of PI",
                  "parent_edu_binary" = "Parental Education"),
  filename    = out_file, png_name = file.path(out_dir, "table_m12.png"), spanners = std_spanners
)

cat("All tables written to:", out_dir, "\n")

##### 1.3 Student FE Tables (Raw) #####

build_student_fe_tables <- function(mode, model_titles, coef_maps, out_md_file, png_suffix = "") {
  stud_spanners <- list(list(label = "Student FE", columns = 2:4))

  for (model_id in seq_along(model_titles)) {
    # Load one model triplet at a time to keep memory low.
    m_panel <- readRDS(file.path(stud_out_dir, sprintf("m%02d_stud_%s_panel.rds", model_id, mode)))
    m_balkan <- readRDS(file.path(stud_out_dir, sprintf("m%02d_stud_%s_balkan.rds", model_id, mode)))
    m_european <- readRDS(file.path(stud_out_dir, sprintf("m%02d_stud_%s_european.rds", model_id, mode)))

    make_md_table(
      model_list = list(
        "Stud: Full" = m_panel,
        "Stud: Balkan" = m_balkan,
        "Stud: European" = m_european
      ),
      model_title = model_titles[model_id],
      coef_keep = coef_maps[[model_id]],
      filename = out_md_file,
      png_name = file.path(out_dir, sprintf("table_m%d_stud%s.png", model_id, png_suffix)),
      spanners = stud_spanners
    )

    rm(m_panel, m_balkan, m_european)
    gc()
  }
}

stud_titles_raw <- c(
  "Model 1: Books + Home Resources + Environmental Discussion (Student FE)",
  "Model 2: Children's Books + Environmental Discussion + Teacher PI (Student FE)",
  "Model 3: Home Resources + Teacher PI (Student FE)",
  "Model 4: Environmental Discussion + Parental Education (Student FE)",
  "Model 5: Teacher PI + Parental Education (Student FE)"
)

stud_coef_maps_raw <- list(
  c("books_score" = "Books in House", "home_resources_learning_rev" = "Home Resources (Learning)", "env_discuss" = "Discuss Environment (PI)"),
  c("children_books_score" = "Children's Books", "env_discuss" = "Discuss Environment (PI)", "teacher_parental_involvement_rev" = "Teacher PI"),
  c("home_resources_learning_rev" = "Home Resources (Learning)", "teacher_parental_involvement_rev" = "Teacher PI"),
  c("env_discuss" = "Discuss Environment (PI)", "parent_edu_binary" = "Parental Education"),
  c("teacher_parental_involvement_rev" = "Teacher PI", "parent_edu_binary" = "Parental Education")
)

out_file_stud <- file.path(out_dir, "output_stud.md")
writeLines("# TIMSS Regression Tables (Student FE)\n", out_file_stud)
build_student_fe_tables(
  mode = "raw",
  model_titles = stud_titles_raw,
  coef_maps = stud_coef_maps_raw,
  out_md_file = out_file_stud
)
cat("Student FE tables written to:", out_dir, "\n")





#### 2. Z-Scaled Panel Models####
##### 2.0 Setup & Scaling ####
z <- function(x) as.numeric(scale(x))

df_panel <- df_panel |>
  mutate(
    PI_read_z                      = z(PI_read),
    PI_math_z                      = z(PI_math),
    books_score_z                  = z(books_score),
    children_books_score_z         = z(children_books_score),
    home_resources_learning_rev_z  = z(home_resources_learning_rev),
    env_discuss_z                  = z(env_discuss),
    teacher_parental_involvement_z = z(teacher_parental_involvement_rev)
  )

df_balkan_z   <- df_panel |> filter(Is_balkan == 1)
df_european_z <- df_panel |> filter(Is_balkan == 0)

# Sanity check — confirm predictor scaling
df_panel |>
  summarise(across(ends_with("_z"), list(
    mean = ~round(mean(., na.rm = TRUE), 3),
    sd   = ~round(sd(.,   na.rm = TRUE), 3)
  ))) |>
  print(width = Inf)


##### Z-MODEL 1: PI Binary Alone ####
m1z_panel_pv_ctry    <- fit_pv_model("PI_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m1z_panel_pv_schl    <- fit_pv_model("PI_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m1z_balkan_pv_ctry   <- fit_pv_model("PI_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m1z_balkan_pv_schl   <- fit_pv_model("PI_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m1z_european_pv_ctry   <- fit_pv_model("PI_binary + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m1z_european_pv_schl   <- fit_pv_model("PI_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 2: PI + Parental Education ####
m2z_panel_pv_ctry    <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m2z_panel_pv_schl    <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m2z_balkan_pv_ctry   <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m2z_balkan_pv_schl   <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m2z_european_pv_ctry   <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m2z_european_pv_schl   <- fit_pv_model("PI_binary + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 3: PI x Parental Education ####
m3z_panel_pv_ctry    <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m3z_panel_pv_schl    <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m3z_balkan_pv_ctry   <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m3z_balkan_pv_schl   <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m3z_european_pv_ctry   <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m3z_european_pv_schl   <- fit_pv_model("PI_binary * parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 4: PI x Parental Education x Is_Balkan (full sample only) ####
m4z_panel_pv_ctry  <- fit_pv_model("PI_binary * parent_edu_binary * Is_balkan + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m4z_panel_pv_schl  <- fit_pv_model("PI_binary * parent_edu_binary * Is_balkan + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

##### Z-MODEL 5: Parental Education + PI Read (z) + PI Math (z) ####
m5z_panel_pv_ctry    <- fit_pv_model("parent_edu_binary + PI_read_z + PI_math_z + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m5z_panel_pv_schl    <- fit_pv_model("parent_edu_binary + PI_read_z + PI_math_z + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m5z_balkan_pv_ctry   <- fit_pv_model("parent_edu_binary + PI_read_z + PI_math_z + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m5z_balkan_pv_schl   <- fit_pv_model("parent_edu_binary + PI_read_z + PI_math_z + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m5z_european_pv_ctry   <- fit_pv_model("parent_edu_binary + PI_read_z + PI_math_z + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m5z_european_pv_schl   <- fit_pv_model("parent_edu_binary + PI_read_z + PI_math_z + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 6: Parental Education x PI Read (z) + Parental Education x PI Math (z) ####
m6z_panel_pv_ctry    <- fit_pv_model("parent_edu_binary * PI_read_z + parent_edu_binary * PI_math_z + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m6z_panel_pv_schl    <- fit_pv_model("parent_edu_binary * PI_read_z + parent_edu_binary * PI_math_z + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m6z_balkan_pv_ctry   <- fit_pv_model("parent_edu_binary * PI_read_z + parent_edu_binary * PI_math_z + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m6z_balkan_pv_schl   <- fit_pv_model("parent_edu_binary * PI_read_z + parent_edu_binary * PI_math_z + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m6z_european_pv_ctry   <- fit_pv_model("parent_edu_binary * PI_read_z + parent_edu_binary * PI_math_z + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m6z_european_pv_schl   <- fit_pv_model("parent_edu_binary * PI_read_z + parent_edu_binary * PI_math_z + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 7: PI x Low Education ####
m7z_panel_pv_ctry    <- fit_pv_model("PI_binary * low_edu + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m7z_panel_pv_schl    <- fit_pv_model("PI_binary * low_edu + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m7z_balkan_pv_ctry   <- fit_pv_model("PI_binary * low_edu + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m7z_balkan_pv_schl   <- fit_pv_model("PI_binary * low_edu + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m7z_european_pv_ctry   <- fit_pv_model("PI_binary * low_edu + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m7z_european_pv_schl   <- fit_pv_model("PI_binary * low_edu + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 8: Books in House (z) + Parental Education ####
m8z_panel_pv_ctry    <- fit_pv_model("books_score_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m8z_panel_pv_schl    <- fit_pv_model("books_score_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m8z_balkan_pv_ctry   <- fit_pv_model("books_score_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m8z_balkan_pv_schl   <- fit_pv_model("books_score_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m8z_european_pv_ctry   <- fit_pv_model("books_score_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m8z_european_pv_schl   <- fit_pv_model("books_score_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 9: Children's Books (z) + Parental Education ####
m9z_panel_pv_ctry    <- fit_pv_model("children_books_score_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m9z_panel_pv_schl    <- fit_pv_model("children_books_score_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m9z_balkan_pv_ctry   <- fit_pv_model("children_books_score_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m9z_balkan_pv_schl   <- fit_pv_model("children_books_score_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m9z_european_pv_ctry   <- fit_pv_model("children_books_score_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m9z_european_pv_schl   <- fit_pv_model("children_books_score_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 10: Home Resources (z) + Parental Education ####
m10z_panel_pv_ctry    <- fit_pv_model("home_resources_learning_rev_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m10z_panel_pv_schl    <- fit_pv_model("home_resources_learning_rev_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m10z_balkan_pv_ctry   <- fit_pv_model("home_resources_learning_rev_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m10z_balkan_pv_schl   <- fit_pv_model("home_resources_learning_rev_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m10z_european_pv_ctry   <- fit_pv_model("home_resources_learning_rev_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m10z_european_pv_schl   <- fit_pv_model("home_resources_learning_rev_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 11: Environmental Discussion (z) + Parental Education ####
m11z_panel_pv_ctry    <- fit_pv_model("env_discuss_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m11z_panel_pv_schl    <- fit_pv_model("env_discuss_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m11z_balkan_pv_ctry   <- fit_pv_model("env_discuss_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m11z_balkan_pv_schl   <- fit_pv_model("env_discuss_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m11z_european_pv_ctry   <- fit_pv_model("env_discuss_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m11z_european_pv_schl   <- fit_pv_model("env_discuss_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

##### Z-MODEL 12: Teacher Perception of PI (z) + Parental Education ####
m12z_panel_pv_ctry    <- fit_pv_model("teacher_parental_involvement_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_panel)
m12z_panel_pv_schl    <- fit_pv_model("teacher_parental_involvement_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_panel)

m12z_balkan_pv_ctry   <- fit_pv_model("teacher_parental_involvement_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_balkan_z)
m12z_balkan_pv_schl   <- fit_pv_model("teacher_parental_involvement_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_balkan_z)

m12z_european_pv_ctry   <- fit_pv_model("teacher_parental_involvement_z + parent_edu_binary + as.factor(IDCNTRY) + as.factor(year)", df_european_z)
m12z_european_pv_schl   <- fit_pv_model("teacher_parental_involvement_z + parent_edu_binary + as.factor(IDSCHOOL) + as.factor(year)", df_european_z)

## ---- CHUNKED STUDENT FE MODELS ----##
stud_rhs_z <- c(
  "books_score_z + home_resources_learning_rev_z + env_discuss_z",
  "children_books_score_z + env_discuss_z + teacher_parental_involvement_z",
  "home_resources_learning_rev_z + teacher_parental_involvement_z",
  "env_discuss_z + parent_edu_binary",
  "teacher_parental_involvement_z + parent_edu_binary"
)

if (RUN_STUD_MODELS) {
  for (model_id in seq_along(stud_rhs_z)) {
    cat(sprintf("Running Z student-FE model %d\n", model_id))
    for (region_name in c("panel", "balkan", "european")) {
      cat(sprintf("  Region: %s\n", region_name))
      dat_region <- get(ifelse(region_name == "panel", "df_panel", paste0("df_", region_name, "_z")))
      m_obj <- fit_pv_country_fe(stud_rhs_z[model_id], dat_region, pv_list = pv_vars)
      saveRDS(m_obj, file = file.path(stud_out_dir, sprintf("m%02d_stud_z_%s.rds", model_id, region_name)))
    }
  }
}

## SAVE Z-Models
save(list = ls(pattern = "^m[0-9]+z_"), file = file.path(out_dir, "models_z_scored.RData"))
cat("Z-scored models saved.\n")


#### 2.2 Tables ####

out_file_z <- file.path(out_dir, "output_z.md")
writeLines("# TIMSS Regression Tables (Standardised)\n", out_file_z)

std_spanners <- list(
  list(label = "Country FE", columns = 2:4),
  list(label = "School FE",  columns = 5:7)
)

## Z-TABLE 1 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m1z_panel_pv_ctry,
    "Ctry: Balkan"   = m1z_balkan_pv_ctry,
    "Ctry: European" = m1z_european_pv_ctry,
    "Schl: Full"     = m1z_panel_pv_schl,
    "Schl: Balkan"   = m1z_balkan_pv_schl,
    "Schl: European" = m1z_european_pv_schl
  ),
  model_title = "Model 1 (Standardised): PI Binary Alone",
  coef_keep   = c("PI_binary" = "PI (Binary)"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m1z.png"),
  spanners    = std_spanners
)

## Z-TABLE 2 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m2z_panel_pv_ctry,
    "Ctry: Balkan"   = m2z_balkan_pv_ctry,
    "Ctry: European" = m2z_european_pv_ctry,
    "Schl: Full"     = m2z_panel_pv_schl,
    "Schl: Balkan"   = m2z_balkan_pv_schl,
    "Schl: European" = m2z_european_pv_schl
  ),
  model_title = "Model 2 (Standardised): PI + Parental Education",
  coef_keep   = c("PI_binary"         = "PI (Binary)",
                  "parent_edu_binary" = "Parental Education"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m2z.png"),
  spanners    = std_spanners
)

## Z-TABLE 3 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m3z_panel_pv_ctry,
    "Ctry: Balkan"   = m3z_balkan_pv_ctry,
    "Ctry: European" = m3z_european_pv_ctry,
    "Schl: Full"     = m3z_panel_pv_schl,
    "Schl: Balkan"   = m3z_balkan_pv_schl,
    "Schl: European" = m3z_european_pv_schl
  ),
  model_title = "Model 3 (Standardised): PI × Parental Education",
  coef_keep   = c("PI_binary"                   = "PI (Binary)",
                  "parent_edu_binary"           = "Parental Education",
                  "PI_binary:parent_edu_binary" = "PI × Parental Education"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m3z.png"),
  spanners    = std_spanners
)

## Z-TABLE 4 
make_md_table(
  model_list  = list(
    "Ctry: Full" = m4z_panel_pv_ctry,
    "Schl: Full" = m4z_panel_pv_schl
  ),
  model_title = "Model 4 (Standardised): PI × Parental Education × Is Balkan",
  coef_keep   = c(
    "PI_binary"                             = "PI (Binary)",
    "parent_edu_binary"                     = "Parental Education",
    "Is_balkan"                             = "Is Balkan",
    "PI_binary:parent_edu_binary"           = "PI × Parental Education",
    "PI_binary:Is_balkan"                   = "PI × Is Balkan",
    "parent_edu_binary:Is_balkan"           = "Parental Education × Is Balkan",
    "PI_binary:parent_edu_binary:Is_balkan" = "PI × Parental Education × Is Balkan"
  ),
  filename = out_file_z,
  png_name = file.path(out_dir, "table_m4z.png"),
  spanners = list(
    list(label = "Country FE", columns = 2),
    list(label = "School FE",  columns = 3)
  )
)

## Z-TABLE 5 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m5z_panel_pv_ctry,
    "Ctry: Balkan"   = m5z_balkan_pv_ctry,
    "Ctry: European" = m5z_european_pv_ctry,
    "Schl: Full"     = m5z_panel_pv_schl,
    "Schl: Balkan"   = m5z_balkan_pv_schl,
    "Schl: European" = m5z_european_pv_schl
  ),
  model_title = "Model 5 (Standardised): Parental Education + PI Reading (SD) + PI Math (SD)",
  coef_keep   = c("parent_edu_binary" = "Parental Education",
                  "PI_read_z"         = "PI Reading (SD)",
                  "PI_math_z"         = "PI Math (SD)"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m5z.png"),
  spanners    = std_spanners
)

## Z-TABLE 6 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m6z_panel_pv_ctry,
    "Ctry: Balkan"   = m6z_balkan_pv_ctry,
    "Ctry: European" = m6z_european_pv_ctry,
    "Schl: Full"     = m6z_panel_pv_schl,
    "Schl: Balkan"   = m6z_balkan_pv_schl,
    "Schl: European" = m6z_european_pv_schl
  ),
  model_title = "Model 6 (Standardised): Parental Education × PI Reading (SD) + Parental Education × PI Math (SD)",
  coef_keep   = c("parent_edu_binary"            = "Parental Education",
                  "PI_read_z"                    = "PI Reading (SD)",
                  "PI_math_z"                    = "PI Math (SD)",
                  "parent_edu_binary:PI_read_z"  = "Parental Education × PI Reading (SD)",
                  "parent_edu_binary:PI_math_z"  = "Parental Education × PI Math (SD)"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m6z.png"),
  spanners    = std_spanners
)

## Z-TABLE 7 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m7z_panel_pv_ctry,
    "Ctry: Balkan"   = m7z_balkan_pv_ctry,
    "Ctry: European" = m7z_european_pv_ctry,
    "Schl: Full"     = m7z_panel_pv_schl,
    "Schl: Balkan"   = m7z_balkan_pv_schl,
    "Schl: European" = m7z_european_pv_schl
  ),
  model_title = "Model 7 (Standardised): PI × Low Education",
  coef_keep   = c("PI_binary"         = "PI (Binary)",
                  "low_edu"           = "Low Education",
                  "PI_binary:low_edu" = "PI × Low Education"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m7z.png"),
  spanners    = std_spanners
)

## Z-TABLE 8 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m8z_panel_pv_ctry,
    "Ctry: Balkan"   = m8z_balkan_pv_ctry,
    "Ctry: European" = m8z_european_pv_ctry,
    "Schl: Full"     = m8z_panel_pv_schl,
    "Schl: Balkan"   = m8z_balkan_pv_schl,
    "Schl: European" = m8z_european_pv_schl
  ),
  model_title = "Model 8 (Standardised): Books in House (SD) + Parental Education",
  coef_keep   = c("books_score_z"     = "Books in House (SD)",
                  "parent_edu_binary" = "Parental Education"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m8z.png"),
  spanners    = std_spanners
)

## Z-TABLE 9 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m9z_panel_pv_ctry,
    "Ctry: Balkan"   = m9z_balkan_pv_ctry,
    "Ctry: European" = m9z_european_pv_ctry,
    "Schl: Full"     = m9z_panel_pv_schl,
    "Schl: Balkan"   = m9z_balkan_pv_schl,
    "Schl: European" = m9z_european_pv_schl
  ),
  model_title = "Model 9 (Standardised): Children's Books (SD) + Parental Education",
  coef_keep   = c("children_books_score_z" = "Children's Books (SD)",
                  "parent_edu_binary"      = "Parental Education"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m9z.png"),
  spanners    = std_spanners
)

## Z-TABLE 10 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m10z_panel_pv_ctry,
    "Ctry: Balkan"   = m10z_balkan_pv_ctry,
    "Ctry: European" = m10z_european_pv_ctry,
    "Schl: Full"     = m10z_panel_pv_schl,
    "Schl: Balkan"   = m10z_balkan_pv_schl,
    "Schl: European" = m10z_european_pv_schl
  ),
  model_title = "Model 10 (Standardised): Home Resources (SD) + Parental Education",
  coef_keep   = c("home_resources_learning_rev_z" = "Home Resources (SD)",
                  "parent_edu_binary"             = "Parental Education"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m10z.png"),
  spanners    = std_spanners
)

## Z-TABLE 11 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m11z_panel_pv_ctry,
    "Ctry: Balkan"   = m11z_balkan_pv_ctry,
    "Ctry: European" = m11z_european_pv_ctry,
    "Schl: Full"     = m11z_panel_pv_schl,
    "Schl: Balkan"   = m11z_balkan_pv_schl,
    "Schl: European" = m11z_european_pv_schl
  ),
  model_title = "Model 11 (Standardised): Environmental Discussion (SD) + Parental Education",
  coef_keep   = c("env_discuss_z"     = "Discuss Environment (SD)",
                  "parent_edu_binary" = "Parental Education"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m11z.png"),
  spanners    = std_spanners
)

## Z-TABLE 12 
make_md_table(
  model_list  = list(
    "Ctry: Full"     = m12z_panel_pv_ctry,
    "Ctry: Balkan"   = m12z_balkan_pv_ctry,
    "Ctry: European" = m12z_european_pv_ctry,
    "Schl: Full"     = m12z_panel_pv_schl,
    "Schl: Balkan"   = m12z_balkan_pv_schl,
    "Schl: European" = m12z_european_pv_schl
  ),
  model_title = "Model 12 (Standardised): Teacher Perception of PI (SD) + Parental Education",
  coef_keep   = c("teacher_parental_involvement_z" = "Teacher Perception of PI (SD)",
                  "parent_edu_binary"              = "Parental Education"),
  filename    = out_file_z,
  png_name    = file.path(out_dir, "table_m12z.png"),
  spanners    = std_spanners
)

cat("All standardised tables written to:", out_dir, "\n")

##### 2.3 Student FE Tables (Standardised) #####
stud_titles_z <- c(
  "Model 1 (Standardised): Books + Home Resources + Environmental Discussion (Student FE)",
  "Model 2 (Standardised): Children's Books + Environmental Discussion + Teacher PI (Student FE)",
  "Model 3 (Standardised): Home Resources + Teacher PI (Student FE)",
  "Model 4 (Standardised): Environmental Discussion + Parental Education (Student FE)",
  "Model 5 (Standardised): Teacher PI + Parental Education (Student FE)"
)

stud_coef_maps_z <- list(
  c("books_score_z" = "Books in House (SD)", "home_resources_learning_rev_z" = "Home Resources (SD)", "env_discuss_z" = "Discuss Environment (SD)"),
  c("children_books_score_z" = "Children's Books (SD)", "env_discuss_z" = "Discuss Environment (SD)", "teacher_parental_involvement_z" = "Teacher PI (SD)"),
  c("home_resources_learning_rev_z" = "Home Resources (SD)", "teacher_parental_involvement_z" = "Teacher PI (SD)"),
  c("env_discuss_z" = "Discuss Environment (SD)", "parent_edu_binary" = "Parental Education"),
  c("teacher_parental_involvement_z" = "Teacher PI (SD)", "parent_edu_binary" = "Parental Education")
)

out_file_stud_z <- file.path(out_dir, "output_stud_z.md")
writeLines("# TIMSS Regression Tables (Student FE, Standardised)\n", out_file_stud_z)
build_student_fe_tables(
  mode = "z",
  model_titles = stud_titles_z,
  coef_maps = stud_coef_maps_z,
  out_md_file = out_file_stud_z,
  png_suffix = "z"
)
cat("Student FE standardised tables written to:", out_dir, "\n")