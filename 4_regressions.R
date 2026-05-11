# =============================================================================
# 4. Regressions – by country, Europe, Balkans
# Uses: dplyr, survey, broom, kableExtra
# Saves all tables as both HTML and CSV.
# =============================================================================

rm(list = ls())
library(dplyr)
library(survey)
library(broom)
library(kableExtra)
source("R/rubin_pv_helpers.R")

master_path <- "data/processed_data/master/master_processed.RData"
out_dir <- "output/tables"
dir.create(out_dir, showWarnings = FALSE)

###########################################
## ---- FUNCTION DEFINITION SECTION ---- ##
###########################################

# Helper: save table as HTML and CSV
save_table <- function(tbl, caption, base_name) {
  k <- kableExtra::kbl(tbl, caption = caption, format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE, color = "white", background = "#002244")
  kableExtra::save_kable(k, file.path(out_dir, paste0(base_name, ".html")), self_contained = FALSE)
  write.csv(tbl, file.path(out_dir, paste0(base_name, ".csv")), row.names = FALSE)
  message("Saved ", base_name, ".html and .csv")
}

# Run PV regression (Math outcome), Rubin combine. Returns list(beta, se, pval)
run_pv_regression <- function(design, dep_vars, indep_rhs) {
  est <- se <- numeric(5)
  pred_vars <- all.vars(as.formula(paste0("~", indep_rhs)))
  for (j in 1:5) {
    use <- complete.cases(design$variables[, c(dep_vars[j], pred_vars)])
    for (p in pred_vars) use <- use & !(design$variables[[p]] %in% c(998, 999))
    dsub <- design[use, ]
    if (nrow(dsub$variables) == 0) stop("No usable rows for PV regression: ", dep_vars[j], " ~ ", indep_rhs)
    fit <- suppressWarnings(svyglm(as.formula(paste(dep_vars[j], "~", indep_rhs)), design = dsub))
    cj <- broom::tidy(fit) %>% filter(.data$term != "(Intercept)")
    est[j] <- cj$estimate[1]
    se[j] <- cj$std.error[1]
  }
  cmb <- rubin_combine(est, se, m = 5)
  beta <- cmb$beta
  se_tot <- cmb$se
  pval <- 2 * pnorm(-abs(beta / se_tot))
  list(beta = beta, se = se_tot, pval = pval)
}

# Run single-outcome regression (for PI_index ~ SES_binary)
run_single_regression <- function(design, dep_var, formula_rhs) {
  pred_vars <- all.vars(as.formula(paste0("~", formula_rhs)))
  use <- complete.cases(design$variables[, c(dep_var, pred_vars)])
  use <- use & !(design$variables[[dep_var]] %in% c(998, 999))
  for (p in pred_vars) use <- use & !(design$variables[[p]] %in% c(998, 999))
  dsub <- design[use, ]
  if (nrow(dsub$variables) < 20) stop("Too few rows for regression: ", dep_var, " ~ ", formula_rhs)
  fit <- suppressWarnings(svyglm(as.formula(paste0(dep_var, " ~ ", formula_rhs)), design = dsub))
  cj <- broom::tidy(fit) %>% filter(.data$term != "(Intercept)")
  beta <- cj$estimate[1]
  se <- cj$std.error[1]
  pval <- 2 * pnorm(-abs(beta / se))
  list(beta = beta, se = se, pval = pval, r2 = NA_real_)
}

# Build design from dat
build_design <- function(dat) {
  njk <- max(dat$JKZONE, na.rm = TRUE)                                                   # Get number of JK zones (strata for replication)
  for (i in 1:njk) {
    dat[[paste0("rwgt_", i)]] <- ifelse(dat$JKZONE == i & dat$JKREP == 1, 2 * dat$TOTWGT, # If in zone i and replicate 1, double weight
                                        ifelse(dat$JKZONE == i & dat$JKREP == 0, 0, dat$TOTWGT))} # If in zone i and replicate 0, zero weight; else keep original
  rwgt_vars <- grep("^rwgt_[0-9]+$", names(dat), value = TRUE) # Get all replication weight column names
  ok_design <- complete.cases(dat[, c("TOTWGT", "JKZONE", "JKREP", rwgt_vars[1])]) # Identify rows with complete data for key variables
  svrepdesign(weights = ~TOTWGT, # Specify the full-sample weight variable
              repweights = dat[ok_design, rwgt_vars], # Pass the replication weight columns for complete rows
              type = "JK2", # Specify Jackknife type 2 replication method
              combined.weights = TRUE, # Indicate that repweights include the full-sample weight factor
              data = dat[ok_design, ])} # Use only complete rows as the data frame

# Weighted SD of a numeric vector (for beta_std = beta / SD(Y))
wtd_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  m <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}

# Group names: unique CountryName from design, then non-Balkan Europe and Balkans.
# Country regressions subset by CountryName; regional regressions subset by Is_balkan.
get_group_names <- function(design) {
  v <- design$variables
  cnames <- unique(v$CountryName[!is.na(v$CountryName) & nzchar(as.character(v$CountryName))])
  c(as.character(sort(cnames)), "Europe", "Balkans")
}
# Subset design to one group using CountryName or Is_balkan.
design_subset <- function(design, gname) {
  v <- design$variables
  if (gname == "Europe") return(design[v$Is_balkan == 0 & !is.na(v$Is_balkan), ])
  if (gname == "Balkans") return(design[v$Is_balkan == 1 & !is.na(v$Is_balkan), ])
  return(design[v$CountryName == gname & !is.na(v$CountryName), ])
}

# Diagnostic: print by country the % of 1s in a binary variable (and non-NA n)
print_binary_by_country <- function(design, binary_var, year_label = "") {
  v <- design$variables
  if (!binary_var %in% names(v)) { message("Variable ", binary_var, " not in design."); return(invisible(NULL)) }
  by_country <- v %>%
    filter(!is.na(.data[["CountryName"]]) & nzchar(as.character(.data[["CountryName"]]))) %>%
    group_by(.data[["CountryName"]]) %>%
    summarise(
      n = sum(!is.na(.data[[binary_var]])),
      n1 = sum(.data[[binary_var]] == 1, na.rm = TRUE),
      pct_1 = if_else(n > 0, 100 * n1 / n, NA_real_),
      .groups = "drop"
    ) %>%
    arrange(.data[["CountryName"]])
  title <- paste0(binary_var, " by country", if (nzchar(year_label)) paste0(" (", year_label, ")") else "", ": pct_1 = % value 1, n = non-NA")
  message("\n--- ", title, " ---")
  print(as.data.frame(by_country))
  invisible(by_country)
}

## ---- PV values vector ---- ##

pv_math <- paste0("ASMMAT", sprintf("%02d", 1:5), "_ptile_region")

##########################################
## ---- Running regression on 2019 ---- ##
## NOTE: WE ARE ONLY RUNNING WITH OBJECTIVE ON MATH! NOT SCIENCE NOR REASON
##########################################

load(master_path)
if (!exists("master_processed")) stop("master_processed not found in ", master_path, ". Run 2_objective_builder.R first.")
master_refactored <- master_processed %>%
  mutate(across(everything(), ~ ifelse(. %in% c(998, 999), NA, .)))

dat19 <- master_refactored %>% filter(year == 2019, CountryName != "Netherlands") 
dat19$IDCNTRY <- as.numeric(dat19$IDCNTRY)
design19 <- build_design(dat19)

print_binary_by_country(design19, "parent_edu_binary", "2019")

dat19_hist <- master_refactored %>% filter(year == 2019, CountryName == "Czech Republic", PI_index != c(998, 999))
hist(dat19_hist$PI_index)
dat23_hist <- master_refactored %>% filter(year == 2023, CountryName == "Czech Republic", PI_index != c(998, 999))
hist(dat23_hist$PI_index)

# --- Math ~ parent_edu_binary ---
res_parent_edu_binary <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), Sig = character(), Beta_std = numeric(), stringsAsFactors = FALSE)
for (gname in get_group_names(design19)) {
  dg <- design_subset(design19, gname)
  if (nrow(dg$variables) < 20) next
  r <- run_pv_regression(dg, pv_math, "parent_edu_binary")
  v <- dg$variables
  if (all(pv_math %in% names(v))) {
    math_avg <- rowMeans(v[, pv_math], na.rm = TRUE)
    sd_math <- wtd_sd(math_avg, v$TOTWGT)
    beta_std <- if (!is.na(sd_math) && sd_math > 0 && !is.na(r$beta)) r$beta / sd_math else NA_real_
  } else beta_std <- NA_real_
  sig <- if (is.na(r$pval)) "" else if (r$pval < 0.01) "***" else if (r$pval < 0.05) "**" else if (r$pval < 0.1) "*" else ""
  res_parent_edu_binary <- rbind(res_parent_edu_binary, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval, Sig = sig, Beta_std = beta_std))
}
res_parent_edu_binary$Beta_SE <- sprintf("%.3f%s (%.3f)", res_parent_edu_binary$Beta, res_parent_edu_binary$Sig, res_parent_edu_binary$SE)
tbl <- res_parent_edu_binary %>% select(Group, Beta, Beta_SE, Beta_std)
save_table(tbl, "Math ~ parent_edu_binary by country/region (2019)", "reg_math_parent_edu_binary_2019")

# --- Parental Investment by SES: PI_index ~ parent_edu_binary (do high-SES parents invest more?) ---
res_pi_ses <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), Sig = character(), stringsAsFactors = FALSE)
for (gname in get_group_names(design19)) {
  dg <- design_subset(design19, gname)
  if (nrow(dg$variables) < 20) next
  r <- run_single_regression(dg, "PI_index", "parent_edu_binary")
  sig <- if (is.na(r$pval)) "" else if (r$pval < 0.01) "***" else if (r$pval < 0.05) "**" else if (r$pval < 0.1) "*" else ""
  res_pi_ses <- rbind(res_pi_ses, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval, Sig = sig, stringsAsFactors = FALSE))
}
res_pi_ses$Beta_SE <- sprintf("%.3f%s (%.3f)", res_pi_ses$Beta, res_pi_ses$Sig, res_pi_ses$SE)
tbl <- res_pi_ses %>% select(Group, Beta, Beta_SE)
save_table(tbl, "Parental Investment Index by Parental Education (Regression, 2019)", "reg_pi_index_parent_edu_binary_2019")
# Regional-level only
tbl_reg_math19 <- res_parent_edu_binary %>% filter(Group %in% c("Europe", "Balkans")) %>% select(Group, Beta, Beta_SE, Beta_std)
tbl_reg_pi19  <- res_pi_ses %>% filter(Group %in% c("Europe", "Balkans")) %>% select(Group, Beta, Beta_SE)
save_table(tbl_reg_math19, "Math ~ parent_edu_binary by region (2019)", "reg_math_parent_edu_binary_regional_2019")
save_table(tbl_reg_pi19, "PI_index ~ parent_edu_binary by region (2019)", "reg_pi_index_parent_edu_binary_regional_2019")


# --- Math ~ SES_binary (for 5_correlations) ---
res_ses_binary <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), stringsAsFactors = FALSE)
for (gname in get_group_names(design19)) {
  dg <- design_subset(design19, gname)
  if (nrow(dg$variables) < 20) next
  r <- run_pv_regression(dg, pv_math, "SES_binary")
  res_ses_binary <- rbind(res_ses_binary, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval))
}
res_ses_binary$Beta_SE <- sprintf("%.3f (%.3f)", res_ses_binary$Beta, res_ses_binary$SE)
tbl <- res_ses_binary %>% select(Group, Beta_SE)
save_table(tbl, "Math ~ SES_binary by country/region (2019)", "reg_math_ses_binary_2019")

# --- Math ~ PI_binary (for 5_correlations) ---
res_pi_binary <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), stringsAsFactors = FALSE)
for (gname in get_group_names(design19)) {
  dg <- design_subset(design19, gname)
  if (nrow(dg$variables) < 20) next
  r <- run_pv_regression(dg, pv_math, "PI_binary")
  res_pi_binary <- rbind(res_pi_binary, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval))
}
res_pi_binary$Beta_SE <- sprintf("%.3f (%.3f)", res_pi_binary$Beta, res_pi_binary$SE)
tbl <- res_pi_binary %>% select(Group, Beta_SE)
save_table(tbl, "Math ~ PI_binary by country/region (2019)", "reg_math_pi_binary_2019")

# Save coefficient tables for 5_correlations (country-level only, 2019)
cty19 <- setdiff(unique(res_ses_binary$Group), c("Europe", "Balkans"))
coef_2019 <- data.frame(
  Country = cty19,
  year = 2019,
  beta_SES_binary = res_ses_binary$Beta[match(cty19, res_ses_binary$Group)],
  beta_PI_index = res_pi_binary$Beta[match(cty19, res_pi_binary$Group)]
)
write.csv(coef_2019, file.path(out_dir, "reg_coefficients_2019.csv"), row.names = FALSE)

print("\n--- 2019: Math ~ SES_index ---\n")
print(res_parent_edu_binary %>% filter(Group == "Europe"))
print(res_parent_edu_binary %>% filter(Group == "Balkans"))
print("\n--- 2019: PI_index ~ SES_binary (Balkans) ---\n")
print(res_pi_ses %>% filter(Group == "Balkans"))

###################################
## ---- Regressions fo 2023 ---- ##
###################################

dat23 <- master_refactored %>% filter(year == 2023, CountryName != "Netherlands")
dat23$IDCNTRY <- as.numeric(dat23$IDCNTRY)

design23 <- build_design(dat23)

print_binary_by_country(design23, "parent_edu_binary", "2023")

# --- Math ~ parent_edu_binary ---
res_parent_edu_binary23 <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), Sig = character(), Beta_std = numeric(), stringsAsFactors = FALSE)
for (gname in get_group_names(design23)) {
  dg <- design_subset(design23, gname)
  if (nrow(dg$variables) < 20) next
  r <- run_pv_regression(dg, pv_math, "parent_edu_binary")
  v <- dg$variables
  if (all(pv_math %in% names(v))) {
    math_avg <- rowMeans(v[, pv_math], na.rm = TRUE)
    sd_math <- wtd_sd(math_avg, v$TOTWGT)
    beta_std <- if (!is.na(sd_math) && sd_math > 0 && !is.na(r$beta)) r$beta / sd_math else NA_real_
  } else beta_std <- NA_real_
  sig <- if (is.na(r$pval)) "" else if (r$pval < 0.01) "***" else if (r$pval < 0.05) "**" else if (r$pval < 0.1) "*" else ""
  res_parent_edu_binary23 <- rbind(res_parent_edu_binary23, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval, Sig = sig, Beta_std = beta_std))
}
res_parent_edu_binary23$Beta_SE <- sprintf("%.3f%s (%.3f)", res_parent_edu_binary23$Beta, res_parent_edu_binary23$Sig, res_parent_edu_binary23$SE)
tbl <- res_parent_edu_binary23 %>% select(Group, Beta, Beta_SE, Beta_std)
save_table(tbl, "Math ~ parent_edu_binary by country/region (2023)", "reg_math_parent_edu_binary_2023")

# --- Parental Investment by SES: PI_index ~ parent_edu_binary (2023) ---
res_pi_ses23 <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), Sig = character(), stringsAsFactors = FALSE)
for (gname in get_group_names(design23)) {
  dg <- design_subset(design23, gname)
  if (nrow(dg$variables) < 20) next
  r <- run_single_regression(dg, "PI_index", "parent_edu_binary")
  sig <- if (is.na(r$pval)) "" else if (r$pval < 0.01) "***" else if (r$pval < 0.05) "**" else if (r$pval < 0.1) "*" else ""
  res_pi_ses23 <- rbind(res_pi_ses23, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval, Sig = sig, stringsAsFactors = FALSE))
}
res_pi_ses23$Beta_SE <- sprintf("%.3f%s (%.3f)", res_pi_ses23$Beta, res_pi_ses23$Sig, res_pi_ses23$SE)
tbl <- res_pi_ses23 %>% select(Group, Beta, Beta_SE)
save_table(tbl, "Parental Investment Index by Parental Education (Regression, 2023)", "reg_pi_index_parent_edu_binary_2023")
# Regional-level only
tbl_reg_math23 <- res_parent_edu_binary23 %>% filter(Group %in% c("Europe", "Balkans")) %>% select(Group, Beta, Beta_SE, Beta_std)
tbl_reg_pi23   <- res_pi_ses23 %>% filter(Group %in% c("Europe", "Balkans")) %>% select(Group, Beta, Beta_SE)
save_table(tbl_reg_math23, "Math ~ parent_edu_binary by region (2023)", "reg_math_parent_edu_binary_regional_2023")
save_table(tbl_reg_pi23, "PI_index ~ parent_edu_binary by region (2023)", "reg_pi_index_parent_edu_binary_regional_2023")


# --- Test: Is PI_index gradient (Balkans) significantly larger than (Europe)? ---
# Two-sample z-test for difference in coefficients (independent regional estimates).
test_region_diff <- function(res_df, year_label) {
  be <- res_df %>% filter(Group == "Europe")  %>% pull(Beta); se_e <- res_df %>% filter(Group == "Europe")  %>% pull(SE)
  bb <- res_df %>% filter(Group == "Balkans") %>% pull(Beta); se_b <- res_df %>% filter(Group == "Balkans") %>% pull(SE)
  if (length(be) == 0 || length(bb) == 0 || is.na(be) || is.na(bb)) return(list(diff = NA, SE_diff = NA, z = NA, p = NA))
  diff <- bb - be
  SE_diff <- sqrt(se_b^2 + se_e^2)
  z <- diff / SE_diff
  p <- 2 * pnorm(-abs(z))
  list(year = year_label, Beta_Europe = be, Beta_Balkans = bb, diff = diff, SE_diff = SE_diff, z = z, p = p)
}
pi_diff_2019 <- test_region_diff(res_pi_ses, "2019")
pi_diff_2023 <- test_region_diff(res_pi_ses23, "2023")
cat("\n--- PI_index ~ parent_edu_binary: Europe vs Balkans (test that Balkans > Europe) ---\n")
cat(sprintf("  2019: Beta Europe = %.3f, Beta Balkans = %.3f, diff = %.3f, SE(diff) = %.3f, z = %.3f, p = %.4f\n",
            pi_diff_2019$Beta_Europe, pi_diff_2019$Beta_Balkans, pi_diff_2019$diff, pi_diff_2019$SE_diff, pi_diff_2019$z, pi_diff_2019$p))
cat(sprintf("  2023: Beta Europe = %.3f, Beta Balkans = %.3f, diff = %.3f, SE(diff) = %.3f, z = %.3f, p = %.4f\n",
            pi_diff_2023$Beta_Europe, pi_diff_2023$Beta_Balkans, pi_diff_2023$diff, pi_diff_2023$SE_diff, pi_diff_2023$z, pi_diff_2023$p))
pi_region_test <- data.frame(
  Year = c(2019, 2023),
  Beta_Europe = c(pi_diff_2019$Beta_Europe, pi_diff_2023$Beta_Europe),
  Beta_Balkans = c(pi_diff_2019$Beta_Balkans, pi_diff_2023$Beta_Balkans),
  diff_Balkans_minus_Europe = c(pi_diff_2019$diff, pi_diff_2023$diff),
  SE_diff = c(pi_diff_2019$SE_diff, pi_diff_2023$SE_diff),
  z = c(pi_diff_2019$z, pi_diff_2023$z),
  p_value = c(pi_diff_2019$p, pi_diff_2023$p)
)
write.csv(pi_region_test, file.path(out_dir, "reg_PI_gradient_Europe_vs_Balkans_test.csv"), row.names = FALSE)
message("Saved reg_PI_gradient_Europe_vs_Balkans_test.csv")


# --- Math ~ SES_binary ---
res_ses_binary23 <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), stringsAsFactors = FALSE)
for (gname in get_group_names(design23)) {
  dg <- design_subset(design23, gname)
  if (nrow(dg$variables) < 20) next
  r <- run_pv_regression(dg, pv_math, "SES_binary")
  res_ses_binary23 <- rbind(res_ses_binary23, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval))
}
res_ses_binary23$Beta_SE <- sprintf("%.3f (%.3f)", res_ses_binary23$Beta, res_ses_binary23$SE)
tbl <- res_ses_binary23 %>% select(Group, Beta_SE)
save_table(tbl, "Math ~ SES_binary by country/region (2023)", "reg_math_ses_binary_2023")

# --- Math ~ PI_binary ---
res_pi_binary23 <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), stringsAsFactors = FALSE)
for (gname in get_group_names(design23)) {
  dg <- design_subset(design23, gname)
  if (nrow(dg$variables) < 20) next
  r <- run_pv_regression(dg, pv_math, "PI_binary")
  res_pi_binary23 <- rbind(res_pi_binary23, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval))
}
res_pi_binary23$Beta_SE <- sprintf("%.3f (%.3f)", res_pi_binary23$Beta, res_pi_binary23$SE)
tbl <- res_pi_binary23 %>% select(Group, Beta_SE)
save_table(tbl, "Math ~ PI_binary by country/region (2023)", "reg_math_PI_binary_2023")


# Save coefficient tables for 5_correlations (2023)
cty_names <- setdiff(unique(res_ses_binary23$Group), c("Europe", "Balkans"))
coef_2023 <- data.frame(
  Country = cty_names,
  year = 2023,
  beta_SES_binary = res_ses_binary23$Beta[match(cty_names, res_ses_binary23$Group)],
  beta_PI_index = res_pi_binary23$Beta[match(cty_names, res_pi_binary23$Group)]
)
write.csv(coef_2023, file.path(out_dir, "reg_coefficients_2023.csv"), row.names = FALSE)

print("\n--- 2023: Math ~ SES_index ---\n")
print(res_parent_edu_binary23 %>% filter(Group == "Europe"))
print(res_parent_edu_binary23 %>% filter(Group == "Balkans"))
print("\n--- 2023: PI_binary ~ SES_binary (Balkans) ---\n")
print(res_pi_ses23 %>% filter(Group == "Balkans"))


###################################
## ---- Regressions for 2024 (cross-section only; no interaction with 2019/2023) ---- ##
###################################
if (any(master_refactored$year == 2024)) {
  dat24 <- master_refactored %>% filter(year == 2024, CountryName != "Netherlands")
  dat24$IDCNTRY <- as.numeric(dat24$IDCNTRY)
  design24 <- build_design(dat24)
  print_binary_by_country(design24, "parent_edu_binary", "2024")

  res_parent_edu_binary24 <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), Sig = character(), Beta_std = numeric(), stringsAsFactors = FALSE)
  for (gname in get_group_names(design24)) {
    dg <- design_subset(design24, gname)
    if (nrow(dg$variables) < 20) next
    r <- run_pv_regression(dg, pv_math, "parent_edu_binary")
    v <- dg$variables
    if (all(pv_math %in% names(v))) {
      math_avg <- rowMeans(v[, pv_math], na.rm = TRUE)
      sd_math <- wtd_sd(math_avg, v$TOTWGT)
      beta_std <- if (!is.na(sd_math) && sd_math > 0 && !is.na(r$beta)) r$beta / sd_math else NA_real_
    } else beta_std <- NA_real_
    sig <- if (is.na(r$pval)) "" else if (r$pval < 0.01) "***" else if (r$pval < 0.05) "**" else if (r$pval < 0.1) "*" else ""
    res_parent_edu_binary24 <- rbind(res_parent_edu_binary24, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval, Sig = sig, Beta_std = beta_std))
  }
  res_parent_edu_binary24$Beta_SE <- sprintf("%.3f%s (%.3f)", res_parent_edu_binary24$Beta, res_parent_edu_binary24$Sig, res_parent_edu_binary24$SE)
  save_table(res_parent_edu_binary24 %>% select(Group, Beta, Beta_SE, Beta_std), "Math ~ parent_edu_binary by country/region (2024)", "reg_math_parent_edu_binary_2024")

  res_pi_ses24 <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), Sig = character(), stringsAsFactors = FALSE)
  for (gname in get_group_names(design24)) {
    dg <- design_subset(design24, gname)
    if (nrow(dg$variables) < 20) next
    r <- run_single_regression(dg, "PI_index", "parent_edu_binary")
    sig <- if (is.na(r$pval)) "" else if (r$pval < 0.01) "***" else if (r$pval < 0.05) "**" else if (r$pval < 0.1) "*" else ""
    res_pi_ses24 <- rbind(res_pi_ses24, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval, Sig = sig, stringsAsFactors = FALSE))
  }
  res_pi_ses24$Beta_SE <- sprintf("%.3f%s (%.3f)", res_pi_ses24$Beta, res_pi_ses24$Sig, res_pi_ses24$SE)
  save_table(res_pi_ses24 %>% select(Group, Beta, Beta_SE), "Parental Investment Index by Parental Education (Regression, 2024)", "reg_pi_index_parent_edu_binary_2024")
  save_table(res_parent_edu_binary24 %>% filter(Group %in% c("Europe", "Balkans")) %>% select(Group, Beta, Beta_SE, Beta_std), "Math ~ parent_edu_binary by region (2024)", "reg_math_parent_edu_binary_regional_2024")
  save_table(res_pi_ses24 %>% filter(Group %in% c("Europe", "Balkans")) %>% select(Group, Beta, Beta_SE), "PI_index ~ parent_edu_binary by region (2024)", "reg_pi_index_parent_edu_binary_regional_2024")

  res_ses_binary24 <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), stringsAsFactors = FALSE)
  for (gname in get_group_names(design24)) {
    dg <- design_subset(design24, gname)
    if (nrow(dg$variables) < 20) next
    r <- run_pv_regression(dg, pv_math, "SES_binary")
    res_ses_binary24 <- rbind(res_ses_binary24, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval))
  }
  res_ses_binary24$Beta_SE <- sprintf("%.3f (%.3f)", res_ses_binary24$Beta, res_ses_binary24$SE)
  save_table(res_ses_binary24 %>% select(Group, Beta_SE), "Math ~ SES_binary by country/region (2024)", "reg_math_ses_binary_2024")

  res_pi_binary24 <- data.frame(Group = character(), Beta = numeric(), SE = numeric(), pval = numeric(), stringsAsFactors = FALSE)
  for (gname in get_group_names(design24)) {
    dg <- design_subset(design24, gname)
    if (nrow(dg$variables) < 20) next
    r <- run_pv_regression(dg, pv_math, "PI_binary")
    res_pi_binary24 <- rbind(res_pi_binary24, data.frame(Group = gname, Beta = r$beta, SE = r$se, pval = r$pval))
  }
  res_pi_binary24$Beta_SE <- sprintf("%.3f (%.3f)", res_pi_binary24$Beta, res_pi_binary24$SE)
  save_table(res_pi_binary24 %>% select(Group, Beta_SE), "Math ~ PI_binary by country/region (2024)", "reg_math_PI_binary_2024")

  cty24 <- setdiff(unique(res_ses_binary24$Group), c("Europe", "Balkans"))
  coef_2024 <- data.frame(
    Country = cty24,
    year = 2024,
    beta_SES_binary = res_ses_binary24$Beta[match(cty24, res_ses_binary24$Group)],
    beta_PI_index = res_pi_binary24$Beta[match(cty24, res_pi_binary24$Group)]
  )
  write.csv(coef_2024, file.path(out_dir, "reg_coefficients_2024.csv"), row.names = FALSE)

  print("\n--- 2024: Math ~ SES_index ---\n")
  print(res_parent_edu_binary24 %>% filter(Group == "Europe"))
  print(res_parent_edu_binary24 %>% filter(Group == "Balkans"))
  print("\n--- 2024: PI_index ~ SES_binary (Balkans) ---\n")
  print(res_pi_ses24 %>% filter(Group == "Balkans"))

  pi_diff_2024 <- test_region_diff(res_pi_ses24, "2024")
  cat(sprintf("  2024: Beta Europe = %.3f, Beta Balkans = %.3f, diff = %.3f, SE(diff) = %.3f, z = %.3f, p = %.4f\n",
              pi_diff_2024$Beta_Europe, pi_diff_2024$Beta_Balkans, pi_diff_2024$diff, pi_diff_2024$SE_diff, pi_diff_2024$z, pi_diff_2024$p))
  pi_region_test <- rbind(pi_region_test,
    data.frame(Year = 2024, Beta_Europe = pi_diff_2024$Beta_Europe, Beta_Balkans = pi_diff_2024$Beta_Balkans,
               diff_Balkans_minus_Europe = pi_diff_2024$diff, SE_diff = pi_diff_2024$SE_diff, z = pi_diff_2024$z, p_value = pi_diff_2024$p))
  write.csv(pi_region_test, file.path(out_dir, "reg_PI_gradient_Europe_vs_Balkans_test.csv"), row.names = FALSE)
  message("Saved 2024 regressions and updated reg_PI_gradient_Europe_vs_Balkans_test.csv")
}


###################################
## Interaction: year dummy + edu interaction (full data, no year filter)
## Tests whether the coefficient change 2019->2023 is significant
###################################

plots_dir <- "output/figures"
dir.create(plots_dir, showWarnings = FALSE)
# Allow subset designs that create single-PSU strata (e.g. PI_index complete cases)
op <- options(survey.lonely.psu = "adjust")
on.exit(options(op), add = TRUE)

# Full data: both years, Netherlands excluded
dat_full <- master_refactored %>% filter(CountryName != "Netherlands")
dat_full$year_2023 <- as.integer(dat_full$year == 2023)
dat_full$edu_year <- dat_full$parent_edu_binary * dat_full$year_2023
design_full <- build_design(dat_full)

# Math ~ parent_edu_binary + year_2023 + edu_year (by region)
pv_math <- paste0("ASMMAT", sprintf("%02d", 1:5), "_ptile_region")
run_pv_regression_interaction <- function(design, dep_vars, indep_rhs) {
  pred_vars <- c("parent_edu_binary", "year_2023", "edu_year")
  est_beta <- est_se <- matrix(NA, 5, length(pred_vars))
  for (j in 1:5) {
    use <- complete.cases(design$variables[, c(dep_vars[j], pred_vars)])
    for (p in pred_vars) if (p %in% names(design$variables)) use <- use & !(design$variables[[p]] %in% c(998, 999))
    dsub <- design[use, ]
    if (nrow(dsub$variables) < 30) stop("Too few rows for interaction PV regression: ", dep_vars[j])
    fit <- suppressWarnings(svyglm(as.formula(paste(dep_vars[j], "~", indep_rhs)), design = dsub))
    cj <- broom::tidy(fit) %>% filter(.data$term != "(Intercept)")
    out <- list(b = setNames(cj$estimate, cj$term), s = setNames(cj$std.error, cj$term))
    for (i in seq_along(pred_vars)) {
      nm <- pred_vars[i]
      if (nm %in% names(out$b) && !is.na(out$b[nm])) {
        est_beta[j, i] <- out$b[nm]; est_se[j, i] <- out$s[nm]
      }
    }
  }
  beta <- colMeans(est_beta, na.rm = TRUE)
  se_tot <- sqrt(colMeans(est_se^2, na.rm = TRUE) + (1 + 1/5) * apply(est_beta, 2, function(x) var(x, na.rm = TRUE)))
  pval <- 2 * pnorm(-abs(beta / se_tot))
  list(beta = setNames(beta, pred_vars), se = setNames(se_tot, pred_vars), pval = setNames(pval, pred_vars), terms = pred_vars)
}

sig_star <- function(p) if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""

print_reg_table <- function(title, res) {
  cat("\n", title, "\n", paste(rep("-", nchar(title)), collapse = ""), "\n", sep = "")
  for (nm in res$terms) {
    b <- res$beta[nm]; s <- res$se[nm]; pv <- res$pval[nm]
    cat(sprintf("  %-20s %8.3f%s  (%6.3f)   p = %.4f\n", nm, b, sig_star(pv), s, pv))
  }
  cat("\n")
}

# Europe (non-Balkans): Math ~ parent_edu_binary + year_2023 + edu_year
dg_eu <- design_subset(design_full, "Europe")
res_math_eu <- run_pv_regression_interaction(dg_eu, pv_math, "parent_edu_binary + year_2023 + edu_year")
print_reg_table("Math ~ parent_edu + year_2023 + edu*year (Europe)", res_math_eu)

# Balkans: Math ~ parent_edu_binary + year_2023 + edu_year
dg_ba <- design_subset(design_full, "Balkans")
res_math_ba <- run_pv_regression_interaction(dg_ba, pv_math, "parent_edu_binary + year_2023 + edu_year")
print_reg_table("Math ~ parent_edu + year_2023 + edu*year (Balkans)", res_math_ba)

# PI_index ~ parent_edu_binary + year_2023 + edu_year (Europe, Balkans)
run_single_regression_interaction <- function(design, dep_var, indep_rhs) {
  pred_vars <- all.vars(as.formula(paste0("~", indep_rhs)))
  need <- c(dep_var, pred_vars)
  if (!all(need %in% names(design$variables))) {
    stop("Missing vars in design: ", paste(setdiff(need, names(design$variables)), collapse = ", "))
  }
  v <- design$variables
  use <- complete.cases(v[, need])
  use <- use & !(v[[dep_var]] %in% c(998, 999))
  for (pn in pred_vars) use <- use & !(v[[pn]] %in% c(998, 999))
  dsub <- design[use, ]
  n_use <- nrow(dsub$variables)
  if (n_use < 30) {
    stop("Too few rows (n = ", n_use, ") for ", dep_var)
  }
  fit <- suppressWarnings(svyglm(as.formula(paste0(dep_var, " ~ ", indep_rhs)), design = dsub))
  cj <- broom::tidy(fit) %>% filter(.data$term != "(Intercept)")
  b <- setNames(cj$estimate, cj$term)
  s <- setNames(cj$std.error, cj$term)
  pv <- setNames(2 * pnorm(-abs(cj$estimate / cj$std.error)), cj$term)
  list(beta = b, se = s, pval = pv, terms = cj$term)
}

res_pi_eu <- run_single_regression_interaction(dg_eu, "PI_index", "parent_edu_binary + year_2023 + edu_year")
print_reg_table("PI_index ~ parent_edu + year_2023 + edu*year (Europe)", res_pi_eu)

res_pi_ba <- run_single_regression_interaction(dg_ba, "PI_index", "parent_edu_binary + year_2023 + edu_year")
print_reg_table("PI_index ~ parent_edu + year_2023 + edu*year (Balkans)", res_pi_ba)

# Save interaction results as table (use named access for PI results)
get_coef <- function(res, nm) if (nm %in% names(res$beta)) res$beta[nm] else NA
get_se   <- function(res, nm) if (nm %in% names(res$se)) res$se[nm] else NA
get_pval <- function(res, nm) if (nm %in% names(res$pval)) res$pval[nm] else NA
int_tbl <- data.frame(
  Model = c(rep("Math ~ parent_edu + year_2023 + edu*year", 2),
            rep("PI_index ~ parent_edu + year_2023 + edu*year", 2)),
  Region = c("Europe", "Balkans", "Europe", "Balkans"),
  edu_coef  = c(get_coef(res_math_eu, "parent_edu_binary"), get_coef(res_math_ba, "parent_edu_binary"),
                get_coef(res_pi_eu, "parent_edu_binary"), get_coef(res_pi_ba, "parent_edu_binary")),
  edu_se    = c(get_se(res_math_eu, "parent_edu_binary"), get_se(res_math_ba, "parent_edu_binary"),
                get_se(res_pi_eu, "parent_edu_binary"), get_se(res_pi_ba, "parent_edu_binary")),
  year_coef = c(get_coef(res_math_eu, "year_2023"), get_coef(res_math_ba, "year_2023"),
                get_coef(res_pi_eu, "year_2023"), get_coef(res_pi_ba, "year_2023")),
  year_se   = c(get_se(res_math_eu, "year_2023"), get_se(res_math_ba, "year_2023"),
                get_se(res_pi_eu, "year_2023"), get_se(res_pi_ba, "year_2023")),
  int_coef  = c(get_coef(res_math_eu, "edu_year"), get_coef(res_math_ba, "edu_year"),
                get_coef(res_pi_eu, "edu_year"), get_coef(res_pi_ba, "edu_year")),
  int_se    = c(get_se(res_math_eu, "edu_year"), get_se(res_math_ba, "edu_year"),
                get_se(res_pi_eu, "edu_year"), get_se(res_pi_ba, "edu_year")),
  int_pval  = c(get_pval(res_math_eu, "edu_year"), get_pval(res_math_ba, "edu_year"),
                get_pval(res_pi_eu, "edu_year"), get_pval(res_pi_ba, "edu_year"))
)
write.csv(int_tbl, file.path(out_dir, "reg_interaction_year_edu.csv"), row.names = FALSE)
message("Saved reg_interaction_year_edu.csv")

print("\n========== Done ==========\n")
