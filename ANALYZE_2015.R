# ==============================================================================
# ANALYZE_2015.R
# 
# Purpose: Analyze 2015 TIMSS data for Grade 4
#          - Extract mother education and max of two parents' education (ASDHEDUP)
#          - Re-scale to binary: 0 = lower education, 1 = higher education
#          - Run regressions to compute SES gaps
#          - Create scatter plots showing correlation between the two measures
#
# This script reuses regression functions from WB_ANALYSIS.R
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(haven)
library(survey)
library(Hmisc)
library(readxl)
library(writexl)
library(ggplot2)
library(ggrepel)

# ==============================================================================
# Reuse regression helpers from WB_ANALYSIS.R
# ==============================================================================
source("d:/CEU/policy lab/WB_TIMMS/WB_ANALYSIS.R")

# Weighted quantile helper for percentile bins
safe_wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & (w > 0)
  if (sum(ok) < 2) return(rep(NA_real_, length(probs)))
  Hmisc::wtd.quantile(x[ok], weights = w[ok], probs = probs, na.rm = TRUE)
}

# Set paths (align with BUILD_MASTER_TABLES.R)
script_dir <- getwd()
raw_data_candidates <- c(
  file.path(script_dir, "data", "raw_data"),
  file.path(script_dir, "WB_TIMMS", "data", "raw_data"),
  file.path(dirname(script_dir), "WB_TIMMS", "data", "raw_data"),
  "data/raw_data",
  "WB_TIMMS/data/raw_data"
)

output_candidates <- c(
  file.path(script_dir, "output"),
  file.path(script_dir, "WB_TIMMS", "output"),
  file.path(dirname(script_dir), "WB_TIMMS", "output"),
  "output",
  "WB_TIMMS/output"
)

raw_data_dir <- raw_data_candidates[dir.exists(raw_data_candidates)][1]
output_dir <- output_candidates[dir.exists(output_candidates)][1]

if (is.na(raw_data_dir)) stop("Could not find raw data directory (data/raw_data).")
if (is.na(output_dir)) stop("Could not find output directory (output).")

tables_dir <- file.path(output_dir, "tables")
# Save plots to same folder as COMPARE_EDUCATION_MEASURES.R
plots_dir <- file.path(dirname(output_dir), "comparison_plots")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

cat("\n=== Building 2015 Master Table for Grade 4 ===\n")

# ==============================================================================
# Load 2015 Grade 4 raw data files (country-specific files like ASGJPNM6)
# ==============================================================================

standardize_names <- function(df) {
  names(df) <- toupper(names(df))
  df
}

load_idb_data <- function(file_base) {
  file_path <- file.path(raw_data_dir, paste0(file_base, ".Rdata"))
  if (!file.exists(file_path)) {
    file_path <- file.path(raw_data_dir, paste0(tolower(file_base), ".rdata"))
  }
  if (!file.exists(file_path)) {
    return(NULL)
  }
  env <- new.env()
  obj <- load(file_path, envir = env)
  df <- get(obj[1], envir = env)
  standardize_names(df)
}

drop_overlap_cols <- function(base_df, add_df, keys) {
  overlap <- intersect(names(base_df), names(add_df))
  overlap <- setdiff(overlap, keys)
  add_df %>% dplyr::select(-dplyr::any_of(overlap))
}

cycle <- "M6"
prefix <- "AS"
ach_pattern <- sprintf("^ASA...%s\\.rdata$", cycle)
ach_files <- list.files(raw_data_dir, pattern = ach_pattern, full.names = FALSE, ignore.case = TRUE)

if (length(ach_files) == 0) {
  stop("No 2015 (M6) achievement files found in: ", raw_data_dir)
}

pv_math <- sprintf("%sMMAT%02d", prefix, 1:5)
pv_sci <- sprintf("%sSSCI%02d", prefix, 1:5)
pv_reason_math <- sprintf("%sMREA%02d", prefix, 1:5)
pv_reason_sci <- sprintf("%sSREA%02d", prefix, 1:5)

ach_keep <- unique(c(
  "IDCNTRY", "IDSCHOOL", "IDSTRATE", "IDSTUD", "TOTWGT", "JKZONE", "JKREP",
  pv_math, pv_sci, pv_reason_math, pv_reason_sci
))
bg_keep <- unique(c(
  "IDCNTRY", "IDSTUD",
  sprintf("%sDHEDUP", prefix),
  sprintf("%sDGEDUP", prefix),
  # Mother's education for 2015 is ASBH20B
  sprintf("%sBH20A", prefix),
  sprintf("%sBH20B", prefix)
))

dfs <- list()
for (f in ach_files) {
  ach_base <- toupper(sub("\\.rdata$", "", f, ignore.case = TRUE))
  country_code <- substr(ach_base, 4, 6)
  ctx_base <- paste0("ASG", country_code, cycle)
  home_base <- paste0("ASH", country_code, cycle)

  ach_df <- load_idb_data(ach_base)
  ctx_df <- load_idb_data(ctx_base)
  home_df <- load_idb_data(home_base)

  if (is.null(ach_df)) next
  ach_df <- dplyr::select(ach_df, dplyr::any_of(ach_keep))
  if (!is.null(ctx_df)) ctx_df <- dplyr::select(ctx_df, dplyr::any_of(bg_keep))
  if (!is.null(home_df)) home_df <- dplyr::select(home_df, dplyr::any_of(bg_keep))

  merge_keys <- intersect(c("IDCNTRY", "IDSTUD"), names(ach_df))
  merged_df <- ach_df
  if (!is.null(ctx_df) && length(merge_keys) > 0) {
    ctx_df <- drop_overlap_cols(merged_df, ctx_df, merge_keys)
    merged_df <- dplyr::left_join(merged_df, ctx_df, by = merge_keys)
  }
  if (!is.null(home_df) && length(merge_keys) > 0) {
    home_df <- drop_overlap_cols(merged_df, home_df, merge_keys)
    merged_df <- dplyr::left_join(merged_df, home_df, by = merge_keys)
  }

  dfs[[length(dfs) + 1]] <- merged_df
}

if (length(dfs) == 0) {
  stop("No student data loaded for 2015 (M6).")
}

df <- bind_rows(dfs)
cat("Loaded and merged data:", nrow(df), "rows\n")

# ==============================================================================
# Extract and recode education variables
# ==============================================================================

# TIMSS 2015 Education Variables:
# - Mother education: ASBH20B (from Home questionnaire)
# - Max of two parents education: ASDHEDUP (derived variable)

cat("\n=== Extracting Education Variables ===\n")

# Mother education for 2015 is ASBH20B
if ("ASBH20B" %in% names(df)) {
  df$mother_edu <- df$ASBH20B
  cat("Mother education variable: ASBH20B\n")
} else {
  stop("Mother education variable ASBH20B not found in 2015 data")
}

# Max of two parents
if ("ASDHEDUP" %in% names(df)) {
  df$max_parents_edu <- df$ASDHEDUP
  cat("Max of two parents education variable: ASDHEDUP\n")
} else {
  stop("ASDHEDUP variable not found in 2015 data")
}

# Show raw value distributions to verify coding
cat("\nRaw mother_edu distribution:\n")
print(table(df$mother_edu, useNA = "ifany"))
cat("\nRaw max_parents_edu distribution:\n")
print(table(df$max_parents_edu, useNA = "ifany"))

# Recode to binary (0 = lower, 1 = higher)
# 
# ASBH20B (Mother's education - ASCENDING scale):
#   1=Did not go to school, 2=Some Primary, 3=Lower secondary, 4=Upper secondary,
#   5=Post-secondary non-tertiary, 6=Short-cycle tertiary, 7=Bachelor's, 8=Postgraduate, 9=NA
#   => Higher = 4+ (upper secondary or more)
#
# ASDHEDUP (max of two parents - DESCENDING scale):
#   1=University or Higher, 2=Post-secondary but not University, 3=Upper Secondary,
#   4=Lower Secondary, 5=Some Primary/Lower Secondary/No School, 6=Not Applicable
#   => Higher = 1, 2, or 3 (upper secondary or more)
#

# Mother education: ASCENDING scale (ASBH20B)
df <- df %>%
  mutate(
    mother_high_edu = case_when(
      is.na(mother_edu) ~ NA_real_,
      mother_edu == 9 ~ NA_real_,
      mother_edu >= 4 ~ 1,
      mother_edu < 4 ~ 0,
      TRUE ~ NA_real_
    )
  )
cat("Mother education (ASBH20B): >= 4 is higher education\n")

# Max of two parents: DESCENDING scale (ASDHEDUP)
df <- df %>%
  mutate(
    max_parents_high_edu = case_when(
      is.na(max_parents_edu) ~ NA_real_,
      max_parents_edu == 6 ~ NA_real_,
      max_parents_edu <= 3 ~ 1,
      max_parents_edu > 3 ~ 0,
      TRUE ~ NA_real_
    )
  )
cat("Max parents education (ASDHEDUP): <= 3 is higher education\n")

cat("Mother high edu: ", sum(df$mother_high_edu == 1, na.rm = TRUE), "higher, ", 
    sum(df$mother_high_edu == 0, na.rm = TRUE), "lower, ",
    sum(is.na(df$mother_high_edu)), "NA\n")
cat("Max parents high edu: ", sum(df$max_parents_high_edu == 1, na.rm = TRUE), "higher, ", 
    sum(df$max_parents_high_edu == 0, na.rm = TRUE), "lower, ",
    sum(is.na(df$max_parents_high_edu)), "NA\n")

# Check availability per country
cat("\n=== Education Variable Availability per Country ===\n")
country_edu_summary <- df %>%
  group_by(IDCNTRY) %>%
  summarise(
    n = n(),
    mother_valid = sum(!is.na(mother_high_edu)),
    max_parents_valid = sum(!is.na(max_parents_high_edu)),
    .groups = "drop"
  )
print(country_edu_summary)

# ==============================================================================
# Filter to European and Balkan countries
# ==============================================================================

# Use the same country list as in BUILD_MASTER_TABLES.R
european_countries <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR", "NOR", "CHE",
  "ISL", "MKD", "SRB", "MNE", "BIH", "ALB", "XKX"
)

# Same membership as 1_build_master_table.R Is_balkan (Western Balkans six)
balkan_countries <- c("ALB", "BIH", "MKD", "MNE", "SRB", "XKX")

# Load country codes
labels <- attr(df$IDCNTRY, "labels")
idcntry_labels <- NULL
if (!is.null(labels)) {
  idcntry_labels <- tibble(
    IDCNTRY = as.numeric(unname(labels)),
    CountryName = as.character(names(labels))
  )
}

unsd_candidates <- c(
  "data/processed_data/UNSD_codes.xlsx",
  "WB_TIMMS/data/processed_data/UNSD_codes.xlsx",
  "WB_TIMMS/UNSD_codes.xlsx"
)
unsd_file <- unsd_candidates[file.exists(unsd_candidates)][1]
if (!is.na(unsd_file)) {
  unsd_raw <- readxl::read_excel(unsd_file)
  name_col <- intersect(c("Country or Area", "CountryName", "Country"), names(unsd_raw))[1]
  if (is.na(name_col)) {
    stop("UNSD_codes.xlsx is missing a country name column.")
  }
  country_codes <- unsd_raw %>%
    rename(CountryName = !!name_col) %>%
    select(IDCNTRY, ISO3 = `ISO-alpha3 Code`, CountryName) %>%
    distinct(IDCNTRY, .keep_all = TRUE)
  
  df <- df %>%
    left_join(country_codes, by = "IDCNTRY") %>%
    left_join(idcntry_labels, by = "IDCNTRY", suffix = c("", "_labels")) %>%
    mutate(CountryName = coalesce(CountryName, CountryName_labels)) %>%
    filter(!is.na(ISO3) & ISO3 %in% european_countries) %>%
    mutate(Is_Balkan = ISO3 %in% balkan_countries)
  
  cat("Filtered to", length(unique(df$IDCNTRY)), "European/Balkan countries\n")
  country_list <- df %>%
    distinct(ISO3, CountryName) %>%
    mutate(country_label = ifelse(!is.na(CountryName), CountryName, ISO3)) %>%
    arrange(country_label) %>%
    pull(country_label)
  cat("Countries:", paste(country_list, collapse = ", "), "\n")
} else {
  warning("UNSD_codes.xlsx not found, skipping country filtering")
  if (!is.null(idcntry_labels)) {
    df <- df %>%
      left_join(idcntry_labels, by = "IDCNTRY")
  }
}

# ==============================================================================
# Calculate percentiles for achievement (region-level) - ALL 4 OUTCOMES
# ==============================================================================

cat("\n=== Calculating Achievement Percentiles ===\n")

# Define all 4 outcome types (same as WB_ANALYSIS.R)
outcomes <- list(
  math = list(prefix = "ASMMAT0", label = "Math"),
  science = list(prefix = "ASSSCI0", label = "Science"),
  math_reasoning = list(prefix = "ASMREA0", label = "Math Reasoning"),
  science_reasoning = list(prefix = "ASSREA0", label = "Science Reasoning")
)

# Calculate region-level percentiles for each outcome
probs <- seq(0, 1, 0.01)
all_pv_cols <- c()
all_ptile_cols <- c()

for (outcome_name in names(outcomes)) {
  prefix <- outcomes[[outcome_name]]$prefix
  label <- outcomes[[outcome_name]]$label
  
  pv_cols <- paste0(prefix, 1:5)
  
  # Check if this outcome exists in the data
  if (!all(pv_cols %in% names(df))) {
    cat("  Skipping", label, "- not available in 2015 data\n")
    next
  }
  
  cat("  Processing", label, "...\n")
  
  for (i in 1:5) {
    pv_col <- pv_cols[i]
    ptile_col <- paste0(pv_col, "_ptile_region")
    
    q <- safe_wtd_quantile(df[[pv_col]], df$TOTWGT, probs)
    q <- unique(q)
    df[[ptile_col]] <- if (length(q) < 2) NA_integer_ else cut(df[[pv_col]], breaks = q, labels = FALSE, include.lowest = TRUE)
    
    all_pv_cols <- c(all_pv_cols, pv_col)
    all_ptile_cols <- c(all_ptile_cols, ptile_col)
  }
  cat("    Calculated percentiles for", label, "\n")
}

# ==============================================================================
# Keep only necessary columns for regression
# ==============================================================================

keep_cols <- c(
  "IDCNTRY", "IDSCHOOL", "IDCLASS", "IDSTUD", "TOTWGT",
  "JKZONE", "JKREP",
  all_pv_cols, all_ptile_cols,
  "mother_high_edu", "max_parents_high_edu",
  "ISO3", "CountryName", "Is_Balkan"
)

df_master <- df %>%
  select(any_of(keep_cols)) %>%
  mutate(year = 2015)

cat("\nMaster table created:", nrow(df_master), "rows,", ncol(df_master), "columns\n")

# Save the 2015 master table
master_2015_path <- file.path(dirname(output_dir), "data", "processed_data", "master_g4_2015.Rdata")
dir.create(dirname(master_2015_path), showWarnings = FALSE, recursive = TRUE)
save(df_master, file = master_2015_path)
cat("Saved 2015 master table to:", master_2015_path, "\n")

# ==============================================================================
# Run regressions using functions from WB_ANALYSIS.R
# ==============================================================================

cat("\n=== Running Regressions ===\n")

# Create country lookup
country_lookup <- df_master %>%
  distinct(IDCNTRY, CountryName, ISO3, Is_Balkan)

# Define independent variables
indep_vars_2015 <- c("mother_high_edu", "max_parents_high_edu")
indep_labels_2015 <- c(
  "mother_high_edu" = "MotherHighEdu",
  "max_parents_high_edu" = "MaxParentsHighEdu"
)

# Run regressions per country for ALL outcomes
all_results <- list()

for (outcome_name in names(outcomes)) {
  prefix <- outcomes[[outcome_name]]$prefix
  label <- outcomes[[outcome_name]]$label
  
  pv_region <- paste0(prefix, 1:5, "_ptile_region")
  
  # Check if percentiles exist for this outcome
  if (!all(pv_region %in% names(df_master))) {
    cat("\nSkipping", label, "- percentiles not available\n")
    next
  }
  
  cat("\n--- Processing", label, "---\n")
  
  for (indep_var in indep_vars_2015) {
    cat("  Regressing", indep_var, "on", label, "...\n")
    
    # Check how many countries have valid data for this variable
    valid_counts <- df_master %>%
      group_by(IDCNTRY) %>%
      summarise(n_valid = sum(!is.na(.data[[indep_var]])), .groups = "drop")
    cat("    Countries with sufficient data (>=10):", sum(valid_counts$n_valid >= 10), "\n")
    
    # Run regression for each country
    reg_results <- df_master %>%
      group_by(IDCNTRY) %>%
      group_modify(~ {
        country_data <- .x

        # Check if we have the required columns and sufficient data
        if (!indep_var %in% names(country_data) || 
            !all(pv_region %in% names(country_data)) ||
            sum(!is.na(country_data[[indep_var]])) < 10) {
          return(tibble(beta = NA_real_, se_total = NA_real_, parameter = indep_var))
        }

        pv_results <- run_pv_regressions(country_data, indep_var, pv_region, run_regression_jk)
        result <- combine_pv_results(pv_results, indep_var)

        if (is.null(result) || nrow(result) == 0) {
          return(tibble(beta = NA_real_, se_total = NA_real_, parameter = indep_var))
        }

        result
      }) %>%
      ungroup() %>%
      left_join(country_lookup, by = "IDCNTRY")
    
    # Report how many succeeded
    n_success <- sum(!is.na(reg_results$beta))
    cat("    Successful regressions:", n_success, "out of", nrow(reg_results), "\n")
    
    all_results[[paste0(outcome_name, "_", indep_var)]] <- reg_results %>%
      mutate(indep_var = indep_var, outcome = outcome_name, year = 2015)
  }
}

# Combine all results
all_results <- bind_rows(all_results)

cat("\nRegressions complete:", nrow(all_results), "country-outcome-variable combinations\n")

# ==============================================================================
# Save regression results - one file per outcome
# ==============================================================================

cat("\n=== Saving Results ===\n")

# Save a summary file per outcome
for (outcome_name in unique(all_results$outcome)) {
  outcome_label <- outcomes[[outcome_name]]$label
  
  summary_df <- all_results %>%
    filter(outcome == outcome_name) %>%
    mutate(
      indep_label = indep_labels_2015[indep_var],
      cell = mapply(function(b, s) {
        if (is.na(b) || is.na(s)) return("")
        sprintf("%.3f (%.3f)", b, s)
      }, beta, se_total)
    ) %>%
    select(CountryName, indep_label, cell) %>%
    tidyr::pivot_wider(names_from = indep_label, values_from = cell)
  
  filename <- paste0("regression_results_2015_", outcome_name, ".xlsx")
  writexl::write_xlsx(summary_df, path = file.path(tables_dir, filename))
  cat("  Saved:", filename, "\n")
}

# Also save a combined file
writexl::write_xlsx(
  all_results %>% select(CountryName, outcome, indep_var, beta, se_total),
  path = file.path(tables_dir, "regression_results_2015_all_outcomes.xlsx")
)
cat("  Saved: regression_results_2015_all_outcomes.xlsx\n")

# ==============================================================================
# Create scatter plots comparing the two measures
# ==============================================================================

cat("\n=== Creating Scatter Plots ===\n")

# Overall plot (all outcomes combined)
plot_data <- all_results %>%
  select(CountryName, outcome, indep_var, beta) %>%
  pivot_wider(names_from = indep_var, values_from = beta) %>%
  filter(!is.na(mother_high_edu) & !is.na(max_parents_high_edu))

if (nrow(plot_data) == 0) {
  warning("No complete data for scatter plot")
} else {
  # Overall correlation
  correlation <- cor(plot_data$mother_high_edu, plot_data$max_parents_high_edu)
  n_total <- nrow(plot_data)
  cat("Overall correlation between MotherHighEdu and MaxParentsHighEdu:", round(correlation, 3), "\n")
  
  # Overall scatter plot (all outcomes for 2015)
  p <- ggplot(plot_data, aes(x = mother_high_edu, y = max_parents_high_edu, color = outcome)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
    labs(
      title = "SES Gaps: Mother's Education vs Max of Two Parents",
      subtitle = paste0("Year: 2015 | All Outcomes | Correlation: ", round(correlation, 3), " | N = ", n_total),
      x = "Beta Coefficient (Mother's High Education)",
      y = "Beta Coefficient (Max of Two Parents' High Education)",
      color = "Outcome"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12)
    )
  
  ggsave(
    filename = file.path(plots_dir, "mother_vs_maxoftwo_2015.png"),
    plot = p, width = 10, height = 8, dpi = 300
  )
  cat("  Saved: mother_vs_maxoftwo_2015.png\n")
  
  # Per-outcome scatter plots (2015 + specific outcome)
  for (outcome_name in unique(plot_data$outcome)) {
    outcome_label <- outcomes[[outcome_name]]$label
    outcome_data <- plot_data %>% filter(outcome == outcome_name)
    
    if (nrow(outcome_data) < 3) next
    
    corr <- cor(outcome_data$mother_high_edu, outcome_data$max_parents_high_edu)
    n_countries <- nrow(outcome_data)
    
    p <- ggplot(outcome_data, aes(x = mother_high_edu, y = max_parents_high_edu)) +
      geom_point(size = 3, alpha = 0.7, color = "#2E86AB") +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
      ggrepel::geom_text_repel(aes(label = CountryName), size = 2.5, max.overlaps = 25) +
      labs(
        title = "SES Gaps: Mother's Education vs Max of Two Parents",
        subtitle = paste0("Year: 2015 | Outcome: ", outcome_label, " | Correlation: ", round(corr, 3), " | N = ", n_countries, " countries"),
        x = "Beta Coefficient (Mother's High Education)",
        y = "Beta Coefficient (Max of Two Parents' High Education)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)
      )
    
    ggsave(
      filename = file.path(plots_dir, paste0("mother_vs_maxoftwo_2015_", outcome_name, ".png")),
      plot = p, width = 10, height = 8, dpi = 300
    )
    cat("  Saved: mother_vs_maxoftwo_2015_", outcome_name, ".png (corr:", round(corr, 3), ")\n")
  }
}

cat("\n=== 2015 Analysis Complete ===\n")
