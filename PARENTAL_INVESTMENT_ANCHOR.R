#!/usr/bin/env Rscript

# =============================================================================
# PARENTAL_INVESTMENT_ANCHOR.R
# 
# Goal:
# - Use the Grade 4 master table (2019 & 2023, Europe/Balkans)
# - Compute country means for the 18 parental investment items ASBH01A–ASBH01R
# - For each item, measure how much its mean varies across countries
#   (lower variation = better anchor candidate)
# - Output:
#   * Country-by-item means table
#   * Summary table with a stability index per item and suggested anchor
# - Outputs are written to: output/parental_investment_country_means
#
# This script does NOT modify the master table. It only reads it.
# After inspecting the summary, you can manually choose the anchor item
# and set it in WB_ANALYSIS.R.
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("Package 'writexl' is required. Please install it with install.packages('writexl').")
}

master_dir <- "data/processed_data/master"
output_dir <- "output/parental_investment_country_means"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

master_rdata_g4 <- file.path(master_dir, "master_table_grade_4.RData")
if (!file.exists(master_rdata_g4)) {
  stop("Missing master file: ", master_rdata_g4, 
       ". Please run BUILD_MASTER_TABLES.R first for grade 4.")
}

cat("Loading master_table_grade_4 from:", master_rdata_g4, "\n")
load(master_rdata_g4)  # should create object master_table_grade_4
if (!exists("master_table_grade_4")) {
  stop("Object 'master_table_grade_4' not found in ", master_rdata_g4)
}

df <- master_table_grade_4

if (!"year" %in% names(df)) {
  stop("Master table does not contain a 'year' column. Please rebuild with BUILD_MASTER_TABLES.R.")
}

if (!"CountryName" %in% names(df)) {
  warning("Column 'CountryName' not found in master table. Using IDCNTRY as fallback name.")
  df$CountryName <- as.character(df$IDCNTRY)
}

# Restrict to years of interest (should already be only 2019/2023, but be explicit)
df <- df %>% filter(year %in% c(2019, 2023))

parental_items <- sprintf("ASBH01%s", LETTERS[1:18])  # ASBH01A..ASBH01R
parental_items <- parental_items[parental_items %in% names(df)]

if (length(parental_items) == 0) {
  stop("None of the parental investment items ASBH01A–ASBH01R are present in the master table.")
}

cat("Parental investment items found in master table:\n")
print(parental_items)

# ---------------------------------------------------------------------------
# 0. Check actual coding schema
# ---------------------------------------------------------------------------
cat("\n=== Checking Actual Variable Coding ===\n")
# Sample a few items to check their unique values
sample_items <- parental_items[1:min(3, length(parental_items))]
for (item in sample_items) {
  if (item %in% names(df)) {
    unique_vals <- sort(unique(df[[item]]))
    cat(sprintf("\n%s unique values: ", item))
    cat(paste(unique_vals, collapse = ", "))
    cat(sprintf(" (N unique: %d)", length(unique_vals)))
    # Check for common missing codes
    missing_codes <- unique_vals[unique_vals %in% c(9, 99, 999, 9999) | is.na(unique_vals)]
    if (length(missing_codes) > 0) {
      cat(sprintf("\n  Missing codes found: %s", paste(missing_codes, collapse = ", ")))
    }
  }
}
cat("\n\nNote: After recoding in BUILD_MASTER_TABLES.R:\n")
cat("  1=Never (lowest investment), 2=Sometimes, 3=Often (highest investment)\n")
cat("  Original coding was inverted: 1=Often, 2=Sometimes, 3=Never\n")
cat("  9 or 99 = Omitted/invalid (kept as-is)\n")
cat("  NA or Sysmis = Not administered (kept as-is)\n\n")

# ---------------------------------------------------------------------------
# 0. Missing data analysis
# ---------------------------------------------------------------------------
cat("=== Missing Data Analysis ===\n")

# Overall missing patterns
n_total <- nrow(df)
missing_summary <- df %>%
  summarise(
    across(
      all_of(parental_items),
      ~ sum(is.na(.x) | .x %in% c(9, 99, 999, 9999, "A", ".A")),
      .names = "{.col}_missing"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "n_missing"
  ) %>%
  mutate(
    item = gsub("_missing$", "", item),
    pct_missing = 100 * n_missing / n_total,
    n_valid = n_total - n_missing,
    pct_valid = 100 - pct_missing
  ) %>%
  arrange(desc(n_missing))

cat("Missing data by item (all years, all countries):\n")
print(missing_summary)

# Missing patterns by year
missing_by_year <- df %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    across(
      all_of(parental_items),
      ~ sum(is.na(.x) | .x %in% c(9, 99, 999, 9999, "A", ".A")),
      .names = "{.col}_missing"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(year, n_total),
    names_to = "item",
    values_to = "n_missing"
  ) %>%
  mutate(
    item = gsub("_missing$", "", item),
    pct_missing = 100 * n_missing / n_total
  )

# Complete case analysis
complete_cases <- df %>%
  select(all_of(parental_items)) %>%
  complete.cases()
n_complete <- sum(complete_cases)
n_partial <- n_total - n_complete

cat("\nComplete case analysis:\n")
cat(sprintf("  Total observations: %d\n", n_total))
cat(sprintf("  Complete cases (all %d items): %d (%.1f%%)\n", length(parental_items), n_complete, 100*n_complete/n_total))
cat(sprintf("  Partial cases (at least one missing): %d (%.1f%%)\n", n_partial, 100*n_partial/n_total))

# Missing patterns by country-year
missing_by_country_year <- df %>%
  group_by(year, IDCNTRY, CountryName) %>%
  summarise(
    n_students = n(),
    n_complete = sum(complete.cases(across(all_of(parental_items)))),
    pct_complete = 100 * n_complete / n_students,
    .groups = "drop"
  ) %>%
  arrange(year, desc(pct_complete))

cat("\nComplete case rates by country-year (first 10):\n")
print(head(missing_by_country_year, 10))

# Analysis: What if we exclude the item with most missing data?
max_missing_item <- missing_summary$item[1]
if (max_missing_item %in% parental_items) {
  items_without_max_missing <- setdiff(parental_items, max_missing_item)
  complete_cases_excluded <- df %>%
    select(all_of(items_without_max_missing)) %>%
    complete.cases()
  n_complete_excluded <- sum(complete_cases_excluded)
  n_partial_excluded <- n_total - n_complete_excluded
  
  cat("\n=== Sensitivity Analysis: Excluding ", max_missing_item, " ===\n", sep = "")
  cat(sprintf("Complete cases WITHOUT %s: %d (%.1f%%)\n", 
              max_missing_item, n_complete_excluded, 100*n_complete_excluded/n_total))
  cat(sprintf("Gain in complete cases: %d (%.1f percentage points)\n",
              n_complete_excluded - n_complete, 
              100*(n_complete_excluded - n_complete)/n_total))
  cat(sprintf("Items used: %d (excluding %s)\n", length(items_without_max_missing), max_missing_item))
  cat("\nTrade-off: Excluding ", max_missing_item, " increases complete cases but loses information from that item.\n", sep = "")
  cat("With pairwise deletion, observations missing ", max_missing_item, " can still contribute\n", sep = "")
  cat("to other variable pairs, so excluding it may not be necessary.\n")
}

# ---------------------------------------------------------------------------
# 1. Country-level means for each parental investment item
# ---------------------------------------------------------------------------

means_wide <- df %>%
  group_by(year, IDCNTRY, CountryName) %>%
  summarise(
    across(
      all_of(parental_items),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    n_students = n(),
    .groups = "drop"
  ) %>%
  arrange(year, CountryName)

# Long format for easier plotting/inspection if needed
means_long <- means_wide %>%
  pivot_longer(
    cols = all_of(parental_items),
    names_to = "item",
    values_to = "item_mean"
  )

# ---------------------------------------------------------------------------
# 2. Stability index per item: how similar are country means?
#
# Here we use:
# - grand_mean: average of country means (over all years & countries)
# - sd_country_mean: standard deviation of country means
# - iqr_country_mean: IQR of country means (robust to outliers)
# Lower sd / IQR -> more stable across countries -> better anchor candidate.
# ---------------------------------------------------------------------------

stability_summary <- means_long %>%
  group_by(item) %>%
  summarise(
    n_country_year = sum(!is.na(item_mean)),
    grand_mean = mean(item_mean, na.rm = TRUE),
    sd_country_mean = sd(item_mean, na.rm = TRUE),
    iqr_country_mean = IQR(item_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sd_country_mean)

anchor_candidate <- stability_summary$item[1]

cat("\nSuggested anchor item based on smallest SD of country means:\n")
print(anchor_candidate)

cat("\nStability summary (first few rows):\n")
print(head(stability_summary, 10))

# ---------------------------------------------------------------------------
# 3. Factor loadings analysis (if lavaan is available)
# ---------------------------------------------------------------------------
if (requireNamespace("lavaan", quietly = TRUE)) {
  cat("\n=== Factor Loadings Analysis ===\n")
  cat("Fitting CFA model to examine which items are most relevant to the latent factor...\n\n")
  
  library(lavaan)
  
  # Fit model for each year separately to see consistency
  factor_loadings_all <- list()
  
  for (yr in c(2019, 2023)) {
    df_year <- df %>% filter(year == yr)
    if (nrow(df_year) == 0) next
    
    # Check which items are available
    items_available <- parental_items[parental_items %in% names(df_year)]
    if (length(items_available) < 3) {
      cat(sprintf("Skipping year %d: insufficient items (%d)\n", yr, length(items_available)))
      next
    }
    
    # Use suggested anchor or first available item
    anchor <- if (anchor_candidate %in% items_available) anchor_candidate else items_available[1]
    other_items <- setdiff(items_available, anchor)
    
    if (length(other_items) == 0) {
      cat(sprintf("Skipping year %d: only anchor item available\n", yr))
      next
    }
    
    # Build model
    PI_model <- paste0(
      "I =~ 1*", anchor,
      if (length(other_items) > 0) paste0(" + ", paste(other_items, collapse = " + ")) else "",
      "\n", anchor, " ~ 0*1\n"
    )
    
    # Fit model
    fit <- tryCatch(
      cfa(PI_model, data = df_year, ordered = items_available, std.lv = FALSE),
      error = function(e) {
        cat(sprintf("Model fitting failed for year %d: %s\n", yr, e$message))
        NULL
      }
    )
    
    if (!is.null(fit)) {
      # Extract standardized loadings
      loadings <- tryCatch({
        std_sol <- standardizedSolution(fit)
        std_sol %>%
          filter(op == "=~") %>%
          select(lhs, rhs, est.std) %>%
          rename(factor = lhs, item = rhs, loading = est.std) %>%
          arrange(desc(abs(loading)))
      }, error = function(e) NULL)
      
      if (!is.null(loadings) && nrow(loadings) > 0) {
        loadings$year <- yr
        factor_loadings_all[[as.character(yr)]] <- loadings
        
        cat(sprintf("\nYear %d - Factor Loadings (standardized):\n", yr))
        print(loadings %>% select(item, loading) %>% arrange(desc(abs(loading))))
        
        # Summary statistics
        cat(sprintf("\n  Mean absolute loading: %.3f\n", mean(abs(loadings$loading))))
        cat(sprintf("  Min loading: %.3f (%s)\n", 
                    min(loadings$loading), 
                    loadings$item[which.min(loadings$loading)]))
        cat(sprintf("  Max loading: %.3f (%s)\n", 
                    max(loadings$loading), 
                    loadings$item[which.max(loadings$loading)]))
        cat(sprintf("  Items with loading > 0.5: %d\n", sum(abs(loadings$loading) > 0.5)))
        cat(sprintf("  Items with loading > 0.7: %d\n", sum(abs(loadings$loading) > 0.7)))
      }
    }
  }
  
  # Combine loadings across years if available
  if (length(factor_loadings_all) > 0) {
    loadings_combined <- bind_rows(factor_loadings_all)
    
    # Average loadings across years
    loadings_avg <- loadings_combined %>%
      group_by(item) %>%
      summarise(
        mean_loading = mean(loading, na.rm = TRUE),
        sd_loading = sd(loading, na.rm = TRUE),
        min_loading = min(loading, na.rm = TRUE),
        max_loading = max(loading, na.rm = TRUE),
        n_years = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(abs(mean_loading)))
    
    cat("\n=== Average Factor Loadings Across Years ===\n")
    print(loadings_avg)
    
    cat("\nInterpretation:\n")
    cat("  - Higher absolute loading = more relevant to the latent factor\n")
    cat("  - Loadings > 0.7 = strong relationship\n")
    cat("  - Loadings 0.5-0.7 = moderate relationship\n")
    cat("  - Loadings < 0.5 = weak relationship (consider excluding)\n")
    
    # Save to output
    factor_loadings_all[["average"]] <- loadings_avg
  } else {
    factor_loadings_all <- NULL
    cat("\nCould not extract factor loadings. Model may not have converged.\n")
  }
} else {
  cat("\n=== Factor Loadings Analysis ===\n")
  cat("lavaan package not available. Install with: install.packages('lavaan')\n")
  factor_loadings_all <- NULL
}

# ---------------------------------------------------------------------------
# 3. Write outputs
# ---------------------------------------------------------------------------

out_path <- file.path(output_dir, "parental_investment_country_means.xlsx")

# Prepare output list
output_list <- list(
  "missing_summary" = missing_summary,
  "missing_by_year" = missing_by_year,
  "missing_by_country_year" = missing_by_country_year,
  "country_means_wide" = means_wide,
  "country_means_long" = means_long,
  "stability_summary" = stability_summary
)

# Add factor loadings if available
if (!is.null(factor_loadings_all)) {
  for (name in names(factor_loadings_all)) {
    output_list[[paste0("factor_loadings_", name)]] <- factor_loadings_all[[name]]
  }
}

writexl::write_xlsx(output_list, out_path)

cat("\nCountry-level parental investment means and stability summary written to:\n")
cat("  ", out_path, "\n")
cat("Suggested anchor item:", anchor_candidate, "\n")

cat("\n=== Summary ===\n")
cat(sprintf("Total observations analyzed: %d\n", n_total))
cat(sprintf("Complete cases: %d (%.1f%%)\n", n_complete, 100*n_complete/n_total))
cat(sprintf("Partial cases: %d (%.1f%%)\n", n_partial, 100*n_partial/n_total))
cat(sprintf("Item with most missing data: %s (%d missing, %.1f%%)\n",
            missing_summary$item[1], missing_summary$n_missing[1], missing_summary$pct_missing[1]))
cat(sprintf("Suggested anchor (lowest SD across countries): %s\n", anchor_candidate))
cat("\nNote: The latent factor model uses pairwise deletion (WLSMV estimator)\n")
cat("      to handle missing data, so observations with partial data can still be included.\n")

