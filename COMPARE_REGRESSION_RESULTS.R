# ==============================================================================
# COMPARE_REGRESSION_RESULTS.R
# 
# Purpose: Compare regression results across different measures
#          (i) Education measures: parentB_high_edu vs ASDHEDUP_binary (max of two)
#          (ii) SES vs Parental Investment: SES_index vs PI_labels (3 labels: Low/Medium/High)
#          
# This script reads existing regression outputs and creates scatter plots
# showing correlations between beta coefficients from different measures.
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(ggplot2)
library(ggrepel)

# Set paths (ensure we're in the correct working directory)
script_dir <- getwd()
cat("Current working directory:", script_dir, "\n")

# Try multiple possible paths
possible_paths <- c(
  file.path(script_dir, "output", "tables"),  # If in WB_TIMMS dir
  file.path(script_dir, "WB_TIMMS", "output", "tables"),  # If in parent dir
  file.path(dirname(script_dir), "output", "tables"),  # If in WB_TIMMS subdir
  "output/tables",  # Relative from WB_TIMMS
  "WB_TIMMS/output/tables"  # Relative from parent
)

output_dir <- NULL
for (path in possible_paths) {
  test_file <- file.path(path, "summary_country_math_2019.xlsx")
  if (file.exists(test_file)) {
    output_dir <- path
    cat("Found output directory:", output_dir, "\n")
    break
  }
}

if (is.null(output_dir)) {
  cat("\nCould not find output directory. Tried:\n")
  for (p in possible_paths) {
    cat("  -", p, "\n")
  }
  stop("Please set working directory to 'WB_TIMMS' or its parent directory")
}

# Plots go to output/figures (same as WB_ANALYSIS distribution plots)
plots_dir <- file.path(dirname(output_dir), "figures")
cat("Plots will be saved to:", plots_dir, "\n")
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# Function: Extract beta coefficients from formatted cells (e.g., "0.45 (0.03)")
# ==============================================================================
extract_beta_from_cell <- function(cell_text) {
  if (is.na(cell_text) || cell_text == "") return(NA_real_)
  # Extract the first number before the parenthesis (the beta coefficient)
  match <- str_extract(cell_text, "^[\\-]?[0-9]+\\.[0-9]+")
  if (is.na(match)) return(NA_real_)
  return(as.numeric(match))
}

# ==============================================================================
# Function: Extract beta coefficients from regression output files
# ==============================================================================
extract_betas <- function(year, outcome) {
  # Construct file path
  file_path <- file.path(
    output_dir, 
    paste0("summary_country_", outcome, "_", year, ".xlsx")
  )
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  # Read the country-level results
  tryCatch({
    df <- read_excel(file_path)
    
    # New format (from updated WB_ANALYSIS.R):
    #   ParentB_HighEdu_beta, HighestEdu_beta (numeric betas)
    if (all(c("ParentB_HighEdu_beta", "HighestEdu_beta") %in% names(df))) {
      beta_data <- df %>%
        transmute(
          CountryName,
          ParentB = ParentB_HighEdu_beta,
          MaxOfTwo = HighestEdu_beta,
          year = year,
          outcome = outcome
        )
      return(beta_data)
    }
    
    # Backwards-compatibility: old string format
    # Column names can be either:
    # Old format: "{IndepLabel}__{Method}" (e.g., "ParentB_HighEdu__JK")
    # New (older) format: "{IndepLabel}" (e.g., "ParentB_HighEdu")
    parentB_col <- "ParentB_HighEdu"
    maxoftwo_col <- "HighestEdu"
    
    if (!parentB_col %in% names(df) || !maxoftwo_col %in% names(df)) {
      warning(paste("Expected columns not found in", file_path))
      return(NULL)
    }
    
    # Extract beta values from formatted cells
    beta_data <- df %>%
      mutate(
        ParentB = sapply(!!sym(parentB_col), extract_beta_from_cell),
        MaxOfTwo = sapply(!!sym(maxoftwo_col), extract_beta_from_cell),
        year = year,
        outcome = outcome
      ) %>%
      select(CountryName, ParentB, MaxOfTwo, year, outcome)
    
    return(beta_data)
  }, error = function(e) {
    warning(paste("Error reading", file_path, ":", e$message))
    return(NULL)
  })
}

# ==============================================================================
# Function: Extract beta coefficients from 2015 regression output files
# (Different column names: MotherHighEdu and MaxParentsHighEdu)
# ==============================================================================
extract_betas_2015 <- function(outcome) {
  file_path <- file.path(output_dir, paste0("regression_results_2015_", outcome, ".xlsx"))
  
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  tryCatch({
    df <- read_excel(file_path)
    
    # 2015 uses different column names: MotherHighEdu and MaxParentsHighEdu
    mother_col <- "MotherHighEdu"
    maxparents_col <- "MaxParentsHighEdu"
    
    if (!mother_col %in% names(df) || !maxparents_col %in% names(df)) {
      return(NULL)
    }
    
    beta_data <- df %>%
      mutate(
        ParentB = sapply(!!sym(mother_col), extract_beta_from_cell),
        MaxOfTwo = sapply(!!sym(maxparents_col), extract_beta_from_cell),
        year = 2015,
        outcome = outcome
      ) %>%
      select(CountryName, ParentB, MaxOfTwo, year, outcome)
    
    return(beta_data)
  }, error = function(e) {
    return(NULL)
  })
}

# ==============================================================================
# Main Analysis: Compare education measures for 2015, 2019 and 2023
# ==============================================================================

cat("\n=== Comparing Education Measures: ParentB/Mother vs Max of Two ===\n")

years_main <- c(2019, 2023)
outcomes <- c("math", "math_reasoning", "science", "science_reasoning")

# First, check if any files exist
cat("\nChecking for required files...\n")

# Check 2019/2023 files
files_found_main <- 0
for (yr in years_main) {
  for (out in outcomes) {
    file_path <- file.path(output_dir, paste0("summary_country_", out, "_", yr, ".xlsx"))
    if (file.exists(file_path)) {
      files_found_main <- files_found_main + 1
      cat("  ✓ Found:", basename(file_path), "\n")
    }
  }
}

# Check 2015 files
files_found_2015 <- 0
for (out in outcomes) {
  file_path <- file.path(output_dir, paste0("regression_results_2015_", out, ".xlsx"))
  if (file.exists(file_path)) {
    files_found_2015 <- files_found_2015 + 1
    cat("  ✓ Found:", basename(file_path), "(2015)\n")
  }
}

total_files <- files_found_main + files_found_2015

if (total_files == 0) {
  cat("\nNo files found in:", output_dir, "\n")
  cat("Expected files like: summary_country_math_2019.xlsx or regression_results_2015_math.xlsx\n")
  stop("No regression output files found")
}

cat("\nFound", total_files, "files (", files_found_main, "for 2019/2023,", files_found_2015, "for 2015). Proceeding...\n\n")

# Collect beta coefficients from 2019/2023
all_betas_main <- map_dfr(years_main, function(yr) {
  map_dfr(outcomes, function(out) {
    extract_betas(yr, out)
  })
})

# Collect beta coefficients from 2015 (if available)
all_betas_2015 <- map_dfr(outcomes, function(out) {
  extract_betas_2015(out)
})

# Combine all years
all_betas <- bind_rows(all_betas_main, all_betas_2015)

# Determine which years we actually have data for
years <- sort(unique(all_betas$year))

if (nrow(all_betas) == 0) {
  stop("No data extracted. Please check file contents and column names.")
}

# Remove rows with missing values
all_betas_complete <- all_betas %>%
  filter(!is.na(ParentB) & !is.na(MaxOfTwo))

cat("\nExtracted", nrow(all_betas_complete), "country-outcome-year combinations\n")

# ==============================================================================
# Create scatter plots and compute correlations
# ==============================================================================

# Overall correlation (all years and outcomes)
overall_cor <- cor(all_betas_complete$ParentB, all_betas_complete$MaxOfTwo)
n_total <- nrow(all_betas_complete)
years_label <- paste(years, collapse = ", ")
cat("\nOverall correlation between ParentB/Mother and MaxOfTwo:", round(overall_cor, 3), "\n")
cat("Years included:", years_label, "\n")

# Plot 1: Overall scatter (all years, all outcomes)
p1 <- ggplot(all_betas_complete, aes(x = ParentB, y = MaxOfTwo)) +
  geom_point(aes(color = factor(year), shape = outcome), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
  labs(
    title = "SES Gaps: Parent B / Mother Education vs Max of Two Parents",
    subtitle = paste0("Years: ", years_label, " | All Outcomes | Correlation: ", round(overall_cor, 3), " | N = ", n_total),
    x = "Beta Coefficient (Parent B / Mother High Edu)",
    y = "Beta Coefficient (Max of Two Parents)",
    color = "Year",
    shape = "Outcome"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggsave(
  filename = file.path(plots_dir, "parentB_vs_maxoftwo_overall.png"),
  plot = p1,
  width = 10,
  height = 7,
  dpi = 300
)

cat("Saved: parentB_vs_maxoftwo_overall.png\n")

# Plot 2: By year
for (yr in years) {
  data_yr <- all_betas_complete %>% filter(year == yr)
  
  if (nrow(data_yr) > 0) {
    cor_yr <- cor(data_yr$ParentB, data_yr$MaxOfTwo)
    n_yr <- nrow(data_yr)
    
    # Use appropriate label for 2015 vs 2019/2023
    parent_label <- ifelse(yr == 2015, "Mother", "Parent B")
    x_label <- ifelse(yr == 2015, "Beta Coefficient (Mother's High Edu)", "Beta Coefficient (Parent B High Edu)")
    
    p_yr <- ggplot(data_yr, aes(x = ParentB, y = MaxOfTwo)) +
      geom_point(aes(color = outcome), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
      labs(
        title = paste0("SES Gaps: ", parent_label, " Education vs Max of Two Parents"),
        subtitle = paste0("Year: ", yr, " | All Outcomes | Correlation: ", round(cor_yr, 3), " | N = ", n_yr),
        x = x_label,
        y = "Beta Coefficient (Max of Two Parents)",
        color = "Outcome"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right"
      )
    
    ggsave(
      filename = file.path(plots_dir, paste0("parentB_vs_maxoftwo_", yr, ".png")),
      plot = p_yr,
      width = 10,
      height = 7,
      dpi = 300
    )
    
    cat("Saved: parentB_vs_maxoftwo_", yr, ".png (correlation: ", round(cor_yr, 3), ")\n", sep = "")
  }
}

# Plot 3: By outcome (all years for each outcome)
for (out in outcomes) {
  data_out <- all_betas_complete %>% filter(outcome == out)
  
  if (nrow(data_out) > 0) {
    cor_out <- cor(data_out$ParentB, data_out$MaxOfTwo)
    n_out <- nrow(data_out)
    outcome_label <- tools::toTitleCase(gsub("_", " ", out))
    years_in_data <- paste(sort(unique(data_out$year)), collapse = ", ")
    
    p_out <- ggplot(data_out, aes(x = ParentB, y = MaxOfTwo)) +
      geom_point(aes(color = factor(year)), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
      labs(
        title = "SES Gaps: Parent B / Mother Education vs Max of Two Parents",
        subtitle = paste0("Outcome: ", outcome_label, " | Years: ", years_in_data, " | Correlation: ", round(cor_out, 3), " | N = ", n_out),
        x = "Beta Coefficient (Parent B / Mother High Edu)",
        y = "Beta Coefficient (Max of Two Parents)",
        color = "Year"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right"
      )
    
    ggsave(
      filename = file.path(plots_dir, paste0("parentB_vs_maxoftwo_", out, ".png")),
      plot = p_out,
      width = 10,
      height = 7,
      dpi = 300
    )
    
    cat("Saved: parentB_vs_maxoftwo_", out, ".png (correlation: ", round(cor_out, 3), ")\n", sep = "")
  }
}

# Plot 4: By year AND outcome (detailed breakdown with country labels)
cat("\n--- Detailed plots by year and outcome ---\n")
for (yr in years) {
  for (out in outcomes) {
    data_yr_out <- all_betas_complete %>% filter(year == yr, outcome == out)
    
    if (nrow(data_yr_out) >= 3) {
      cor_yr_out <- cor(data_yr_out$ParentB, data_yr_out$MaxOfTwo)
      n_countries <- nrow(data_yr_out)
      outcome_label <- tools::toTitleCase(gsub("_", " ", out))
      
      # Use appropriate label for 2015 vs 2019/2023
      parent_label <- ifelse(yr == 2015, "Mother", "Parent B")
      x_label <- ifelse(yr == 2015, "Beta Coefficient (Mother's High Edu)", "Beta Coefficient (Parent B High Edu)")
      
      p_yr_out <- ggplot(data_yr_out, aes(x = ParentB, y = MaxOfTwo)) +
        geom_point(size = 3, alpha = 0.7, color = "#2E86AB") +
        geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
        ggrepel::geom_text_repel(aes(label = CountryName), size = 2.5, max.overlaps = 25) +
        labs(
          title = paste0("SES Gaps: ", parent_label, " Education vs Max of Two Parents"),
          subtitle = paste0("Year: ", yr, " | Outcome: ", outcome_label, " | Correlation: ", round(cor_yr_out, 3), " | N = ", n_countries, " countries"),
          x = x_label,
          y = "Beta Coefficient (Max of Two Parents)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 11)
        )
      
      filename <- paste0("parentB_vs_maxoftwo_", yr, "_", out, ".png")
      ggsave(
        filename = file.path(plots_dir, filename),
        plot = p_yr_out,
        width = 10,
        height = 8,
        dpi = 300
      )
      
      cat("  Saved:", filename, "(corr:", round(cor_yr_out, 3), ")\n")
    }
  }
}

# ==============================================================================
# Save summary tables
# ==============================================================================

# Summary by year and outcome
summary_table <- all_betas_complete %>%
  group_by(year, outcome) %>%
  summarise(
    n_countries = n(),
    correlation = cor(ParentB, MaxOfTwo),
    mean_parentB = mean(ParentB),
    mean_maxoftwo = mean(MaxOfTwo),
    diff_means = mean(MaxOfTwo) - mean(ParentB),
    .groups = "drop"
  )

# Summary by year only
summary_by_year <- all_betas_complete %>%
  group_by(year) %>%
  summarise(
    n_observations = n(),
    correlation = cor(ParentB, MaxOfTwo),
    mean_parentB = mean(ParentB),
    mean_maxoftwo = mean(MaxOfTwo),
    .groups = "drop"
  )

# Summary by outcome only  
summary_by_outcome <- all_betas_complete %>%
  group_by(outcome) %>%
  summarise(
    n_observations = n(),
    correlation = cor(ParentB, MaxOfTwo),
    mean_parentB = mean(ParentB),
    mean_maxoftwo = mean(MaxOfTwo),
    .groups = "drop"
  )

# Save all summaries
writexl::write_xlsx(
  list(
    "By Year and Outcome" = summary_table,
    "By Year" = summary_by_year,
    "By Outcome" = summary_by_outcome,
    "All Data" = all_betas_complete
  ),
  file.path(output_dir, "education_measures_summary.xlsx")
)

cat("\n=== Summary Statistics ===\n")
cat("\nBy Year:\n")
print(summary_by_year)
cat("\nBy Outcome:\n")
print(summary_by_outcome)
cat("\nBy Year and Outcome:\n")
print(summary_table)

cat("\nSummary saved: education_measures_summary.xlsx\n")

cat("\n=== Education Measures Analysis Complete ===\n")
cat("All plots saved to:", plots_dir, "\n")
cat("Years included:", paste(years, collapse = ", "), "\n")
cat("Outcomes:", paste(outcomes, collapse = ", "), "\n")
cat("\nGenerated plots:\n")
cat("  - parentB_vs_maxoftwo_overall.png (all years and outcomes)\n")
for (yr in years) {
  cat("  - parentB_vs_maxoftwo_", yr, ".png\n", sep = "")
}
cat("  - parentB_vs_maxoftwo_[outcome].png (by outcome, all years)\n")
cat("  - parentB_vs_maxoftwo_[year]_[outcome].png (by year and outcome with country labels)\n")

# ==============================================================================
# Part 2: Compare SES vs Parental Investment Gaps
# ==============================================================================

cat("\n\n=== Comparing SES vs Parental Investment Gaps ===\n")

# Function to extract SES and Parental Investment betas
extract_ses_parental_betas <- function(year, outcome) {
  file_path <- file.path(
    output_dir, 
    paste0("summary_country_", outcome, "_", year, ".xlsx")
  )
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  tryCatch({
    df <- read_excel(file_path)
    
    # New format: SES_beta, PI_labels_beta (parental investment 3 labels)
    if (all(c("SES_beta", "PI_labels_beta") %in% names(df))) {
      beta_data <- df %>%
        transmute(
          CountryName,
          SES = SES_beta,
          Parental_Invest = PI_labels_beta,
          year = year,
          outcome = outcome
        )
      return(beta_data)
    }
    
    # Backwards-compatibility: Parental_Invest_beta (percentile) or old string format
    if (all(c("SES_beta", "Parental_Invest_beta") %in% names(df))) {
      beta_data <- df %>%
        transmute(
          CountryName,
          SES = SES_beta,
          Parental_Invest = Parental_Invest_beta,
          year = year,
          outcome = outcome
        )
      return(beta_data)
    }
    ses_col <- "SES"
    parental_col <- "Parental_Invest"
    
    if (!ses_col %in% names(df) || !parental_col %in% names(df)) {
      warning(paste("Expected columns not found in", file_path, 
                    "\nAvailable columns:", paste(names(df), collapse = ", ")))
      return(NULL)
    }
    
    beta_data <- df %>%
      mutate(
        SES = sapply(!!sym(ses_col), extract_beta_from_cell),
        Parental_Invest = sapply(!!sym(parental_col), extract_beta_from_cell),
        year = year,
        outcome = outcome
      ) %>%
      select(CountryName, SES, Parental_Invest, year, outcome)
    
    return(beta_data)
  }, error = function(e) {
    warning(paste("Error reading", file_path, ":", e$message))
    return(NULL)
  })
}

# Check for files
cat("\nChecking for required files...\n")
files_found_ses <- 0
for (yr in years_main) {
  for (out in outcomes) {
    file_path <- file.path(output_dir, paste0("summary_country_", out, "_", yr, ".xlsx"))
    if (file.exists(file_path)) {
      files_found_ses <- files_found_ses + 1
      cat("  ✓ Found:", basename(file_path), "\n")
    }
  }
}

if (files_found_ses == 0) {
  cat("\nNo files found for SES vs Parental Investment comparison.\n")
  cat("Skipping this analysis.\n")
} else {
  cat("\nFound", files_found_ses, "files. Proceeding...\n\n")
  
  # Collect beta coefficients
  all_betas_ses <- map_dfr(years_main, function(yr) {
    map_dfr(outcomes, function(out) {
      extract_ses_parental_betas(yr, out)
    })
  })
  
  if (nrow(all_betas_ses) == 0) {
    cat("No data extracted for SES vs Parental Investment. Skipping.\n")
  } else {
    # Remove rows with missing values
    all_betas_ses_complete <- all_betas_ses %>%
      filter(!is.na(SES) & !is.na(Parental_Invest))
    
    cat("\nExtracted", nrow(all_betas_ses_complete), "country-outcome-year combinations\n")
    
    # Overall correlation
    overall_cor_ses <- cor(all_betas_ses_complete$SES, all_betas_ses_complete$Parental_Invest)
    n_total_ses <- nrow(all_betas_ses_complete)
    years_ses <- sort(unique(all_betas_ses_complete$year))
    years_label_ses <- paste(years_ses, collapse = ", ")
    cat("\nOverall correlation between SES and Parental Investment:", round(overall_cor_ses, 3), "\n")
    cat("Years included:", years_label_ses, "\n")
    
    # Plot 1: Overall scatter
    p1_ses <- ggplot(all_betas_ses_complete, aes(x = SES, y = Parental_Invest)) +
      geom_point(aes(color = factor(year), shape = outcome), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
      labs(
        title = "SES Gaps vs Parental Investment Gaps",
        subtitle = paste0("Years: ", years_label_ses, " | All Outcomes | Correlation: ", round(overall_cor_ses, 3), " | N = ", n_total_ses),
        x = "Beta Coefficient (SES Index)",
        y = "Beta Coefficient (Parental Investment, 3 labels)",
        color = "Year",
        shape = "Outcome"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right"
      )
    
    ggsave(
      filename = file.path(plots_dir, "ses_vs_parental_invest_overall.png"),
      plot = p1_ses,
      width = 10,
      height = 7,
      dpi = 300
    )
    
    cat("Saved: ses_vs_parental_invest_overall.png\n")
    
    # Plot 2: By year
    for (yr in years_ses) {
      data_yr <- all_betas_ses_complete %>% filter(year == yr)
      
      if (nrow(data_yr) > 0) {
        cor_yr <- cor(data_yr$SES, data_yr$Parental_Invest)
        n_yr <- nrow(data_yr)
        
        p_yr <- ggplot(data_yr, aes(x = SES, y = Parental_Invest)) +
          geom_point(aes(color = outcome), size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
          geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
          labs(
            title = "SES Gaps vs Parental Investment Gaps",
            subtitle = paste0("Year: ", yr, " | All Outcomes | Correlation: ", round(cor_yr, 3), " | N = ", n_yr),
            x = "Beta Coefficient (SES Index)",
            y = "Beta Coefficient (Parental Investment, 3 labels)",
            color = "Outcome"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(face = "bold", size = 14),
            legend.position = "right"
          )
        
        ggsave(
          filename = file.path(plots_dir, paste0("ses_vs_parental_invest_", yr, ".png")),
          plot = p_yr,
          width = 10,
          height = 7,
          dpi = 300
        )
        
        cat("Saved: ses_vs_parental_invest_", yr, ".png (correlation: ", round(cor_yr, 3), ")\n", sep = "")
      }
    }
    
    # Plot 3: By outcome
    for (out in outcomes) {
      data_out <- all_betas_ses_complete %>% filter(outcome == out)
      
      if (nrow(data_out) > 0) {
        cor_out <- cor(data_out$SES, data_out$Parental_Invest)
        n_out <- nrow(data_out)
        outcome_label <- tools::toTitleCase(gsub("_", " ", out))
        years_in_data <- paste(sort(unique(data_out$year)), collapse = ", ")
        
        p_out <- ggplot(data_out, aes(x = SES, y = Parental_Invest)) +
          geom_point(aes(color = factor(year)), size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
          geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
          labs(
            title = "SES Gaps vs Parental Investment Gaps",
            subtitle = paste0("Outcome: ", outcome_label, " | Years: ", years_in_data, " | Correlation: ", round(cor_out, 3), " | N = ", n_out),
            x = "Beta Coefficient (SES Index)",
            y = "Beta Coefficient (Parental Investment, 3 labels)",
            color = "Year"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(face = "bold", size = 14),
            legend.position = "right"
          )
        
        ggsave(
          filename = file.path(plots_dir, paste0("ses_vs_parental_invest_", out, ".png")),
          plot = p_out,
          width = 10,
          height = 7,
          dpi = 300
        )
        
        cat("Saved: ses_vs_parental_invest_", out, ".png (correlation: ", round(cor_out, 3), ")\n", sep = "")
      }
    }
    
    # Plot 4: By year AND outcome (detailed with country labels)
    cat("\n--- Detailed plots by year and outcome ---\n")
    for (yr in years_ses) {
      for (out in outcomes) {
        data_yr_out <- all_betas_ses_complete %>% filter(year == yr, outcome == out)
        
        if (nrow(data_yr_out) >= 3) {
          cor_yr_out <- cor(data_yr_out$SES, data_yr_out$Parental_Invest)
          n_countries <- nrow(data_yr_out)
          outcome_label <- tools::toTitleCase(gsub("_", " ", out))
          
          p_yr_out <- ggplot(data_yr_out, aes(x = SES, y = Parental_Invest)) +
            geom_point(size = 3, alpha = 0.7, color = "#2E86AB") +
            geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
            geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
            ggrepel::geom_text_repel(aes(label = CountryName), size = 2.5, max.overlaps = 25) +
            labs(
              title = "SES Gaps vs Parental Investment Gaps",
              subtitle = paste0("Year: ", yr, " | Outcome: ", outcome_label, " | Correlation: ", round(cor_yr_out, 3), " | N = ", n_countries, " countries"),
              x = "Beta Coefficient (SES Index)",
              y = "Beta Coefficient (Parental Investment, 3 labels)"
            ) +
            theme_minimal(base_size = 12) +
            theme(
              plot.title = element_text(face = "bold", size = 14),
              plot.subtitle = element_text(size = 11)
            )
          
          filename <- paste0("ses_vs_parental_invest_", yr, "_", out, ".png")
          ggsave(
            filename = file.path(plots_dir, filename),
            plot = p_yr_out,
            width = 10,
            height = 8,
            dpi = 300
          )
          
          cat("  Saved:", filename, "(corr:", round(cor_yr_out, 3), ")\n")
        }
      }
    }
    
    # Save summary tables for SES vs Parental Investment
    summary_table_ses <- all_betas_ses_complete %>%
      group_by(year, outcome) %>%
      summarise(
        n_countries = n(),
        correlation = cor(SES, Parental_Invest),
        mean_ses = mean(SES),
        mean_parental = mean(Parental_Invest),
        diff_means = mean(Parental_Invest) - mean(SES),
        .groups = "drop"
      )
    
    summary_by_year_ses <- all_betas_ses_complete %>%
      group_by(year) %>%
      summarise(
        n_observations = n(),
        correlation = cor(SES, Parental_Invest),
        mean_ses = mean(SES),
        mean_parental = mean(Parental_Invest),
        .groups = "drop"
      )
    
    summary_by_outcome_ses <- all_betas_ses_complete %>%
      group_by(outcome) %>%
      summarise(
        n_observations = n(),
        correlation = cor(SES, Parental_Invest),
        mean_ses = mean(SES),
        mean_parental = mean(Parental_Invest),
        .groups = "drop"
      )
    
    writexl::write_xlsx(
      list(
        "By Year and Outcome" = summary_table_ses,
        "By Year" = summary_by_year_ses,
        "By Outcome" = summary_by_outcome_ses,
        "All Data" = all_betas_ses_complete
      ),
      file.path(output_dir, "ses_parental_investment_summary.xlsx")
    )
    
    cat("\n=== Summary Statistics (SES vs Parental Investment) ===\n")
    cat("\nBy Year:\n")
    print(summary_by_year_ses)
    cat("\nBy Outcome:\n")
    print(summary_by_outcome_ses)
    cat("\nBy Year and Outcome:\n")
    print(summary_table_ses)
    
    cat("\nSummary saved: ses_parental_investment_summary.xlsx\n")
    
    cat("\n=== SES vs Parental Investment Analysis Complete ===\n")
    cat("Generated plots:\n")
    cat("  - ses_vs_parental_invest_overall.png (all years and outcomes)\n")
    for (yr in years_ses) {
      cat("  - ses_vs_parental_invest_", yr, ".png\n", sep = "")
    }
    cat("  - ses_vs_parental_invest_[outcome].png (by outcome, all years)\n")
    cat("  - ses_vs_parental_invest_[year]_[outcome].png (by year and outcome with country labels)\n")
  }
}

cat("\n=== All Regression Comparison Analyses Complete ===\n")
cat("All plots saved to:", plots_dir, "\n")

