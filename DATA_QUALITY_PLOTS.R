rm(list = ls())

# ---- Configure paths (run from project root) -------------------------------
setwd("d:/CEU/policy lab/WB_TIMMS")

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

master_dir <- "data/processed_data/master"
plots_dir <- "plots/data_quality"
output_dir <- "output/data_quality"
country_lookup_path <- "data/processed_data/UNSD_codes.xlsx"

dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

load_master <- function(grade) {
  path <- file.path(master_dir, sprintf("master_table_grade_%d.RData", grade))
  if (!file.exists(path)) stop("Missing master table: ", path)
  load(path)  # loads object master_table_grade_{grade}
  get(sprintf("master_table_grade_%d", grade))
}

load_country_lookup <- function() {
  if (!file.exists(country_lookup_path)) {
    return(NULL)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Missing package readxl for country lookup.")
  }
  readxl::read_excel(country_lookup_path) %>%
    dplyr::mutate(IDCNTRY = as.numeric(IDCNTRY)) %>%
    dplyr::filter(!is.na(IDCNTRY)) %>%
    dplyr::group_by(IDCNTRY) %>%
    dplyr::summarise(
      CountryName = dplyr::first(na.omit(CountryName)),
      .groups = "drop"
    )
}

coverage_summary <- function(master, grade) {
  prefix <- if (grade == 4) "AS" else "BS"
  pv_math <- sprintf("%sMMAT%02d", prefix, 1:5)
  pv_sci <- sprintf("%sSSCI%02d", prefix, 1:5)
  pv_mrea <- sprintf("%sMREA%02d", prefix, 1:5)
  pv_srea <- sprintf("%sSREA%02d", prefix, 1:5)

  master %>%
    dplyr::mutate(IDCNTRY = as.numeric(IDCNTRY)) %>%
    dplyr::mutate(
      has_math = rowSums(!is.na(dplyr::select(., dplyr::any_of(pv_math)))) > 0,
      has_sci = rowSums(!is.na(dplyr::select(., dplyr::any_of(pv_sci)))) > 0,
      has_mrea = rowSums(!is.na(dplyr::select(., dplyr::any_of(pv_mrea)))) > 0,
      has_srea = rowSums(!is.na(dplyr::select(., dplyr::any_of(pv_srea)))) > 0
    ) %>%
    dplyr::group_by(year, IDCNTRY) %>%
    dplyr::summarise(
      n = dplyr::n(),
      hedup_share = mean(!is.na(ASDHEDUP), na.rm = TRUE),
      ses_share = mean(!is.na(SES_index), na.rm = TRUE),
      math_share = mean(has_math, na.rm = TRUE),
      sci_share = mean(has_sci, na.rm = TRUE),
      mrea_share = mean(has_mrea, na.rm = TRUE),
      srea_share = mean(has_srea, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(grade = grade)
}

plot_heatmap <- function(df, value_col, title, outfile) {
  ggplot(df, aes(x = factor(year), y = factor(country_label), fill = .data[[value_col]])) +
    geom_tile(color = "white", size = 0.1) +
    scale_fill_gradient(low = "#f7fbff", high = "#08306b", limits = c(0, 1), na.value = "gray85") +
    labs(title = title, x = "Year", y = "Country", fill = "Share") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6))
  ggsave(outfile, width = 8, height = 10, dpi = 150)
}

country_lookup <- load_country_lookup()

for (g in c(4, 8)) {
  master <- load_master(g)
  summary_df <- coverage_summary(master, g)
  if (!is.null(country_lookup)) {
    summary_df <- summary_df %>%
      dplyr::left_join(
        country_lookup %>% dplyr::select(IDCNTRY, CountryName),
        by = "IDCNTRY"
      ) %>%
      dplyr::mutate(country_label = dplyr::coalesce(CountryName, as.character(IDCNTRY)))
  } else {
    summary_df <- summary_df %>%
      dplyr::mutate(country_label = as.character(IDCNTRY))
  }
  readr::write_csv(summary_df, file.path(output_dir, sprintf("coverage_by_country_grade_%d.csv", g)))

  plot_heatmap(
    summary_df,
    "ses_share",
    sprintf("SES Index Coverage (Grade %d)", g),
    file.path(plots_dir, sprintf("ses_coverage_grade_%d.png", g))
  )
  plot_heatmap(
    summary_df,
    "mrea_share",
    sprintf("Math Reasoning PV Coverage (Grade %d)", g),
    file.path(plots_dir, sprintf("math_reasoning_coverage_grade_%d.png", g))
  )
  plot_heatmap(
    summary_df,
    "srea_share",
    sprintf("Science Reasoning PV Coverage (Grade %d)", g),
    file.path(plots_dir, sprintf("science_reasoning_coverage_grade_%d.png", g))
  )
}
