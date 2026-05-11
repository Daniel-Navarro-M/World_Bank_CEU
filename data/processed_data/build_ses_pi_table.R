# Build CSV: countries (col 1), codes (col 2), beta coefficients by variable and year.
# Reads regression Excel tables (math reasoning) and writes ses_pi_betas_by_country_year.csv
# Run from WB_TIMMS project root.

library(dplyr)
library(tidyr)
library(readxl)

base_dir <- if (basename(getwd()) == "WB_TIMMS") "." else if (dir.exists("WB_TIMMS")) "WB_TIMMS" else "."
tables_dir <- file.path(base_dir, "output", "tables")
master_path <- file.path(base_dir, "data", "processed_data", "master", "master_table_grade_4.RData")
out_path <- file.path(base_dir, "data", "processed_data", "ses_pi_betas_by_country_year.csv")

reference_outcome <- "math_reasoning"
years <- c(2019L, 2023L)

# Country -> ISO3 from master (trim for matching)
country_lookup <- tibble(CountryName = character(), ISO3 = character())
if (file.exists(master_path)) {
  load(master_path)
  if (exists("master_table_grade_4")) {
    country_lookup <- master_table_grade_4 %>%
      mutate(IDCNTRY = as.numeric(IDCNTRY), CountryName = trimws(as.character(CountryName))) %>%
      distinct(CountryName, .keep_all = TRUE) %>%
      filter(!is.na(CountryName), CountryName != "", nzchar(CountryName))
    if ("ISO3" %in% names(country_lookup)) {
      country_lookup <- country_lookup %>% select(CountryName, ISO3)
    } else {
      country_lookup <- country_lookup %>% mutate(ISO3 = NA_character_) %>% select(CountryName, ISO3)
    }
  }
}

# Helper: get first column name (country), and beta column names (flexible)
col_country <- function(d) names(d)[1]
col_beta <- function(d, stub) {
  cand <- names(d)[grepl(paste0("^", stub, "_beta$"), names(d), ignore.case = TRUE)]
  if (length(cand)) cand[1] else NULL
}

collect <- list()
for (yr in years) {
  f <- file.path(tables_dir, paste0("summary_country_", reference_outcome, "_", yr, ".xlsx"))
  if (!file.exists(f)) { warning("File not found: ", f); next }
  d <- read_excel(f)
  if (nrow(d) == 0) next
  c_country <- col_country(d)
  c_ses <- col_beta(d, "SES")
  c_pi <- col_beta(d, "PI_labels")
  if (is.null(c_ses) || is.null(c_pi)) {
    warning("Columns SES_beta or PI_labels_beta not found in ", f, ". Names: ", paste(names(d), collapse = ", "))
    next
  }
  d <- d %>%
    transmute(
      CountryName = trimws(as.character(.data[[c_country]])),
      year = yr,
      SES_beta = as.numeric(.data[[c_ses]]),
      PI_labels_beta = as.numeric(.data[[c_pi]])
    ) %>%
    filter(!is.na(CountryName), CountryName != "")
  collect[[length(collect) + 1]] <- d
}

if (length(collect) == 0) stop("No data read. Check output/tables/summary_country_math_reasoning_2019.xlsx and 2023.xlsx")

long <- bind_rows(collect)
all_countries <- long %>% distinct(CountryName) %>% arrange(CountryName) %>% pull(CountryName)

# One row per (CountryName, year); keep first non-NA if duplicates
long <- long %>%
  group_by(CountryName, year) %>%
  summarise(
    SES_beta = if (all(is.na(SES_beta))) NA_real_ else first(SES_beta[!is.na(SES_beta)]),
    PI_labels_beta = if (all(is.na(PI_labels_beta))) NA_real_ else first(PI_labels_beta[!is.na(PI_labels_beta)]),
    .groups = "drop"
  )

wide <- long %>%
  pivot_wider(
    names_from = year,
    values_from = c(SES_beta, PI_labels_beta),
    names_glue = "{.value}_{year}"
  ) %>%
  rename(
    SES_2019 = SES_beta_2019,
    SES_2023 = SES_beta_2023,
    PI_B_2019 = PI_labels_beta_2019,
    PI_B_2023 = PI_labels_beta_2023
  )

# Final table: col 1 = country, col 2 = code, then betas (every country from both Excels)
out <- tibble(CountryName = all_countries) %>%
  left_join(wide, by = "CountryName") %>%
  left_join(country_lookup, by = "CountryName") %>%
  select(CountryName, ISO3, PI_B_2019, PI_B_2023, SES_2019, SES_2023) %>%
  arrange(CountryName)

write.csv(out, out_path, row.names = FALSE)
cat("Written ", nrow(out), " rows to ", out_path, "\n", sep = "")
