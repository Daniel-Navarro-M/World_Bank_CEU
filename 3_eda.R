rm(list = ls())
library(dplyr)
library(tidyr)
library(Hmisc)
library(ggplot2)
library(ggrepel)
library(scales)

source("R/world_bank_theme.R")
source("R/rubin_pv_helpers.R")

processed_data_dir <- "data/processed_data"
output_dir <- "output/tables"
plots_dir <- "output/figures"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

load(file.path(processed_data_dir, "master", "master_processed.RData"))

dat <- master_processed
req <- c("PI_binary", "parent_edu_binary", "ASDHEDUP", "CountryName", "year", "TOTWGT")
if (length(setdiff(req, names(dat))) > 0) stop("Missing cols. Run 2_objective_builder.R first.")
CODE_NA <- 998
CODE_NOT_ADMIN <- 999

# Save one CSV table with stable naming.
save_csv <- function(tbl, base_name) {
  p <- file.path(output_dir, paste0(base_name, ".csv"))
  write.csv(tbl, p, row.names = FALSE)
  message("Saved: ", p)
}

# Build weighted categorical distribution by country/year.
pct_dist <- function(df, var, lab_map, include_na = FALSE) {
  w <- df$TOTWGT
  x <- df[[var]]
  valid <- !is.na(w) & w > 0
  if (include_na) x <- replace(x, is.na(x), 999)
  t <- Hmisc::wtd.table(x[valid], w[valid], type = "table")
  p <- round(100 * as.vector(t) / sum(t), 1)
  nm <- names(t)
  setNames(p, ifelse(nm %in% names(lab_map), lab_map[nm], nm))
}

# Produce country-year table for one categorical variable.
run_table <- function(df, var, lab_map, include_na = FALSE) {
  rows <- list()
  for (i in unique(df$IDCNTRY)) {
    for (yr in unique(df$year[df$IDCNTRY == i])) {
      sub <- df %>% filter(IDCNTRY == i, year == yr)
      p <- pct_dist(sub, var, lab_map, include_na)
      rows[[length(rows) + 1]] <- data.frame(CountryName = unique(sub$CountryName)[1], year = yr, as.list(p), check.names = FALSE)
    }
  }
  bind_rows(rows)
}

# Clean PI fields for weighted means.
prep_pi <- function(df) {
  df %>%
    filter(!is.na(TOTWGT), TOTWGT > 0) %>%
    mutate(
      PI_index = as.numeric(na_if(na_if(PI_index, 998), 999)),
      PI_read  = as.numeric(na_if(na_if(PI_read, 998), 999)),
      PI_math  = as.numeric(na_if(na_if(PI_math, 998), 999))
    )
}

# Aggregate PI means by country for one year.
pi_country_means <- function(df_year) {
  df_year %>%
    filter(!is.na(PI_index)) %>%
    group_by(IDCNTRY, CountryName) %>%
    summarise(
      weighted_mean_PI = weighted.mean(PI_index, TOTWGT, na.rm = TRUE),
      weighted_mean_PI_read = weighted.mean(PI_read, TOTWGT, na.rm = TRUE),
      weighted_mean_PI_math = weighted.mean(PI_math, TOTWGT, na.rm = TRUE),
      .groups = "drop"
    )
}

# Aggregate PI means by parental education for one year.
pi_country_edu_means <- function(df_year) {
  df_year %>%
    filter(!is.na(parent_edu_binary), !(parent_edu_binary %in% c(998, 999))) %>%
    group_by(IDCNTRY, CountryName, parent_edu_binary) %>%
    summarise(
      weighted_mean_PI = weighted.mean(PI_index, TOTWGT, na.rm = TRUE),
      weighted_mean_PI_read = weighted.mean(PI_read, TOTWGT, na.rm = TRUE),
      weighted_mean_PI_math = weighted.mean(PI_math, TOTWGT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(parent_edu_label = recode(parent_edu_binary, `0` = "Low Education", `1` = "High Education")) %>%
    select(-parent_edu_binary) %>%
    pivot_wider(names_from = parent_edu_label, values_from = c(weighted_mean_PI, weighted_mean_PI_read, weighted_mean_PI_math))
}

 # rubin_combine is sourced from R/rubin_pv_helpers.R

edu_labs <- c("1"="Did not go","2"="Some Primary","3"="Lower sec","4"="Upper sec","5"="Post-sec non-tert","6"="Short-cycle tert","7"="Bachelor","8"="Postgrad","998"="Omitted/invalid","999"="Not administered")
hedup_labs <- c("1"="University+","2"="Post-sec","3"="Upper sec","4"="Lower sec","5"="Primary/None","998"="Omitted/invalid","999"="Not administered")
occ_labs <- c("1"="Never worked","2"="Small business","3"="Clerical","4"="Service/Sales","5"="Agric/Fishery","6"="Craft/Trade","7"="Plant operator","8"="General laborer","9"="Manager","10"="Professional","11"="Technician","998"="Omitted/invalid","999"="Not administered")
occ_hedup_labs <- c("1"="Professional","2"="Small business","3"="Clerical","4"="Skilled worker","5"="General laborer","6"="Never worked","998"="Omitted/invalid","999"="Not administered")
pi_labs <- c("0"="Never", "1"="Sometimes", "2"="Often", "998"="Omitted/invalid", "999"="Not administered")


# ---- PI means by country / by education (all available years) ----
years_avail <- sort(unique(dat$year))
for (yr in years_avail) {
  d <- dat %>% filter(year == yr, CountryName != "Netherlands") %>% prep_pi()
  save_csv(pi_country_means(d), paste0("eda_PI_average_country_", yr))
  save_csv(pi_country_edu_means(d), paste0("eda_PI_edu_average_country_", yr))
}


# Distribution of parental education by country

t_hedup <- run_table(dat, "ASDHEDUP", hedup_labs)
for (yr in years_avail) {
  d <- dat %>% filter(year == yr)
  png(file.path(plots_dir, paste0("parent_edu_", yr, ".png")), width = 800, height = 600)
  fr <- table(d$ASDHEDUP)
  pie(fr, labels = hedup_labs[names(fr)], main = paste("Distribution of Parent Education (", yr, ")", sep = ""), cex = 0.8, init.angle = 90)
  dev.off()
  save_csv(t_hedup %>% filter(year == yr, CountryName != "Netherlands") %>% select(-year), paste0("eda_highest_edu_", yr))
}


edu_labels <- c("0" = "Upper \nSecondary education or less",
                "1" = "More than Upper \nSecondary education",
                "998" = "Not valid or missing",
                "999" = "Not administered",
                "NA" = "Missing")
t_parentBI <- run_table(dat, "parent_edu_binary", c("1"="High Edu","0"="Low Edu", "998"="Not Valid or Missing", "999"="Not administered"))
for (yr in years_avail) {
  d <- dat %>% filter(year == yr)
  png(file.path(plots_dir, paste0("parent_edu_binary_", yr, ".png")), width = 800, height = 600)
  fr <- table(d$parent_edu_binary, useNA = "ifany")
  pie(fr, labels = edu_labels[names(fr)], main = paste("Distribution of Parent Education Binary (", yr, ")", sep = ""), cex = 0.8, init.angle = 90)
  dev.off()
  save_csv(t_parentBI %>% filter(year == yr, CountryName != "Netherlands") %>% select(-year), paste0("eda_parent_binary_edu_", yr))
}


##### Math achievement summary by country (quantiles, mean, std)
# Uses 5 plausible values (ASMMAT01-05): per-student mean, then weighted descriptives by country/year.
# Filter Netherlands; separate tables per year.
pv_math <- sprintf("ASMMAT%02d", 1:5)
if (all(pv_math %in% names(dat))) {
  dat_math <- dat %>% filter(CountryName != "Netherlands", !is.na(TOTWGT), TOTWGT > 0) %>% mutate(math_avg = rowMeans(across(all_of(pv_math)), na.rm = TRUE))
  for (yr in years_avail) {
    tbl <- dat_math %>% filter(year == yr, is.finite(math_avg)) %>% group_by(IDCNTRY, CountryName) %>%
      summarise(n=n(), mean=Hmisc::wtd.mean(math_avg, TOTWGT, na.rm = TRUE), std=sqrt(Hmisc::wtd.var(math_avg, TOTWGT, normwt = TRUE)),
                p10=Hmisc::wtd.quantile(math_avg, TOTWGT, 0.10, na.rm = TRUE), p25=Hmisc::wtd.quantile(math_avg, TOTWGT, 0.25, na.rm = TRUE),
                p50=Hmisc::wtd.quantile(math_avg, TOTWGT, 0.50, na.rm = TRUE), p75=Hmisc::wtd.quantile(math_avg, TOTWGT, 0.75, na.rm = TRUE),
                p90=Hmisc::wtd.quantile(math_avg, TOTWGT, 0.90, na.rm = TRUE), .groups = "drop")
    save_csv(tbl, paste0("eda_country_summary_", yr))
  }
}


##### PI average by region (Europe, Balkans), both years
# 3 rows: PI_index, PI_math, PI_read. 4 columns: Europe_2019, Europe_2023, Balkans_2019, Balkans_2023.
pi_region_df <- dat %>%
  filter(CountryName != "Netherlands", !is.na(TOTWGT) & TOTWGT > 0,
         !(PI_index %in% c(998, 999)), !(PI_read %in% c(998, 999)), !(PI_math %in% c(998, 999))) %>%
  mutate(
    REGION_GROUP = if_else(Is_balkan == 1, "Balkans", "Europe"),
    PI_index = na_if(as.numeric(PI_index), 998), PI_index = na_if(PI_index, 999),
    PI_read  = na_if(as.numeric(PI_read), 998),  PI_read  = na_if(PI_read, 999),
    PI_math  = na_if(as.numeric(PI_math), 998),  PI_math  = na_if(PI_math, 999)
  )
pi_reg_agg <- pi_region_df %>%
  group_by(REGION_GROUP, year) %>%
  summarise(
    PI_index = weighted.mean(PI_index, TOTWGT, na.rm = TRUE),
    PI_math  = weighted.mean(PI_math, TOTWGT, na.rm = TRUE),
    PI_read  = weighted.mean(PI_read, TOTWGT, na.rm = TRUE),
    .groups = "drop"
  )
eda_PI_regional <- data.frame(
  Variable = c("PI_index", "PI_math", "PI_read"),
  Europe_2019  = c(
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2019) %>% pull(PI_index),
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2019) %>% pull(PI_math),
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2019) %>% pull(PI_read)),
  Europe_2023  = c(
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2023) %>% pull(PI_index),
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2023) %>% pull(PI_math),
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2023) %>% pull(PI_read)),
  Balkans_2019 = c(
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2019) %>% pull(PI_index),
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2019) %>% pull(PI_math),
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2019) %>% pull(PI_read)),
  Balkans_2023 = c(
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2023) %>% pull(PI_index),
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2023) %>% pull(PI_math),
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2023) %>% pull(PI_read))
)
if (any(pi_reg_agg$year == 2024)) {
  eda_PI_regional$Europe_2024 <- c(
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2024) %>% pull(PI_index),
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2024) %>% pull(PI_math),
    pi_reg_agg %>% filter(REGION_GROUP == "Europe", year == 2024) %>% pull(PI_read))
  eda_PI_regional$Balkans_2024 <- c(
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2024) %>% pull(PI_index),
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2024) %>% pull(PI_math),
    pi_reg_agg %>% filter(REGION_GROUP == "Balkans", year == 2024) %>% pull(PI_read))
}
save_csv(eda_PI_regional, "eda_PI_regional")


# PI averages by region and parental education (Low vs High), by year
pi_region_edu <- dat %>%
  filter(
    CountryName != "Netherlands",
    !is.na(TOTWGT) & TOTWGT > 0,
    !(PI_index %in% c(998, 999)),
    !(PI_read %in% c(998, 999)),
    !(PI_math %in% c(998, 999)),
    !(parent_edu_binary %in% c(998, 999))
  ) %>%
  mutate(
    REGION_GROUP = if_else(Is_balkan == 1, "Balkans", "Europe"),
    PI_index = as.numeric(na_if(na_if(PI_index, 998), 999)),
    PI_read  = as.numeric(na_if(na_if(PI_read, 998), 999)),
    PI_math  = as.numeric(na_if(na_if(PI_math, 998), 999)),
    parent_edu_binary = as.numeric(parent_edu_binary),
    parent_edu_label = dplyr::case_when(
      parent_edu_binary == 1 ~ "High Education",
      parent_edu_binary == 0 ~ "Low Education",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(parent_edu_label)) %>%
  group_by(REGION_GROUP, year, parent_edu_label) %>%
  summarise(
    PI_index = Hmisc::wtd.mean(PI_index, TOTWGT, na.rm = TRUE),
    PI_math  = Hmisc::wtd.mean(PI_math,  TOTWGT, na.rm = TRUE),
    PI_read  = Hmisc::wtd.mean(PI_read,  TOTWGT, na.rm = TRUE),
    .groups = "drop"
  )

pi_reg_edu_2019 <- pi_region_edu %>%
  filter(year == 2019) %>%
  select(REGION_GROUP, parent_edu_label, PI_index, PI_math, PI_read)
pi_reg_edu_2023 <- pi_region_edu %>%
  filter(year == 2023) %>%
  select(REGION_GROUP, parent_edu_label, PI_index, PI_math, PI_read)
pi_reg_edu_2024 <- pi_region_edu %>%
  filter(year == 2024) %>%
  select(REGION_GROUP, parent_edu_label, PI_index, PI_math, PI_read)

# Reshape so rows = variables, columns = region × education level
eda_PI_regional_edu_2019 <- data.frame(
  Variable = c("PI_index", "PI_math", "PI_read"),
  Europe_Low_Edu  = c(
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_index),
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_math),
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_read)),
  Europe_High_Edu = c(
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_index),
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_math),
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_read)),
  Balkans_Low_Edu  = c(
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_index),
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_math),
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_read)),
  Balkans_High_Edu = c(
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_index),
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_math),
    pi_reg_edu_2019 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_read))
)

eda_PI_regional_edu_2023 <- data.frame(
  Variable = c("PI_index", "PI_math", "PI_read"),
  Europe_Low_Edu  = c(
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_index),
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_math),
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_read)),
  Europe_High_Edu = c(
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_index),
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_math),
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_read)),
  Balkans_Low_Edu  = c(
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_index),
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_math),
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_read)),
  Balkans_High_Edu = c(
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_index),
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_math),
    pi_reg_edu_2023 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_read))
)

save_csv(eda_PI_regional_edu_2019, "eda_PI_regional_edu_2019")
save_csv(eda_PI_regional_edu_2023, "eda_PI_regional_edu_2023")

if (exists("pi_reg_edu_2024") && nrow(pi_reg_edu_2024) > 0) {
  eda_PI_regional_edu_2024 <- data.frame(
    Variable = c("PI_index", "PI_math", "PI_read"),
    Europe_Low_Edu  = c(
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_index),
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_math),
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "Low Education")  %>% pull(PI_read)),
    Europe_High_Edu = c(
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_index),
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_math),
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Europe",  parent_edu_label == "High Education") %>% pull(PI_read)),
    Balkans_Low_Edu  = c(
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_index),
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_math),
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "Low Education")  %>% pull(PI_read)),
    Balkans_High_Edu = c(
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_index),
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_math),
      pi_reg_edu_2024 %>% filter(REGION_GROUP == "Balkans", parent_edu_label == "High Education") %>% pull(PI_read))
  )
  save_csv(eda_PI_regional_edu_2024, "eda_PI_regional_edu_2024")
}



##### External validation for educational achievement variable
# Parental Education Binary External Validation

timss_iso3_lookup <- tribble(
  ~IDCNTRY, ~iso3,
  40,  "AUT", 100, "BGR", 191, "HRV", 203, "CZE", 208, "DNK",
  233, "EST", 246, "FIN", 250, "FRA", 276, "DEU", 300, "GRC",
  348, "HUN", 372, "IRL", 380, "ITA", 428, "LVA", 440, "LTU",
  528, "NLD", 578, "NOR", 616, "POL", 620, "PRT", 703, "SVK",
  705, "SVN", 724, "ESP", 752, "SWE", 792, "TUR", 826, "GBR",
  8,   "ALB", 70,  "BIH", 807, "MKD", 499, "MNE", 688, "SRB",
  36,  "AUS", 124, "CAN", 376, "ISR", 392, "JPN", 410, "KOR",
  554, "NZL", 840, "USA", 398, "KAZ", 275, "PSE", 414, "KWT",
  400, "JOR", 422, "LBN", 682, "SAU", 788, "TUN", 818, "EGY",
  368, "IRQ", 364, "IRN", 702, "SGP", 158, "TWN", 344, "HKG",
  446, "MAC", 608, "PHL", 710, "ZAF", 76,  "BRA", 152, "CHL",
  170, "COL", 411, "XKX", 470, "MLT", 642, "ROU",
  956, "BEL", 957, "BEL"
)

if ("iso3" %in% names(dat)) dat <- dat |> select(-iso3)

library(readr)

wb_data_raw <- read_csv("data/processed_data/World_Bank_Edu_Attainment.csv", show_col_types = FALSE)
wb_data_raw
wb_filtered <- wb_data_raw |>
  filter(`Series Name` ==
           "Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)")
wb_bachelor <- wb_filtered |>
  select(iso3 = `Country Code`, wb_bachelor_2019 = `2019 [YR2019]`, wb_bachelor_2023 = `2023 [YR2023]`) |>
  mutate(across(starts_with("wb"), as.numeric))
if ("2024 [YR2024]" %in% names(wb_filtered)) {
  wb_bachelor$wb_bachelor_2024 <- as.numeric(wb_filtered[["2024 [YR2024]"]])
}


dat <- dat |>
  mutate(REGION_GROUP = if_else(Is_balkan == 1, "Balkans", "Europe"))

if ("iso3" %in% names(dat)) dat <- dat |> select(-iso3)
dat <- dat |> left_join(timss_iso3_lookup, by = "IDCNTRY")

dat$parent_edu_binary_2 <- case_when(
  dat$parent_edu_score %in% 1:4 ~ 0,
  dat$parent_edu_score %in% 5 ~ 1,
  TRUE ~ NA_real_)

edu_summary <- dat |>
  filter(!is.na(parent_edu_binary_2), CountryName != "Netherlands") |>
  group_by(iso3, CountryName, REGION_GROUP, year) |>
  summarise(
    Binary_High = weighted.mean(parent_edu_binary_2 == 1, w = TOTWGT, na.rm = TRUE) * 100,
    Binary_Low  = weighted.mean(parent_edu_binary_2 == 0, w = TOTWGT, na.rm = TRUE) * 100,
    n           = n(),
    .groups     = "drop"
  )

edu_with_wb <- edu_summary |> left_join(wb_bachelor, by = "iso3")

corr_2019 <- edu_with_wb |>
  filter(year == 2019, REGION_GROUP %in% c("Balkans","Europe"),
         !is.na(Binary_High), !is.na(wb_bachelor_2019))

corr_2023 <- edu_with_wb |>
  filter(year == 2023, REGION_GROUP %in% c("Balkans","Europe"),
         !is.na(Binary_High), !is.na(wb_bachelor_2023))

test_2019 <- if (nrow(corr_2019) >= 3) cor.test(corr_2019$Binary_High, corr_2019$wb_bachelor_2019) else list(estimate = NA_real_, p.value = NA_real_)
test_2023 <- if (nrow(corr_2023) >= 3) cor.test(corr_2023$Binary_High, corr_2023$wb_bachelor_2023) else list(estimate = NA_real_, p.value = NA_real_)

val_msg <- sprintf(
  "\nExternal Validation Correlations (parent_edu_binary vs WB Bachelor+):\n  2019: r = %.3f  (n = %d)\n  2023: r = %.3f  (n = %d)\n",
  test_2019$estimate, nrow(corr_2019), test_2023$estimate, nrow(corr_2023)
)
corr_2024 <- NULL
test_2024 <- NULL
if ("wb_bachelor_2024" %in% names(edu_with_wb)) {
  corr_2024 <- edu_with_wb |>
    filter(year == 2024, REGION_GROUP %in% c("Balkans","Europe"),
           !is.na(Binary_High), !is.na(wb_bachelor_2024))
  test_2024 <- if (nrow(corr_2024) >= 3) cor.test(corr_2024$Binary_High, corr_2024$wb_bachelor_2024) else list(estimate = NA_real_, p.value = NA_real_)
  val_msg <- paste0(val_msg, sprintf("  2024: r = %.3f  (n = %d)\n", test_2024$estimate, nrow(corr_2024)))
}
cat(val_msg)


# World Bank theme/colors from R/world_bank_theme.R (already sourced)

make_validation_scatter <- function(corr_df, wb_col, test_obj, yr, outfile) {
  # Same axis limits all years for visual comparison (values are 0–100 scale)
  p <- ggplot(corr_df, aes(x = .data[[wb_col]], y = Binary_High)) +
    geom_point(size = 4, alpha = 0.7, color = "#1565C0") +
    geom_smooth(method = "lm", se = TRUE, color = "#A23B72", fill = "#A23B72",
                alpha = 0.2, linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_text_repel(aes(label = iso3), size = 3.5, fontface = "bold", max.overlaps = 20) +
    labs(
      title    = paste0("External validation: parental education (binary) vs World Bank bachelor+ (", yr, ")"),
      subtitle = sprintf("parent_edu_binary post secondary & university | r = %.3f, n = %d countries",
                         test_obj$estimate, nrow(corr_df)),
      x       = "World Bank: At Least Bachelor's Degree (%, population 25+)",
      y       = "TIMSS: Above Upper Secondary (%, parents of 4th graders)",
      caption = "Dashed line = perfect agreement. TIMSS includes post-secondary non-tertiary in addition to university. Axes fixed 0–60% (x) and 0–80% (y) across years."
    ) +
    scatter_theme +
    scale_x_continuous(labels = label_percent(scale = 1)) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    coord_cartesian(xlim = c(0, 60), ylim = c(0, 80), expand = FALSE, clip = "off")
  ggsave(outfile, p, width = 12, height = 9, dpi = 300, bg = "white")
  invisible(p)
}


make_validation_scatter(corr_2019, "wb_bachelor_2019", test_2019, 2019,
                          file.path(plots_dir, "Parent_Edu_Binary_1_Validation_2019.png"))
make_validation_scatter(corr_2023, "wb_bachelor_2023", test_2023, 2023,
                          file.path(plots_dir, "Parent_Edu_Binary_1_Validation_2023.png"))
if (!is.null(corr_2024) && nrow(corr_2024) >= 3 && !is.null(test_2024))
  make_validation_scatter(corr_2024, "wb_bachelor_2024", test_2024, 2024,
                          file.path(plots_dir, "Parent_Edu_Binary_1_Validation_2024.png"))


# PI items
pi_items <- sprintf("ASBH01%s", LETTERS[1:18])
pi_read <- sprintf("ASBH01%s", LETTERS[1:9])
pi_math <- sprintf("ASBH01%s", LETTERS[10:18])
pi_rows <- list()
for (it in pi_items) {
  if (!it %in% names(dat)) next
  for (i in unique(dat$IDCNTRY)) {
    for (yr in unique(dat$year[dat$IDCNTRY == i])) {
      df <- dat %>% filter(IDCNTRY == i, year == yr)
      p <- pct_dist(df, it, pi_labs)
      dom <- if (it %in% pi_read) "PI_read" else "PI_math"
      pi_rows[[length(pi_rows) + 1]] <- data.frame(CountryName = unique(df$CountryName)[1], year = yr, Variable = it, Domain = dom, as.list(p), check.names = FALSE)
    }
  }
}
pi_wide <- bind_rows(pi_rows)
save_csv(pi_wide, "eda_pi_items")

# School Inputs Exploration — longitudinal 2023 vs 2024: overlap of unique CountryID+SchoolID and CountryID+StudentID
long_path <- file.path(processed_data_dir, "master", "master_longitudinal_processed.RData")
if (file.exists(long_path)) {
  load(long_path)
  dlong <- master_longitudinal_processed %>% filter(year %in% c(2023L, 2024L))
  # Align with 1_build_master_table.R balkan list if Is_balkan missing or inconsistent
  balkan_countries_long <- c("Albania", "Bosnia and Herzegovina", "North Macedonia", "Montenegro", "Serbia", "Kosovo")
  dlong <- dlong %>%
    mutate(
      is_balkan = if ("Is_balkan" %in% names(dlong)) {
        as.integer(!is.na(Is_balkan) & (Is_balkan == 1L | Is_balkan == TRUE))
      } else {
        as.integer(CountryName %in% balkan_countries_long)
      }
    )
  overlap_23_24 <- function(df, id_col) {
    df <- df %>%
      mutate(
        .key = paste(
          as.numeric(haven::zap_labels(IDCNTRY)),
          as.numeric(haven::zap_labels(.data[[id_col]])),
          sep = "|"
        )
      )
    row1 <- function(sub) {
      k23 <- unique(sub$.key[sub$year == 2023]); k24 <- unique(sub$.key[sub$year == 2024])
      m <- length(intersect(k23, k24)); a <- length(setdiff(k23, k24)); b <- length(setdiff(k24, k23)); N <- m + a + b
      tibble(N = N, matched_pct = 100 * m / pmax(N, 1L), only_2023_pct = 100 * a / pmax(N, 1L), only_2024_pct = 100 * b / pmax(N, 1L))
    }
    cn_flag <- df %>%
      group_by(Country = CountryName) %>%
      summarise(is_balkan = max(is_balkan, na.rm = TRUE), .groups = "drop")
    by_country <- df %>%
      group_by(Country = CountryName) %>%
      group_modify(~ row1(.x)) %>%
      ungroup() %>%
      left_join(cn_flag, by = "Country")
    balk <- row1(df %>% filter(is_balkan == 1L)) %>% mutate(Country = "Balkans (all countries)", is_balkan = 1L)
    nonb <- row1(df %>% filter(is_balkan == 0L)) %>% mutate(Country = "Non-Balkans (all countries)", is_balkan = 0L)
    all_row <- row1(df) %>% mutate(Country = "All database", is_balkan = NA_integer_)
    bind_rows(by_country %>% arrange(Country), balk, nonb, all_row)
  }
  tbl_schools <- overlap_23_24(dlong, "IDSCHOOL")
  tbl_students <- overlap_23_24(dlong, "IDSTUD")
  print(tbl_schools); print(tbl_students)
  save_csv(tbl_schools, "eda_longitudinal_school_overlap")
  save_csv(tbl_students, "eda_longitudinal_student_overlap")

  # ---- Longitudinal 2023 vs 2024 Math means (PVs + Rubin), SD, delta and significance test ----
  pv_math_long <- sprintf("ASMMAT%02d", 1:5)
  if (all(pv_math_long %in% names(dlong))) {
    dlong_math <- dlong %>%
      filter(!is.na(CountryName), !is.na(TOTWGT), TOTWGT > 0, year %in% c(2023L, 2024L))

    countries_23 <- unique(dlong_math$CountryName[dlong_math$year == 2023L])
    countries_24 <- unique(dlong_math$CountryName[dlong_math$year == 2024L])
    countries_both <- sort(intersect(countries_23, countries_24))
    dlong_math <- dlong_math %>% filter(CountryName %in% countries_both)

    # Build one Rubin-combined row (country or region) from PV-specific weighted means.
    calc_math_delta_row <- function(df_in, label) {
      mean23_j <- mean24_j <- var23_j <- var24_j <- n23_j <- n24_j <- rep(NA_real_, length(pv_math_long))
      for (j in seq_along(pv_math_long)) {
        v <- pv_math_long[j]
        d23 <- df_in %>% filter(year == 2023L, !is.na(.data[[v]]))
        d24 <- df_in %>% filter(year == 2024L, !is.na(.data[[v]]))
        if (nrow(d23) > 1) {
          mean23_j[j] <- weighted.mean(d23[[v]], d23$TOTWGT)
          var23_j[j] <- Hmisc::wtd.var(d23[[v]], d23$TOTWGT)
          n23_j[j] <- nrow(d23)
        }
        if (nrow(d24) > 1) {
          mean24_j[j] <- weighted.mean(d24[[v]], d24$TOTWGT)
          var24_j[j] <- Hmisc::wtd.var(d24[[v]], d24$TOTWGT)
          n24_j[j] <- nrow(d24)
        }
      }

      se23_j <- sqrt(var23_j / pmax(n23_j, 1))
      se24_j <- sqrt(var24_j / pmax(n24_j, 1))
      delta_j <- mean24_j - mean23_j
      se_delta_j <- sqrt(se24_j^2 + se23_j^2)

      cmb23 <- rubin_combine(mean23_j, se23_j)
      cmb24 <- rubin_combine(mean24_j, se24_j)
      cmbd <- rubin_combine(delta_j, se_delta_j)
      t_d <- ifelse(is.finite(cmbd$se) && cmbd$se > 0, cmbd$beta / cmbd$se, NA_real_)
      p_d <- ifelse(is.finite(t_d), 2 * pnorm(-abs(t_d)), NA_real_)
      data.frame(
        Country = label,
        mean_2023 = round(cmb23$beta, 3),
        sd_2023 = round(sqrt(mean(var23_j, na.rm = TRUE)), 3),
        mean_2024 = round(cmb24$beta, 3),
        sd_2024 = round(sqrt(mean(var24_j, na.rm = TRUE)), 3),
        delta_2024_2023 = round(cmbd$beta, 3),
        t_stat = round(t_d, 3),
        p_value = signif(p_d, 4),
        stringsAsFactors = FALSE
      )
    }

    rows_delta <- lapply(countries_both, function(cn) calc_math_delta_row(dlong_math %>% filter(CountryName == cn), cn))
    tbl_long_math_delta <- bind_rows(rows_delta) %>% arrange(Country)
    row_balkan <- calc_math_delta_row(dlong_math %>% filter(is_balkan == 1L), "Balkans (regional)")
    row_europe <- calc_math_delta_row(dlong_math %>% filter(is_balkan == 0L), "Non-Balkans Europe (regional)")
    row_all <- calc_math_delta_row(dlong_math, "All longitudinal countries")
    tbl_long_math_delta <- bind_rows(tbl_long_math_delta, row_balkan, row_europe, row_all)
    save_csv(tbl_long_math_delta, "eda_longitudinal_math_mean_delta_rubin_2023_2024")

    # ---- Distribution plot: country facets (2 columns x 3 rows), 2023 vs 2024 ----
    dplot <- dlong_math %>%
      mutate(
        math_pv_mean = rowMeans(across(all_of(pv_math_long)), na.rm = TRUE),
        year = factor(year)
      ) %>%
      filter(is.finite(math_pv_mean))

    p_long_dist <- ggplot(dplot, aes(x = math_pv_mean, fill = year)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.45, position = "identity") +
      facet_wrap(~ CountryName, ncol = 2) +
      scale_fill_manual(values = c("2023" = "#1f78b4", "2024" = "#e31a1c")) +
      labs(
        title = "Distribution of math scores by year (longitudinal countries)",
        subtitle = "Each panel is one country (2023 vs 2024)",
        x = "Math score (mean of 5 plausible values)",
        y = "Density",
        fill = "Year"
      ) +
      theme_minimal(base_size = 11)

    ggsave(
      file.path(plots_dir, "eda_longitudinal_math_distribution_2023_2024.png"),
      p_long_dist, width = 12, height = 13, dpi = 300, bg = "white"
    )
    message("Saved: ", file.path(plots_dir, "eda_longitudinal_math_distribution_2023_2024.png"))
  } else {
    message("Skip longitudinal math delta table/plot: ASMMAT01-05 not found in master_longitudinal_processed.")
  }
} else message("Skip longitudinal school/student overlap: missing ", long_path)

# Heatmap (dat is master_processed; use its vars only)
vars_hm <- c("parentA_edu", "parentB_edu", "ASDHEDUP", "parentA_occ", "parentB_occ", "ASDHOCCP", "SES_index", "PI_factor_p", "PI_read", "PI_math", pi_items)
vars_hm <- intersect(vars_hm, names(dat))
dat_hm <- dat
miss_list <- list()
for (i in unique(dat_hm$IDCNTRY)) {
  for (yr in unique(dat_hm$year[dat_hm$IDCNTRY == i])) {
    df <- dat_hm %>% filter(IDCNTRY == i, year == yr, !is.na(CountryName))
    w <- df$TOTWGT
    for (v in vars_hm) {
      x <- df[[v]]
      valid <- !is.na(w) & w > 0
      miss <- valid & (is.na(x) | x %in% c(CODE_NA, CODE_NOT_ADMIN))
      pct <- 100 * sum(w[miss]) / sum(w[valid])
      miss_list[[length(miss_list) + 1]] <- data.frame(CountryName = unique(df$CountryName)[1], year = yr, variable = v, pct_missing = pct)
    }
  }
}
miss_df <- bind_rows(miss_list) %>% filter(!is.na(CountryName), !is.na(year))
if (nrow(miss_df) > 0 && requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(miss_df, ggplot2::aes(x = variable, y = CountryName, fill = pct_missing)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b", limits = c(0, 100), na.value = "gray90") +
    ggplot2::labs(title = "Missing values by country and variable (%)", x = "Variable", y = "Country", fill = "% missing") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7))
  if (length(unique(miss_df$year)) > 1) p <- p + ggplot2::facet_wrap(~ year, ncol = 1)
  ggplot2::ggsave(file.path(plots_dir, "eda_missing_heatmap.png"), p, width = 12, height = 10, dpi = 150)
  message("Saved heatmap: ", file.path(plots_dir, "eda_missing_heatmap.png"))
}


# ===============================
# ## ZOOM IN PARENT AGREES ASBLH EDA ##
# ===============================
load("data/processed_data/master/master_longitudinal_processed.RData")
library(dplyr)

d <- master_longitudinal_processed %>%
  filter(year %in% c(2023, 2024)) %>%
  distinct(IDSTUD, IDCNTRY, year, .keep_all = TRUE) %>%
  group_by(IDSTUD, IDCNTRY) %>%
  filter(n_distinct(year) == 2) %>%
  arrange(year, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(
    PI_read = ifelse(as.numeric(PI_read) %in% c(9, 99, 998, 999), NA, as.numeric(PI_read)),
    PI_math = ifelse(as.numeric(PI_math) %in% c(9, 99, 998, 999), NA, as.numeric(PI_math)),
    PI_index = ifelse(as.numeric(PI_index) %in% c(998, 999), NA, as.numeric(PI_index)),
    books_score = ifelse(as.numeric(books_score) %in% c(998, 999), NA, as.numeric(books_score)),
    children_books_score = ifelse(as.numeric(children_books_score) %in% c(998, 999), NA, as.numeric(children_books_score)),
    home_books_count = ifelse(as.numeric(home_books_count) %in% c(998, 999), NA, as.numeric(home_books_count)),
    children_books_count = ifelse(as.numeric(children_books_count) %in% c(998, 999), NA, as.numeric(children_books_count)),
    resources_computer = ifelse(as.numeric(resources_computer) %in% c(998, 999), NA, as.numeric(resources_computer)),
    resources_tablet = ifelse(as.numeric(resources_tablet) %in% c(998, 999), NA, as.numeric(resources_tablet)),
    resources_internet = ifelse(as.numeric(resources_internet) %in% c(998, 999), NA, as.numeric(resources_internet)),
    gen_agree_included_rev = ifelse(as.numeric(gen_agree_included_rev) %in% c(998, 999), NA, as.numeric(gen_agree_included_rev)),
    gen_agree_safe_env_rev = ifelse(as.numeric(gen_agree_safe_env_rev) %in% c(998, 999), NA, as.numeric(gen_agree_safe_env_rev)),
    gen_agree_cares_progress_rev = ifelse(as.numeric(gen_agree_cares_progress_rev) %in% c(998, 999), NA, as.numeric(gen_agree_cares_progress_rev)),
    gen_agree_keeps_informed_rev = ifelse(as.numeric(gen_agree_keeps_informed_rev) %in% c(998, 999), NA, as.numeric(gen_agree_keeps_informed_rev)),
    gen_agree_promotes_standards_rev = ifelse(as.numeric(gen_agree_promotes_standards_rev) %in% c(998, 999), NA, as.numeric(gen_agree_promotes_standards_rev)),
    gen_agree_helps_reading_rev = ifelse(as.numeric(gen_agree_helps_reading_rev) %in% c(998, 999), NA, as.numeric(gen_agree_helps_reading_rev)),
    gen_agree_helps_math_rev = ifelse(as.numeric(gen_agree_helps_math_rev) %in% c(998, 999), NA, as.numeric(gen_agree_helps_math_rev)),
    gen_agree_helps_science_rev = ifelse(as.numeric(gen_agree_helps_science_rev) %in% c(998, 999), NA, as.numeric(gen_agree_helps_science_rev))
  )

agree_vars <- c("gen_agree_included_rev","gen_agree_safe_env_rev","gen_agree_cares_progress_rev","gen_agree_keeps_informed_rev","gen_agree_promotes_standards_rev","gen_agree_helps_reading_rev","gen_agree_helps_math_rev","gen_agree_helps_science_rev")
agree_vars <- agree_vars[agree_vars %in% names(d)]
if (length(agree_vars) > 0) {
  agree_mat <- as.matrix(d[, agree_vars, drop = FALSE])
  d$PI_agree_index <- rowSums(agree_mat, na.rm = TRUE)
  d$PI_agree_index[rowSums(!is.na(agree_mat)) == 0] <- NA_real_
}

home_vars <- c("home_books_count","children_books_count","resources_computer","resources_tablet","resources_internet")
home_vars <- home_vars[home_vars %in% names(d)]
core_vars <- c(
  "PI_read","PI_math","PI_index","PI_binary","parent_edu_binary",
  "books_score","children_books_score","home_resources_learning_rev",
  "env_discuss","teacher_parental_involvement_rev"
)
core_vars <- core_vars[core_vars %in% names(d)]

build_change_tbl <- function(df, vars_vec) {
  bind_rows(lapply(vars_vec, function(v) {
    g <- df %>% group_by(IDSTUD, IDCNTRY) %>% summarise(v_2023 = first(.data[[v]]), v_2024 = last(.data[[v]]), comparable = !is.na(v_2023) & !is.na(v_2024), changed = comparable & (v_2023 != v_2024), .groups = "drop")
    s23 <- df %>% filter(year == 2023) %>% summarise(mean = mean(.data[[v]], na.rm = TRUE), sd = sd(.data[[v]], na.rm = TRUE), q1 = quantile(.data[[v]], 0.25, na.rm = TRUE), q3 = quantile(.data[[v]], 0.75, na.rm = TRUE))
    s24 <- df %>% filter(year == 2024) %>% summarise(mean = mean(.data[[v]], na.rm = TRUE), sd = sd(.data[[v]], na.rm = TRUE), q1 = quantile(.data[[v]], 0.25, na.rm = TRUE), q3 = quantile(.data[[v]], 0.75, na.rm = TRUE))
    data.frame(variable = v, students_total = nrow(g), students_comparable = sum(g$comparable, na.rm = TRUE), students_changed = sum(g$changed, na.rm = TRUE), change_rate_pct = round(100 * sum(g$changed, na.rm = TRUE) / max(sum(g$comparable, na.rm = TRUE), 1), 2), mean_2023 = s23$mean, sd_2023 = s23$sd, q1_2023 = s23$q1, q3_2023 = s23$q3, mean_2024 = s24$mean, sd_2024 = s24$sd, q1_2024 = s24$q1, q3_2024 = s24$q3, stringsAsFactors = FALSE)
  }))
}

tbl_gen_agrees <- build_change_tbl(d, c(agree_vars, "PI_agree_index"))
tbl_home_resources <- build_change_tbl(d, home_vars)
tbl_core <- build_change_tbl(d, core_vars)

cat("\nChange CORE VARIABLES\n")
print(tbl_core)

cat("\nChange PI GEN AGREE\n")
print(tbl_gen_agrees)
cat("\nChange HOME_RESOURCES\n")
print(tbl_home_resources)

dir.create("output/descriptive_stats_panel", recursive = TRUE, showWarnings = FALSE)
write.csv(tbl_core, "output/descriptive_stats_panel/EDA_PI_student_change.csv", row.names = FALSE)
write.csv(tbl_gen_agrees, "output/descriptive_stats_panel/EDA_GEN_AGREES.csv", row.names = FALSE)
write.csv(tbl_home_resources, "output/descriptive_stats_panel/EDA_HOME_RESOURCES.csv", row.names = FALSE)
cat("Saved: output/descriptive_stats_panel/EDA_PI_student_change.csv\n")
cat("Saved: output/descriptive_stats_panel/EDA_GEN_AGREES.csv\n")
cat("Saved: output/descriptive_stats_panel/EDA_HOME_RESOURCES.csv\n")


