#### 3.1 Exploratory Data Analysis####
##### Setup #####
setwd("C:/Users/roisi/OneDrive - Central European University (CEU GmbH Hungarian Branch Office)/Desktop/CEU/CEU Fall 2025/Policy Lab/Descriptive Stats Clean")
rm(list = ls())
library(dplyr)
library(tidyr)
library(Hmisc)
library(kableExtra)
library(readxl)
library(stringr)
library(ggplot2)
library(scales)
library(readr)
library(ggrepel)
library(tidyverse)
library(psych)
library(knitr)
library(kableExtra)

processed_data_dir <- "data/processed_data/master"
support_files_dir <- "support files"
output_dir <- "output/tables"
plots_dir <- "output/figures"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

load(file.path(processed_data_dir, "master_processed.RData"))

list.files(pattern = "*.RData", recursive = TRUE)
master <- master_processed
rm(master_processed)

# Region Group
master <- master |>
  mutate(
    REGION_GROUP = if_else(Is_balkan == 1, "Balkans", "Europe")
  )

head(master)
nrow(master)
ncol(master)
names(master)
table(master$year)

# All NA Codes
NA_CODES <- c(9, 99, 999, 998)

# Variable label lookup
var_labels <- c(
  # IDs and weights
  IDCNTRY        = "Numeric Country Code",
  IDSCHOOL       = "School ID",
  IDSTUD         = "Student ID",
  CountryName    = "Country Name",
  year           = "Survey Year",
  grade          = "Grade",
  Is_balkan      = "Balkan Country Indicator",
  TOTWGT         = "Total Student Weight",
  JKZONE         = "Jackknife Zone",
  JKREP          = "Jackknife Replicate",
  
  # Math – Region Percentiles
  ASMMAT01_ptile_region  = "Math PV1 – Percentile (Region)",
  ASMMAT02_ptile_region  = "Math PV2 – Percentile (Region)",
  ASMMAT03_ptile_region  = "Math PV3 – Percentile (Region)",
  ASMMAT04_ptile_region  = "Math PV4 – Percentile (Region)",
  ASMMAT05_ptile_region  = "Math PV5 – Percentile (Region)",
  
  # Math – Country Percentiles
  ASMMAT01_ptile_country = "Math PV1 – Percentile (Country)",
  ASMMAT02_ptile_country = "Math PV2 – Percentile (Country)",
  ASMMAT03_ptile_country = "Math PV3 – Percentile (Country)",
  ASMMAT04_ptile_country = "Math PV4 – Percentile (Country)",
  ASMMAT05_ptile_country = "Math PV5 – Percentile (Country)",
  
  # Science – Region Percentiles
  ASSSCI01_ptile_region  = "Science PV1 – Percentile (Region)",
  ASSSCI02_ptile_region  = "Science PV2 – Percentile (Region)",
  ASSSCI03_ptile_region  = "Science PV3 – Percentile (Region)",
  ASSSCI04_ptile_region  = "Science PV4 – Percentile (Region)",
  ASSSCI05_ptile_region  = "Science PV5 – Percentile (Region)",
  
  # Science – Country Percentiles
  ASSSCI01_ptile_country = "Science PV1 – Percentile (Country)",
  ASSSCI02_ptile_country = "Science PV2 – Percentile (Country)",
  ASSSCI03_ptile_country = "Science PV3 – Percentile (Country)",
  ASSSCI04_ptile_country = "Science PV4 – Percentile (Country)",
  ASSSCI05_ptile_country = "Science PV5 – Percentile (Country)",
  
  # Math Reasoning – Region Percentiles
  ASMREA01_ptile_region  = "Math Reasoning PV1 – Percentile (Region)",
  ASMREA02_ptile_region  = "Math Reasoning PV2 – Percentile (Region)",
  ASMREA03_ptile_region  = "Math Reasoning PV3 – Percentile (Region)",
  ASMREA04_ptile_region  = "Math Reasoning PV4 – Percentile (Region)",
  ASMREA05_ptile_region  = "Math Reasoning PV5 – Percentile (Region)",
  
  # Math Reasoning – Country Percentiles
  ASMREA01_ptile_country = "Math Reasoning PV1 – Percentile (Country)",
  ASMREA02_ptile_country = "Math Reasoning PV2 – Percentile (Country)",
  ASMREA03_ptile_country = "Math Reasoning PV3 – Percentile (Country)",
  ASMREA04_ptile_country = "Math Reasoning PV4 – Percentile (Country)",
  ASMREA05_ptile_country = "Math Reasoning PV5 – Percentile (Country)",
  
  # Science Reasoning – Region Percentiles
  ASSREA01_ptile_region  = "Science Reasoning PV1 – Percentile (Region)",
  ASSREA02_ptile_region  = "Science Reasoning PV2 – Percentile (Region)",
  ASSREA03_ptile_region  = "Science Reasoning PV3 – Percentile (Region)",
  ASSREA04_ptile_region  = "Science Reasoning PV4 – Percentile (Region)",
  ASSREA05_ptile_region  = "Science Reasoning PV5 – Percentile (Region)",
  
  # Science Reasoning – Country Percentiles
  ASSREA01_ptile_country = "Science Reasoning PV1 – Percentile (Country)",
  ASSREA02_ptile_country = "Science Reasoning PV2 – Percentile (Country)",
  ASSREA03_ptile_country = "Science Reasoning PV3 – Percentile (Country)",
  ASSREA04_ptile_country = "Science Reasoning PV4 – Percentile (Country)",
  ASSREA05_ptile_country = "Science Reasoning PV5 – Percentile (Country)",
  
  # Socioeconomic Status
  SES_index      = "Socioeconomic Status Index (Standardized)",
  SES_binary     = "SES Indicator (Binary)",
  parent_edu_binary = "Parental Education Indicator (Binary)",
  ASDHEDUP       = "Parents' Highest Education Level",
  ASDHOCCP       = "Parents' Highest Occupation",
  parentA_edu    = "Parent A: Education Level",
  parentB_edu    = "Parent B: Education Level",
  parentA_occ    = "Parent A: Occupation",
  parentB_occ    = "Parent B: Occupation",
  
  # Parental Investment Indicators
  PI_index       = "Parental Investment Index (Standardized)",
  PI_factor      = "Parental Investment Factor Score",
  PI_factor_p    = "Parental Investment Factor Percentile",
  PI_read        = "Parental Investment – Literacy Activities",
  PI_math        = "Parental Investment – Numeracy Activities",
  
  # Parental investment items
  ASBH01A       = "PI: Read Books",
  ASBH01B       = "PI: Tell Stories",
  ASBH01C       = "PI: Sing Songs",
  ASBH01D       = "PI: Play with Alphabet Toys",
  ASBH01E       = "PI: Talk about What Had Done",
  ASBH01F       = "PI: Book Discussion",
  ASBH01G       = "PI: Play Word Games",
  ASBH01H       = "PI: Write Letters/Words",
  ASBH01I       = "PI: Read Aloud Signs",
  ASBH01J       = "PI: Counting Songs",
  ASBH01K       = "PI: Number Toys",
  ASBH01L       = "PI: Count Things",
  ASBH01M       = "PI: Game with Shapes",
  ASBH01N       = "PI: Building Blocks",
  ASBH01O       = "PI: Board/Card Games",
  ASBH01P       = "PI: Write Numbers",
  ASBH01Q       = "PI: Draw Shapes",
  ASBH01R       = "PI: Measure or Weigh things"
)

# Helper: apply labels to a vector of variable names
label_vars <- function(vars) {
  ifelse(vars %in% names(var_labels), var_labels[vars], vars)
}
names(master)

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

# World Bank Theme for ggplot2
theme_wb <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title = element_text(
        color = wb_colors$blue_dark, size = rel(1.3), face = "bold",
        hjust = 0, margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        color = wb_colors$gray_dark, size = rel(1.0),
        hjust = 0, margin = margin(b = 12)
      ),
      plot.caption = element_text(
        color = wb_colors$gray_medium, size = rel(0.75),
        hjust = 0, lineheight = 1.3, margin = margin(t = 12)
      ),
      axis.title = element_text(color = wb_colors$gray_dark, size = rel(0.95), face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(color = wb_colors$gray_dark, size = rel(0.9)),
      axis.line = element_line(color = wb_colors$gray_light, linewidth = 0.5),
      axis.ticks = element_line(color = wb_colors$gray_light, linewidth = 0.3),
      panel.grid.major.y = element_line(color = wb_colors$gray_lighter, linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.title = element_text(color = wb_colors$gray_dark, size = rel(0.9), face = "bold"),
      legend.text = element_text(color = wb_colors$gray_dark, size = rel(0.85)),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      strip.text = element_text(
        color = wb_colors$blue_dark, size = rel(1.0), face = "bold",
        margin = margin(b = 5, t = 5)
      ),
      strip.background = element_rect(fill = wb_colors$gray_lighter, color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
}

scale_color_wb <- function(...) {
  scale_color_manual(values = c(
    "Balkans" = wb_colors$balkans,
    "Europe"  = wb_colors$europe,
    "Other"   = wb_colors$gray_medium
  ), ...)
}

scale_fill_wb <- function(...) {
  scale_fill_manual(values = c(
    "Balkans" = wb_colors$balkans,
    "Europe"  = wb_colors$europe,
    "Other"   = wb_colors$gray_medium
  ), ...)
}


##### 3.1.1 : TIMSS Participation by Country by Year (Grade 4 & 8) #####

country_groups <- master |>
  distinct(CountryName, Is_balkan) |>
  mutate(
    Group = if_else(Is_balkan == 1, "Balkans", "Europe"),
    CountryName = case_when(
      CountryName == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
      CountryName == "T\u00fcrkiye"         ~ "Turkiye",
      TRUE ~ CountryName
    )
  )

in_scope <- country_groups$CountryName

hist_raw <- read_excel(
  file.path(support_files_dir, "A-1_country-participation.xlsx"),
  col_names = FALSE,
  skip = 8
)

g4_columns <- c("...5"=2023,"...7"=2019,"...9"=2015,
                "...11"=2011,"...13"=2007,"...15"=2003,"...17"=1995)
g8_columns <- c("...19"=2023,"...21"=2019,"...23"=2015,"...25"=2011,
                "...27"=2007,"...29"=2003,"...31"=1999,"...33"=1995)
participated_symbols <- c("\u26ab", "\u26aa")

hist_clean <- hist_raw |>
  rename(country = '...3') |>
  filter(
    !is.na(country),
    str_length(country) < 40,
    !grepl("Ontario|Quebec|Abu Dhabi|Dubai|Sharjah|Kurdistan|Iraq$", country)
  ) |>
  mutate(country = case_when(
    country == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    country == "T\u00fcrkiye"         ~ "Turkiye",
    TRUE                              ~ country
  )) |>
  filter(country %in% in_scope)

g4_long <- hist_clean |>
  select(country, all_of(names(g4_columns))) |>
  pivot_longer(-country, names_to = "col", values_to = "symbol") |>
  mutate(year = g4_columns[col], grade = "Grade4") |>
  filter(symbol %in% participated_symbols) |>
  select(country, year, grade)

g8_long <- hist_clean |>
  select(country, all_of(names(g8_columns))) |>
  pivot_longer(-country, names_to = "col", values_to = "symbol") |>
  mutate(year = g8_columns[col], grade = "Grade8") |>
  filter(symbol %in% participated_symbols) |>
  select(country, year, grade)

participated_long <- bind_rows(g4_long, g8_long)

participated_summary <- participated_long |>
  group_by(country, year) |>
  summarise(
    grade4 = "Grade4" %in% grade,
    grade8 = "Grade8" %in% grade,
    .groups = "drop"
  ) |>
  mutate(
    cell = case_when(
      grade4 & grade8  ~ "\u2713 (4 & 8)",
      grade4           ~ "\u2713 (4)",
      grade8           ~ "\u2713 (8)",
      TRUE             ~ ""
    )
  ) |>
  select(country, year, cell)

all_years <- sort(unique(participated_summary$year))

participated_wide <- participated_summary |>
  pivot_wider(names_from  = year, values_from = cell, values_fill = "") |>
  left_join(country_groups, by = c("country" = "CountryName")) |>
  select(Group, Country = country, all_of(as.character(all_years))) |>
  arrange(Group, Country)

croatia_row <- tibble(
  Group   = "Balkans",
  Country = "Croatia",
  `1995`  = "", `1999` = "", `2003` = "", `2007` = "",
  `2011`  = "\u2713 (4)",
  `2015`  = "\u2713 (4)",
  `2019`  = "\u2713 (4)",
  `2023`  = ""
)

participated_wide <- bind_rows(participated_wide, croatia_row) |>
  arrange(Group, Country)

balkan_rows <- which(participated_wide$Group == "Balkans")
europe_rows <- which(participated_wide$Group == "Europe")

n_year_cols <- length(all_years)

participated_table <- participated_wide |>
  kbl(
    caption = paste0(
      "<b>Table 1.1: Country Participation in TIMSS by Year and Grade (1995\u20132023)</b>",
      "<br><small>",
      "\u2713 (4) = Grade 4 only &nbsp;|&nbsp; ",
      "\u2713 (8) = Grade 8 only &nbsp;|&nbsp; ",
      "\u2713 (4 & 8) = Both grades",
      "</small>"
    ),
    booktabs = TRUE,
    align    = c("l", "l", rep("c", n_year_cols)),
    escape   = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = TRUE,
    font_size         = 10,
    fixed_thead       = TRUE
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = wb_colors$blue_dark) |>
  row_spec(balkan_rows, bold = TRUE, color = wb_colors$blue_dark, background = wb_colors$gray_lighter) |>
  collapse_rows(columns = 1, valign = "top") |>
  add_header_above(
    c(" " = 2, "TIMSS Assessment Year" = n_year_cols),
    bold = TRUE, color = "white", background = wb_colors$blue_dark
  ) |>
  pack_rows("Balkans", min(balkan_rows), max(balkan_rows),
            bold = TRUE, color = wb_colors$balkans,
            label_row_css = paste0("background-color:", wb_colors$gray_lighter, ";",
                                   "color:", wb_colors$balkans, ";font-weight: bold;")) |>
  pack_rows("Europe", min(europe_rows), max(europe_rows),
            bold = TRUE, color = wb_colors$blue_dark,
            label_row_css = paste0("background-color:", wb_colors$gray_lighter, ";",
                                   "color:", wb_colors$blue_dark, ";font-weight: bold;")) |>
  footnote(
    general = paste0(
      "Source: IEA TIMSS Country Participation Appendix A.1 (1995\u20132023). ",
      "Croatia is manually added due to data gap, sourced from National Center for Education Statistics (US). ",
      "Both filled and open circles are coded as participation. ",
      "Open circle indicates the country did not meet sampling guidelines."
    ),
    general_title = "Note: ", footnote_as_chunk = TRUE
  )

out_path <- file.path(output_dir, "table_country_participation_by_year.html")
save_kable(participated_table, out_path)
cat("Saved:", out_path, "\n")


##### 3.1.2 : Sample Characteristics #####

sample_summary_pooled <- master |>
  group_by(year, REGION_GROUP) |>                          # REGION_GROUP already on master
  summarise(
    n_students  = n(),
    n_schools   = n_distinct(IDSCHOOL),
    n_countries = n_distinct(IDCNTRY),
    .groups     = "drop"
  )

sample_summary_balkans <- sample_summary_pooled |>
  filter(REGION_GROUP == "Balkans") |>
  mutate(label = paste0("Balkans\nN = ", format(n_students, big.mark = ","),
                        "\nSchools = ", format(n_schools, big.mark = ","),
                        "\nCountries = ", n_countries))

sample_summary_europe <- sample_summary_pooled |>
  filter(REGION_GROUP == "Europe") |>
  mutate(label = paste0("Europe\nN = ", format(n_students, big.mark = ","),
                        "\nSchools = ", format(n_schools, big.mark = ","),
                        "\nCountries = ", n_countries))

p_sample <- sample_summary_pooled |>
  ggplot(aes(x = factor(year), y = n_students / 1000, fill = REGION_GROUP)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = format(n_students, big.mark = ",")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3, color = wb_colors$gray_dark) +
  geom_label(data = sample_summary_balkans,
             aes(x = factor(year), y = n_students / 1000, label = label),
             nudge_x = -0.22, vjust = 0.5, hjust = 1, size = 2.2,
             color = wb_colors$balkans, fill = "white", alpha = 0.9,
             label.size = 0.2, label.padding = unit(0.15, "lines"),
             lineheight = 0.85, fontface = "bold", inherit.aes = FALSE) +
  geom_label(data = sample_summary_europe,
             aes(x = factor(year), y = n_students / 1000, label = label),
             nudge_x = 0.22, vjust = 0.5, hjust = 0, size = 2.2,
             color = wb_colors$blue_dark, fill = "white", alpha = 0.9,
             label.size = 0.2, label.padding = unit(0.15, "lines"),
             lineheight = 0.85, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_wb() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35)), labels = label_comma()) +
  labs(
    title    = "Sample Size by Year and Region",
    subtitle = "TIMSS Grade 4 Mathematics Assessments, 2011-2023",
    x        = "Assessment Year",
    y        = "Number of Students (thousands)",
    fill     = "Region",
    caption  = paste0(
      "Source: TIMSS International Database\n",
      "Note: Sample includes all Grade 4 students with valid mathematics scores.\n",
      "Balkans includes: Albania, Bosnia and Herzegovina, North Macedonia, Montenegro, Serbia, Kosovo.\n",
      "Europe includes all other participating European countries."
    )
  ) +
  theme_wb() +
  theme(plot.caption = element_text(lineheight = 1.2))

print(p_sample)
ggsave(file.path(plots_dir, "wb_sample_size_by_year.png"),
       p_sample, width = 12, height = 8, dpi = 300)

sample_table_wide <- sample_summary_pooled |>
  mutate(cell = paste0(format(n_students, big.mark = ","), " students | ",
                       format(n_schools,  big.mark = ","), " schools | ",
                       n_countries, " countries")) |>
  select(REGION_GROUP, year, cell) |>
  pivot_wider(names_from = year, values_from = cell, values_fill = "-") |>
  arrange(REGION_GROUP) |>
  rename(Region = REGION_GROUP)

n_year_cols_sample <- ncol(sample_table_wide) - 1

sample_table <- sample_table_wide |>
  kbl(
    caption  = "<b>Table 1.2: Sample Characteristics by Year and Region</b>",
    booktabs = TRUE,
    align    = c("l", rep("c", n_year_cols_sample)),
    escape   = FALSE
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = TRUE, font_size = 10, fixed_thead = TRUE) |>
  row_spec(0, bold = TRUE, color = "white", background = wb_colors$blue_dark) |>
  row_spec(which(sample_table_wide$Region == "Balkans"),
           bold = TRUE, color = wb_colors$balkans, background = wb_colors$gray_lighter) |>
  add_header_above(c(" " = 1, "TIMSS Assessment Year" = n_year_cols_sample),
                   bold = TRUE, color = "white", background = wb_colors$blue_dark) |>
  footnote(
    general = paste0(
      "Source: TIMSS International Database. ",
      "Sample includes all Grade 4 students with valid mathematics scores. ",
      "Balkans includes: Albania, Bosnia and Herzegovina, North Macedonia, Montenegro, Serbia, Kosovo."
    ),
    general_title = "Note: ", footnote_as_chunk = TRUE
  )

out_path <- file.path(output_dir, "table_sample_summary.html")
save_kable(sample_table, out_path)


###### Missing data summary ######

is_omitted   <- function(x) is.numeric(x) & !is.na(x) & x == 998
is_not_admin <- function(x) is.numeric(x) & !is.na(x) & x == 999
is_true_na   <- function(x) is.na(x)
is_any_missing <- function(x) is_true_na(x) | is_omitted(x) | is_not_admin(x)

analysis_vars <- setdiff(
  names(master),
  c("IDCNTRY", "IDSCHOOL", "IDSTRATE", "IDSTUD", "TOTWGT",
    "JKZONE", "JKREP", "year", "grade", "Is_balkan",
    "REGION_GROUP", "CountryName")
)

achievement_vars <- c("ASMMAT01","ASMMAT02","ASMMAT03","ASMMAT04","ASMMAT05",
                      "ASSSCI01","ASSSCI02","ASSSCI03","ASSSCI04","ASSSCI05",
                      "ASMREA01","ASMREA02","ASMREA03","ASMREA04","ASMREA05",
                      "ASSREA01","ASSREA02","ASSREA03","ASSREA04","ASSREA05")

missing_by_var <- master |>
  summarise(across(all_of(analysis_vars), list(
    pct_omitted   = ~ mean(is_omitted(.))   * 100,
    pct_not_admin = ~ mean(is_not_admin(.)) * 100,
    pct_true_na   = ~ mean(is_true_na(.))   * 100
  ))) |>
  pivot_longer(everything(), names_to = c("variable", "type"), names_pattern = "^(.+)_(pct_.+)$") |>
  pivot_wider(names_from = type, values_from = value) |>
  mutate(
    pct_total = pct_omitted + pct_not_admin + pct_true_na,
    var_type  = if_else(variable %in% achievement_vars, "Achievement", "Questionnaire")
  ) |>
  filter(pct_total > 0) |>
  arrange(desc(pct_total))

missing_by_year_summary <- master |>
  group_by(year) |>
  summarise(
    n             = n(),
    pct_omitted   = mean(rowMeans(across(all_of(analysis_vars), is_omitted)))   * 100,
    pct_not_admin = mean(rowMeans(across(all_of(analysis_vars), is_not_admin))) * 100,
    pct_true_na   = mean(rowMeans(across(all_of(analysis_vars), is_true_na)))   * 100,
    .groups       = "drop"
  ) |>
  mutate(pct_total = pct_omitted + pct_not_admin + pct_true_na)

missing_by_region <- master |>
  group_by(REGION_GROUP) |>
  summarise(
    n             = n(),
    pct_omitted   = mean(rowMeans(across(all_of(analysis_vars), is_omitted)))   * 100,
    pct_not_admin = mean(rowMeans(across(all_of(analysis_vars), is_not_admin))) * 100,
    pct_true_na   = mean(rowMeans(across(all_of(analysis_vars), is_true_na)))   * 100,
    .groups       = "drop"
  ) |>
  mutate(pct_total = pct_omitted + pct_not_admin + pct_true_na)

missing_by_country <- master |>
  group_by(CountryName, REGION_GROUP) |>
  summarise(
    n             = n(),
    pct_omitted   = mean(rowMeans(across(all_of(analysis_vars), is_omitted)))   * 100,
    pct_not_admin = mean(rowMeans(across(all_of(analysis_vars), is_not_admin))) * 100,
    pct_true_na   = mean(rowMeans(across(all_of(analysis_vars), is_true_na)))   * 100,
    .groups       = "drop"
  ) |>
  mutate(pct_total = pct_omitted + pct_not_admin + pct_true_na) |>
  arrange(REGION_GROUP, desc(pct_total))

missing_table_data <- missing_by_var |>
  mutate(across(starts_with("pct_"), ~ round(., 1))) |>
  mutate(variable = label_vars(variable)) |>
  rename(
    Variable          = variable,
    Type              = var_type,
    `% Omitted`       = pct_omitted,
    `% Not Admin`     = pct_not_admin,
    `% True NA`       = pct_true_na,
    `% Total Missing` = pct_total
  )

missing_table <- missing_table_data |>
  kbl(caption = "<b>Table 1.3: Missing Data by Variable</b>",
      booktabs = TRUE, align = c("l","l","r","r","r","r"), escape = FALSE) |>
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE, font_size = 10, fixed_thead = TRUE) |>
  row_spec(0, bold = TRUE, color = "white", background = wb_colors$blue_dark) |>
  row_spec(which(missing_table_data$`% Total Missing` > 10),
           color = wb_colors$orange, bold = TRUE) |>
  add_header_above(c(" " = 2, "Missing by Type" = 3, " " = 1),
                   bold = TRUE, color = "white", background = wb_colors$blue_dark) |>
  footnote(
    general = paste0(
      "Omitted = respondent was present but answer was missing or invalid (coded 998). ",
      "Not Administered = variable was not given to this respondent, e.g. different cycle (coded 999). ",
      "True NA = genuine missing value in achievement plausible value variables. ",
      "Values 9 and 99 (e.g. Not Applicable, Omitted) are also excluded from analysis throughout. ",
      "Variables with > 10% total missing are highlighted."
    ),
    general_title = "Note: ", footnote_as_chunk = TRUE
  )

out_path <- file.path(output_dir, "table_missing_data.html")
save_kable(missing_table, out_path)
cat("Saved:", out_path, "\n")

p_missing_var <- missing_by_var |>
  filter(pct_total > 0) |>
  mutate(variable = label_vars(variable), variable = reorder(variable, pct_total)) |>
  pivot_longer(c(pct_omitted, pct_not_admin, pct_true_na), names_to = "type", values_to = "pct") |>
  mutate(type = recode(type,
                       pct_omitted   = "Omitted/Invalid (998)",
                       pct_not_admin = "Not Administered (999)",
                       pct_true_na   = "True NA")) |>
  ggplot(aes(x = variable, y = pct, fill = type)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 10, linetype = "dashed", color = wb_colors$orange, linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Omitted/Invalid (998)"  = wb_colors$orange,
    "Not Administered (999)" = wb_colors$blue_medium,
    "True NA"                = wb_colors$gray_medium)) +
  coord_flip() +
  labs(title = "Missing Data by Variable and Type", subtitle = "Dashed line = 10% threshold",
       x = NULL, y = "% Missing", fill = NULL, caption = "Source: TIMSS International Database.") +
  theme_wb()

ggsave(file.path(plots_dir, "wb_missing_by_variable.png"), p_missing_var, width = 10, height = 6, dpi = 300)

p_missing_year <- missing_by_year_summary |>
  pivot_longer(c(pct_omitted, pct_not_admin, pct_true_na), names_to = "type", values_to = "pct") |>
  mutate(type = recode(type,
                       pct_omitted   = "Omitted/Invalid (998)",
                       pct_not_admin = "Not Administered (999)",
                       pct_true_na   = "True NA")) |>
  ggplot(aes(x = factor(year), y = pct, fill = type)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c(
    "Omitted/Invalid (998)"  = wb_colors$orange,
    "Not Administered (999)" = wb_colors$blue_medium,
    "True NA"                = wb_colors$gray_medium)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Average Missing Data by Year",
       subtitle = "Stacked by missing type, averaged across all analysis variables",
       x = "Assessment Year", y = "% Missing", fill = NULL,
       caption = "Source: TIMSS International Database.") +
  theme_wb()

ggsave(file.path(plots_dir, "wb_missing_by_year.png"), p_missing_year, width = 8, height = 5, dpi = 300)

p_missing_country <- missing_by_country |>
  mutate(CountryName = reorder(CountryName, pct_total)) |>
  pivot_longer(c(pct_omitted, pct_not_admin, pct_true_na), names_to = "type", values_to = "pct") |>
  mutate(type = recode(type,
                       pct_omitted   = "Omitted/Invalid (998)",
                       pct_not_admin = "Not Administered (999)",
                       pct_true_na   = "True NA")) |>
  ggplot(aes(x = CountryName, y = pct, fill = type)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 10, linetype = "dashed", color = wb_colors$orange, linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Omitted/Invalid (998)"  = wb_colors$orange,
    "Not Administered (999)" = wb_colors$blue_medium,
    "True NA"                = wb_colors$gray_medium)) +
  coord_flip() +
  labs(title = "Missing Data by Country",
       subtitle = "Stacked by missing type, averaged across all analysis variables",
       x = NULL, y = "% Missing", fill = NULL,
       caption = "Source: TIMSS International Database.") +
  theme_wb()

ggsave(file.path(plots_dir, "wb_missing_by_country.png"), p_missing_country, width = 10, height = 8, dpi = 300)

print(p_missing_var)
print(p_missing_year)
print(p_missing_country)


##### 3.1.3 : Parental Education Descriptive Stats #####

score_labels <- c("1" = "Primary/None",
                  "2" = "Lower Secondary",
                  "3" = "Upper Secondary",
                  "4" = "Post-Secondary",
                  "5" = "University+")

binary_labels <- c("0" = "Upper Sec & Below",
                   "1" = "Post-Sec & Above")

parentAB_labels <- c("1" = "Did not go to school",
                     "2" = "Some Primary/Lower Sec",
                     "3" = "Lower Secondary (ISCED 2)",
                     "4" = "Upper Secondary (ISCED 3)",
                     "5" = "Post-Secondary Non-Tertiary (ISCED 4)",
                     "6" = "Short-Cycle Tertiary (ISCED 5)",
                     "7" = "Bachelor's or Equivalent (ISCED 6)",
                     "8" = "Postgraduate Master's/PhD (ISCED 7/8)",
                     "9" = "Not Applicable")

freq_table_fn <- function(data, var, labels = NULL, group_var = NULL) {
  var_sym <- sym(var)
  
  base <- data |>
    filter(!is.na(!!var_sym), !(!!var_sym %in% NA_CODES)) |>
    mutate(cat = as.character(!!var_sym))
  
  if (!is.null(labels)) {
    base <- base |> mutate(cat = recode(cat, !!!labels))
  }
  
  if (is.null(group_var)) {
    base |>
      count(cat) |>
      mutate(pct      = round(n / sum(n) * 100, 1),
             variable = var, group = "Overall", year = "All Years") |>
      rename(category = cat) |>
      select(group, year, variable, category, n, pct)
  } else {
    grp_sym <- sym(group_var)
    base |>
      mutate(year = as.character(year)) |>
      group_by(!!grp_sym, year) |>
      count(cat) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      ungroup() |>
      rename(group = !!grp_sym, category = cat) |>
      mutate(variable = var) |>
      select(group, year, variable, category, n, pct)
  }
}

continuous_summary <- function(data, var, group_var = NULL) {
  var_sym <- sym(var)
  
  base <- data |>
    filter(!is.na(!!var_sym), !(!!var_sym %in% NA_CODES))
  
  summarise_stats <- function(df) {
    df |> summarise(
      n           = n(),
      mean        = round(mean(!!var_sym,   na.rm = TRUE), 2),
      sd          = round(sd(!!var_sym,     na.rm = TRUE), 2),
      median      = round(median(!!var_sym, na.rm = TRUE), 2),
      min         = min(!!var_sym, na.rm = TRUE),
      max         = max(!!var_sym, na.rm = TRUE),
      pct_missing = round(mean(is.na(!!var_sym)) * 100, 1),
      .groups     = "drop"
    ) |> mutate(variable = var)
  }
  
  if (is.null(group_var)) {
    base |> summarise_stats() |>
      mutate(group = "Overall", year = "All Years") |>
      select(group, year, variable, everything())
  } else {
    grp_sym <- sym(group_var)
    base |>
      mutate(year = as.character(year)) |>
      group_by(!!grp_sym, year) |> summarise_stats() |> ungroup() |>
      rename(group = !!grp_sym) |>
      select(group, year, variable, everything())
  }
}

# Balkan lookup (used in country-level joins below)
balkan_lookup <- master |>
  distinct(CountryName, Is_balkan) |>
  rename(group = CountryName, is_balkan = Is_balkan)

## Overall
freq_score_overall   <- freq_table_fn(master, "parent_edu_score",  score_labels)
freq_binary_overall  <- freq_table_fn(master, "parent_edu_binary", binary_labels)
freq_parentA_overall <- freq_table_fn(master, "parentA_edu",       parentAB_labels)
freq_parentB_overall <- freq_table_fn(master, "parentB_edu",       parentAB_labels)

## By country
freq_score_country   <- freq_table_fn(master, "parent_edu_score",  score_labels,   "CountryName") |>
  left_join(master |> distinct(CountryName, Is_balkan), by = c("group" = "CountryName")) |>
  arrange(desc(Is_balkan), group)
freq_binary_country  <- freq_table_fn(master, "parent_edu_binary", binary_labels,  "CountryName") |>
  left_join(master |> distinct(CountryName, Is_balkan), by = c("group" = "CountryName")) |>
  arrange(desc(Is_balkan), group)
freq_parentA_country <- freq_table_fn(master, "parentA_edu",       parentAB_labels,"CountryName") |>
  left_join(master |> distinct(CountryName, Is_balkan), by = c("group" = "CountryName")) |>
  arrange(desc(Is_balkan), group)
freq_parentB_country <- freq_table_fn(master, "parentB_edu",       parentAB_labels,"CountryName") |>
  left_join(master |> distinct(CountryName, Is_balkan), by = c("group" = "CountryName")) |>
  arrange(desc(Is_balkan), group)

## Continuous summary for parent_edu_score
cont_score_overall <- continuous_summary(master, "parent_edu_score")
cont_score_country <- continuous_summary(master, "parent_edu_score", "CountryName") |>
  left_join(balkan_lookup, by = "group") |>
  mutate(is_balkan = replace_na(is_balkan, 0)) |>
  arrange(desc(is_balkan), group)

# Save CSVs
write_csv(bind_rows(freq_score_overall,   freq_score_country   |> select(-Is_balkan)),
          file.path(output_dir, "desc_parent_edu_score_freq.csv"))
write_csv(bind_rows(freq_binary_overall,  freq_binary_country  |> select(-Is_balkan)),
          file.path(output_dir, "desc_parent_edu_binary_freq.csv"))
write_csv(bind_rows(freq_parentA_overall, freq_parentA_country |> select(-Is_balkan)),
          file.path(output_dir, "desc_parentA_edu_freq.csv"))
write_csv(bind_rows(freq_parentB_overall, freq_parentB_country |> select(-Is_balkan)),
          file.path(output_dir, "desc_parentB_edu_freq.csv"))
write_csv(bind_rows(cont_score_overall,   cont_score_country   |> select(-is_balkan)),
          file.path(output_dir, "desc_parent_edu_score_continuous.csv"))


make_kable_freq <- function(overall_df, country_df, title, subtitle_note,
                            footnote_text, file_name) {
  # Standardise the balkan column name before binding
  country_df <- country_df |> rename_with(~ "is_balkan", any_of(c("Is_balkan", "is_balkan")))
  
  combined <- bind_rows(
    overall_df |> mutate(is_balkan = -1),
    country_df
  )
  
  balkan_rows <- which(combined$is_balkan == 1)
  overall_row <- which(combined$is_balkan == -1)
  multi_var   <- n_distinct(combined$variable) > 1
  
  display_df <- combined |> select(-is_balkan)
  if (!multi_var) display_df <- display_df |> select(-variable)
  
  col_names <- c("Country / Group", "Year", if (multi_var) "Variable", "Category", "N", "%")
  align     <- c("l", "l", if (multi_var) "l", "l", "r", "r")
  
  tbl <- display_df |>
    kbl(caption   = paste0("<b>", title, "</b><br><small>", subtitle_note, "</small>"),
        booktabs  = TRUE, align = align, col.names = col_names, escape = FALSE) |>
    kable_styling(bootstrap_options = c("striped","hover","condensed"),
                  full_width = TRUE, font_size = 10, fixed_thead = TRUE) |>
    row_spec(0,           bold = TRUE, color = "white",           background = wb_colors$blue_dark)   |>
    row_spec(overall_row, bold = TRUE, color = "white",           background = wb_colors$blue_medium) |>
    row_spec(balkan_rows, bold = TRUE, color = wb_colors$blue_dark, background = wb_colors$gray_lighter) |>
    pack_rows("Balkans", min(balkan_rows), max(balkan_rows), bold = TRUE, color = wb_colors$balkans,
              label_row_css = paste0("background-color:", wb_colors$gray_lighter, ";",
                                     "color:", wb_colors$balkans, ";font-weight: bold;")) |>
    footnote(general = footnote_text, general_title = "Note: ", footnote_as_chunk = TRUE)
  
  out_path <- file.path(output_dir, file_name)
  save_kable(tbl, out_path)
  cat("Saved:", out_path, "\n")
  invisible(tbl)
}


make_kable_cont <- function(overall_df, country_df, title, footnote_text, file_name) {
  combined <- bind_rows(
    overall_df |> mutate(is_balkan = -1),
    country_df
  )
  
  balkan_rows <- which(combined$is_balkan == 1)
  overall_row <- which(combined$is_balkan == -1)
  multi_var   <- n_distinct(combined$variable) > 1
  
  display_df <- combined |>
    select(-is_balkan) |>
    rename(`Country / Group` = group, Year = year, N = n,
           Mean = mean, SD = sd, Median = median, Min = min, Max = max,
           `% Missing` = pct_missing)
  
  if (multi_var) display_df <- display_df |> rename(Variable = variable) else
    display_df <- display_df |> select(-variable)
  
  n_leading    <- ncol(display_df) - 7
  header_above <- setNames(c(n_leading, 6L, 1L), c(" ", "Descriptive Statistics", " "))
  
  tbl <- display_df |>
    kbl(caption = paste0("<b>", title, "</b>"), booktabs = TRUE, escape = FALSE) |>
    kable_styling(bootstrap_options = c("striped","hover","condensed"),
                  full_width = TRUE, font_size = 10, fixed_thead = TRUE) |>
    row_spec(0,           bold = TRUE, color = "white",           background = wb_colors$blue_dark)   |>
    row_spec(overall_row, bold = TRUE, color = "white",           background = wb_colors$blue_medium) |>
    row_spec(balkan_rows, bold = TRUE, color = wb_colors$blue_dark, background = wb_colors$gray_lighter) |>
    add_header_above(header_above, bold = TRUE, color = "white", background = wb_colors$blue_dark) |>
    pack_rows("Balkans", min(balkan_rows), max(balkan_rows), bold = TRUE, color = wb_colors$balkans,
              label_row_css = paste0("background-color:", wb_colors$gray_lighter, ";",
                                     "color:", wb_colors$balkans, ";font-weight: bold;")) |>
    footnote(general = footnote_text, general_title = "Note: ", footnote_as_chunk = TRUE)
  
  out_path <- file.path(output_dir, file_name)
  save_kable(tbl, out_path)
  cat("Saved:", out_path, "\n")
  invisible(tbl)
}

make_kable_freq(
  freq_score_overall, freq_score_country,
  title         = "Table 3.1: Parent Education Score — Frequency Distribution",
  subtitle_note = "1 = Primary/None &nbsp;|&nbsp; 2 = Lower Sec &nbsp;|&nbsp; 3 = Upper Sec &nbsp;|&nbsp; 4 = Post-Sec &nbsp;|&nbsp; 5 = University+ &nbsp;|&nbsp; Balkan countries highlighted",
  footnote_text = paste0(
    "Source: TIMSS International Database. ",
    "parent_edu_score is derived from ASDHEDUP (highest education of either parent). ",
    "Values 9, 99, 999, 998 (not applicable / omitted / not administered) are excluded. ",
    "Balkans includes: Albania, Bosnia and Herzegovina, North Macedonia, Montenegro, Serbia, Kosovo."
  ),
  file_name = "table_parent_edu_score_freq.html"
)

make_kable_freq(
  freq_binary_overall, freq_binary_country,
  title         = "Table 3.2: Parent Education Binary — Frequency Distribution",
  subtitle_note = "0 = Upper Secondary &amp; Below &nbsp;|&nbsp; 1 = Post-Secondary &amp; Above &nbsp;|&nbsp; Balkan countries highlighted",
  footnote_text = paste0(
    "Source: TIMSS International Database. ",
    "parent_edu_binary = 0 maps to parent_edu_score 1–3 (up to upper secondary). ",
    "parent_edu_binary = 1 maps to parent_edu_score 4–5 (post-secondary and above). ",
    "Values 9, 99, 999, 998 excluded. ",
    "Balkans includes: Albania, Bosnia and Herzegovina, North Macedonia, Montenegro, Serbia, Kosovo."
  ),
  file_name = "table_parent_edu_binary_freq.html"
)

make_kable_freq(
  freq_parentA_overall, freq_parentA_country,
  title         = "Table 3.3: Parent A Education (parentA_edu) — Frequency Distribution",
  subtitle_note = "Raw TIMSS item: highest education level reported for Parent A &nbsp;|&nbsp; Balkan countries highlighted",
  footnote_text = paste0(
    "Source: TIMSS International Database. parentA_edu corresponds to ASBH15A. ",
    "ISCED levels: 1=No school, 2=Some Primary/Lower Sec, 3=Lower Sec (ISCED 2), ",
    "4=Upper Sec (ISCED 3), 5=Post-Sec Non-Tertiary (ISCED 4), 6=Short-Cycle Tertiary (ISCED 5), ",
    "7=Bachelor's (ISCED 6), 8=Postgraduate (ISCED 7/8), 9=Not Applicable. ",
    "Values 9, 99, 999, 998 excluded. ",
    "Balkans includes: Albania, Bosnia and Herzegovina, North Macedonia, Montenegro, Serbia, Kosovo."
  ),
  file_name = "table_parentA_edu_freq.html"
)

make_kable_freq(
  freq_parentB_overall, freq_parentB_country,
  title         = "Table 3.4: Parent B Education (parentB_edu) — Frequency Distribution",
  subtitle_note = "Raw TIMSS item: highest education level reported for Parent B &nbsp;|&nbsp; Balkan countries highlighted",
  footnote_text = paste0(
    "Source: TIMSS International Database. parentB_edu corresponds to ASBH15B. ",
    "ISCED levels: 1=No school, 2=Some Primary/Lower Sec, 3=Lower Sec (ISCED 2), ",
    "4=Upper Sec (ISCED 3), 5=Post-Sec Non-Tertiary (ISCED 4), 6=Short-Cycle Tertiary (ISCED 5), ",
    "7=Bachelor's (ISCED 6), 8=Postgraduate (ISCED 7/8), 9=Not Applicable. ",
    "Values 9, 99, 999, 998 excluded. ",
    "Balkans includes: Albania, Bosnia and Herzegovina, North Macedonia, Montenegro, Serbia, Kosovo."
  ),
  file_name = "table_parentB_edu_freq.html"
)

make_kable_cont(
  cont_score_overall, cont_score_country,
  title         = "Table 3.5: Parent Education Score — Continuous Summary by Country",
  footnote_text = paste0(
    "Source: TIMSS International Database. ",
    "parent_edu_score: 1=Primary/None, 2=Lower Secondary, 3=Upper Secondary, ",
    "4=Post-Secondary, 5=University+. ",
    "Values 9, 99, 999, 998 excluded prior to computation. ",
    "% Missing reflects true NA only after exclusions. ",
    "Balkans includes: Albania, Bosnia and Herzegovina, North Macedonia, Montenegro, Serbia, Kosovo."
  ),
  file_name = "table_parent_edu_score_continuous.html"
)


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

if ("iso3" %in% names(master)) master <- master |> select(-iso3)
master <- master |> left_join(timss_iso3_lookup, by = "IDCNTRY")

unmatched <- master |> filter(is.na(iso3)) |> distinct(IDCNTRY, CountryName)
if (nrow(unmatched) > 0) {
  message("WARNING: No iso3 match for these IDCNTRY codes:")
  print(unmatched)
}

wb_data_raw <- read_csv(file.path(support_files_dir, "World_Bank_Edu_Attainment.csv"),
                        show_col_types = FALSE)

wb_bachelor <- wb_data_raw |>
  filter(`Series Name` ==
           "Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)") |>
  select(iso3 = `Country Code`,
         wb_bachelor_2019 = `2019 [YR2019]`,
         wb_bachelor_2023 = `2023 [YR2023]`) |>
  mutate(across(starts_with("wb"), as.numeric))

edu_summary <- master |>
  filter(!is.na(parent_edu_binary)) |>
  group_by(iso3, CountryName, REGION_GROUP, year) |>
  summarise(
    Binary_High = weighted.mean(parent_edu_binary == 1, w = TOTWGT, na.rm = TRUE) * 100,
    Binary_Low  = weighted.mean(parent_edu_binary == 0, w = TOTWGT, na.rm = TRUE) * 100,
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

cat(sprintf(
  "\nExternal Validation Correlations (parent_edu_binary vs WB Bachelor+):\n  2019: r = %.3f  (n = %d)\n  2023: r = %.3f  (n = %d)\n",
  test_2019$estimate, nrow(corr_2019), test_2023$estimate, nrow(corr_2023)
))

scatter_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11, color = "gray30"),
    plot.caption     = element_text(hjust = 0, size = 9, color = "gray40", lineheight = 1.2),
    axis.title.x     = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y     = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

make_validation_scatter <- function(corr_df, wb_col, test_obj, yr, outfile) {
  p <- ggplot(corr_df, aes(x = .data[[wb_col]], y = Binary_High)) +
    geom_point(size = 4, alpha = 0.7, color = "#1565C0") +
    geom_smooth(method = "lm", se = TRUE, color = "#A23B72", fill = "#A23B72",
                alpha = 0.2, linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_text_repel(aes(label = iso3), size = 3.5, fontface = "bold", max.overlaps = 20) +
    labs(
      title    = paste0("External Validation: Parental Education Binary vs World Bank (", yr, ")"),
      subtitle = sprintf("parent_edu_binary post secondary & university | r = %.3f, n = %d countries",
                         test_obj$estimate, nrow(corr_df)),
      x       = "World Bank: At Least Bachelor's Degree (%, population 25+)",
      y       = "TIMSS: Above Upper Secondary (%, parents of 4th graders)",
      caption = "Dashed line = perfect agreement. TIMSS includes post-secondary non-tertiary in addition to university."
    ) +
    scatter_theme +
    scale_x_continuous(labels = label_percent(scale = 1)) +
    scale_y_continuous(labels = label_percent(scale = 1))
  ggsave(outfile, p, width = 12, height = 9, dpi = 300, bg = "white")
  invisible(p)
}

if (nrow(corr_2019) >= 3)
  make_validation_scatter(corr_2019, "wb_bachelor_2019", test_2019, 2019,
                          file.path(plots_dir, "Parent_Edu_Binary_1_Validation_2019.png"))
if (nrow(corr_2023) >= 3)
  make_validation_scatter(corr_2023, "wb_bachelor_2023", test_2023, 2023,
                          file.path(plots_dir, "Parent_Edu_Binary_1_Validation_2023.png"))


##### 3.1. : Parental Investment Descriptive Stats #####

# ASBH01A–ASBH01R are binary 1=Often, 2=Sometimes/Never
# NA codes: 9, 99, 999, 998 (excluded via NA_CODES)

home_learning_vars <- c(
  "ASBH01A","ASBH01B","ASBH01C","ASBH01D","ASBH01E","ASBH01F",
  "ASBH01G","ASBH01H","ASBH01I","ASBH01J","ASBH01K","ASBH01L",
  "ASBH01M","ASBH01N","ASBH01O","ASBH01P","ASBH01Q","ASBH01R"
)

activity_labels <- c(
  "Read books",
  "Tell stories",
  "Sing songs",
  "Play with alphabet toys",
  "Talk about what child had done",
  "Book discussion",
  "Play word games",
  "Write letters or words",
  "Read aloud signs and labels",
  "Counting songs or rhymes",
  "Play with number toys",
  "Count things",
  "Play games with shapes",
  "Play with building blocks",
  "Play board or card games",
  "Write numbers",
  "Draw shapes",
  "Measure or weigh things"
)

literacy_vars <- home_learning_vars[1:9]   # Items A–I
numeracy_vars <- home_learning_vars[10:18]  # Items J–R

# Basic descriptive stats 
# 1= Often, 2=Sometimes/Never

recode_pi_binary <- function(x) {
  x <- ifelse(x %in% NA_CODES, NA, x)   # NA codes out
  ifelse(x == 1, 1L,                     # Often       → 1
         ifelse(x == 2, 0L, NA_integer_))       # Sometimes/Never → 0; anything else → NA
}


desc_stats <- master |>
  select(all_of(home_learning_vars)) |>
  mutate(across(everything(), recode_pi_binary)) |>
  describe() |>
  as.data.frame() |>
  rownames_to_column("Variable") |>
  mutate(Activity = activity_labels) |>
  select(Variable, Activity, n, mean, sd, median, min, max, skew, kurtosis) |>
  mutate(across(c(mean, sd, skew, kurtosis), ~ round(., 2)))

desc_stats |>
  select(-Variable) |>
  kable(
    caption   = "Descriptive Statistics for Home Learning Activities (1 = Often, 0 = Sometimes/Never)",
    col.names = c("Activity", "N", "Mean (Prop. Often)", "SD", "Median", "Min", "Max", "Skew", "Kurtosis")
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

# Save descriptive stats
write_csv(desc_stats,
          file.path(output_dir, "desc_PI_binary_descriptive_stats.csv"))

# Frequency Distribution
get_freq_dist <- function(var_name, label) {
  master |>
    mutate(resp = recode_pi_binary(.data[[var_name]])) |>  # recode before counting
    filter(!is.na(resp)) |>
    count(Response = resp) |>
    complete(Response = 0:1, fill = list(n = 0)) |>        # guarantee both levels present
    mutate(
      Activity   = label,
      Percentage = round(n / sum(n) * 100, 1)
    ) |>
    rename(Count = n)
}

pi_freq_table <- map2_dfr(home_learning_vars, activity_labels, get_freq_dist)

freq_wide <- pi_freq_table |>
  pivot_wider(
    names_from  = Response,
    values_from = c(Count, Percentage),
    names_glue  = "{Response}_{.value}"
  ) |>
  select(Activity, starts_with("0_"), starts_with("1_"))

freq_wide |>
  kable(
    caption   = "Frequency Distribution of Home Learning Activities (Binary)",
    col.names = c("Activity", "Count", "%", "Count", "%")
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) |>
  add_header_above(c(" "                    = 1,
                     "Sometimes/Never (0)"  = 2,
                     "Often (1)"            = 2))

# Save frequency table
write_csv(pi_freq_table,
          file.path(output_dir, "desc_PI_binary_freq_long.csv"))
write_csv(freq_wide,
          file.path(output_dir, "desc_PI_binary_freq_wide.csv"))
# Literacy vs Numeracy
# Balkans vs Other Europe
# Parental Investment by Occupation Level
# Parental Investment by Education Level
# Parental Investment by SES Index
# Missing data summary

##### 3.1. : Household Resources Descriptive Stats #####
# (placeholder)

##### 3.1. : Socio-Economic Index Descriptive Stats #####
# (placeholder)

##### 3.1. : TIMSS Maths Results Descriptive Stats FIX##### 
names(master)
math_descriptives <- master |>
  filter(REGION_GROUP %in% c("Balkans", "Europe")) |>
  group_by(year, REGION_GROUP) |>
  summarise(
    n_total      = n(),
    n_valid      = sum(!is.na(ASMMAT_avg)),
    mean_score   = mean(ASMMAT_avg, na.rm = TRUE),
    weighted_mean = weighted.mean(ASMMAT_avg, w = TOTWGT, na.rm = TRUE),
    sd_score     = sd(ASMMAT_avg, na.rm = TRUE),
    se_score     = sd_score / sqrt(n_valid),
    median_score = median(ASMMAT_avg, na.rm = TRUE),
    min_score    = min(ASMMAT_avg, na.rm = TRUE),
    max_score    = max(ASMMAT_avg, na.rm = TRUE),
    p10          = quantile(ASMMAT_avg, 0.10, na.rm = TRUE),
    p25          = quantile(ASMMAT_avg, 0.25, na.rm = TRUE),
    p75          = quantile(ASMMAT_avg, 0.75, na.rm = TRUE),
    p90          = quantile(ASMMAT_avg, 0.90, na.rm = TRUE),
    iqr          = p75 - p25,
    .groups      = "drop"
  )

print(math_descriptives)

##### 3.1. : Plot Maths Results on Interest Variables #####
