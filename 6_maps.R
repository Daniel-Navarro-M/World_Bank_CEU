# =============================================================================
# 6. Maps – Edu binary gradients on Math + PI_index
# Produces maps per year: 2019, 2023, and 2024 (when 2024 regression tables exist):
#   Math~parent_edu_binary, PI_index~parent_edu_binary
# =============================================================================

rm(list = ls())
library(dplyr)
library(tibble)
source("R/world_bank_theme.R")
library(readr)
library(sf)
library(ggplot2)

# ---- Paths ----
out_dir   <- "output/tables"
plots_dir <- "output/figures/maps"
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

geo_path  <- "data/europe_with_unsd.geojson"  # or "data/europe.geojson" if that's the file

# ---- 1. Parse "beta (se)" strings ----
parse_beta_se <- function(x) {
  x <- as.character(x)
  beta_str <- sub("\\s*\\(.*", "", x)              # strip everything after "("
  beta_str <- gsub("\\*", "", beta_str)           # drop significance stars
  beta <- as.numeric(trimws(beta_str))
  
  se_str <- sub(".*\\(([^)]+)\\).*", "\\1", x)    # capture inside parentheses
  se <- suppressWarnings(as.numeric(trimws(se_str)))
  
  data.frame(beta = beta, se = se, stringsAsFactors = FALSE)
}

# ---- 2. Load regression tables (same ones used in 5_correlations.R) ----
coef_PI_2019   <- read.csv(file.path(out_dir, "reg_pi_index_parent_edu_binary_2019.csv"),
                           stringsAsFactors = FALSE)
coef_PI_2023   <- read.csv(file.path(out_dir, "reg_pi_index_parent_edu_binary_2023.csv"),
                           stringsAsFactors = FALSE)
coef_math_2019 <- read.csv(file.path(out_dir, "reg_math_parent_edu_binary_2019.csv"),
                           stringsAsFactors = FALSE)
coef_math_2023 <- read.csv(file.path(out_dir, "reg_math_parent_edu_binary_2023.csv"),
                           stringsAsFactors = FALSE)
has_2024 <- file.exists(file.path(out_dir, "reg_pi_index_parent_edu_binary_2024.csv")) &&
  file.exists(file.path(out_dir, "reg_math_parent_edu_binary_2024.csv"))
if (has_2024) {
  coef_PI_2024   <- read.csv(file.path(out_dir, "reg_pi_index_parent_edu_binary_2024.csv"), stringsAsFactors = FALSE)
  coef_math_2024 <- read.csv(file.path(out_dir, "reg_math_parent_edu_binary_2024.csv"), stringsAsFactors = FALSE)
}

# Parse Beta_SE into numeric beta
coef_PI_2019   <- coef_PI_2019   %>% bind_cols(parse_beta_se(.$Beta_SE)) %>% rename(beta_PI = beta,   se_PI = se)
coef_PI_2023   <- coef_PI_2023   %>% bind_cols(parse_beta_se(.$Beta_SE)) %>% rename(beta_PI = beta,   se_PI = se)
coef_math_2019 <- coef_math_2019 %>% bind_cols(parse_beta_se(.$Beta_SE)) %>% rename(beta_math = beta, se_math = se)
coef_math_2023 <- coef_math_2023 %>% bind_cols(parse_beta_se(.$Beta_SE)) %>% rename(beta_math = beta, se_math = se)
if (has_2024) {
  coef_PI_2024   <- coef_PI_2024   %>% bind_cols(parse_beta_se(.$Beta_SE)) %>% rename(beta_PI = beta,   se_PI = se)
  coef_math_2024 <- coef_math_2024 %>% bind_cols(parse_beta_se(.$Beta_SE)) %>% rename(beta_math = beta, se_math = se)
}

# Keep country rows only
countries_only <- function(d) d %>% filter(!Group %in% c("Europe", "Balkans"))
coef_PI_2019   <- countries_only(coef_PI_2019)
coef_PI_2023   <- countries_only(coef_PI_2023)
coef_math_2019 <- countries_only(coef_math_2019)
coef_math_2023 <- countries_only(coef_math_2023)
if (has_2024) { coef_PI_2024 <- countries_only(coef_PI_2024); coef_math_2024 <- countries_only(coef_math_2024) }

# Merge Math and PI for each year
reg_2019 <- coef_math_2019 %>%
  select(Country = Group, beta_math) %>%
  left_join(coef_PI_2019 %>% select(Group, beta_PI), by = c("Country" = "Group")) %>%
  mutate(year = 2019)

reg_2023 <- coef_math_2023 %>%
  select(Country = Group, beta_math) %>%
  left_join(coef_PI_2023 %>% select(Group, beta_PI), by = c("Country" = "Group")) %>%
  mutate(year = 2023)

reg_all <- bind_rows(reg_2019, reg_2023)
if (has_2024) {
  reg_2024 <- coef_math_2024 %>%
    select(Country = Group, beta_math) %>%
    left_join(coef_PI_2024 %>% select(Group, beta_PI), by = c("Country" = "Group")) %>%
    mutate(year = 2024)
  reg_all <- bind_rows(reg_all, reg_2024)
}

# ---- 3. Map Country -> ISO3 using IDBAnalyzerCountries + timss_iso3_lookup ----
processed_data_dir <- "data/processed_data"
source(file.path(processed_data_dir, "IDBAnalyzerCountries.R"))  # defines country_labels

# timss_iso3_lookup from 3_eda.R
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

country_iso_tbl <- tibble(
  IDCNTRY = as.integer(country_labels),
  CountryName = names(country_labels)
) %>%
  left_join(timss_iso3_lookup, by = "IDCNTRY") %>%
  # IDBAnalyzerCountries.R repeats some CountryName with different IDCNTRY (e.g. Serbia, Romania);
  # keep one row per name, preferring a row with a resolved iso3 for the join to regression tables.
  dplyr::arrange(CountryName, dplyr::desc(!is.na(iso3))) %>%
  dplyr::distinct(CountryName, .keep_all = TRUE)

# Join ISO3 to regression table
reg_all <- reg_all %>%
  left_join(country_iso_tbl, by = c("Country" = "CountryName"))

# Manual fixes / overrides for mapping
# Belgium (Flemish / French) -> BEL; Kosovo -> KOS (to match geojson admin code)
reg_all <- reg_all %>%
  mutate(
    iso3 = case_when(
      Country %in% c("Belgium (Flemish)", "Belgium (French)") ~ "BEL",
      Country == "Kosovo" ~ "KOS",  # for joining to shapefile
      TRUE ~ iso3
    )
  )

# ---- 4. Load Europe geojson and join ----
eu <- st_read(geo_path, quiet = TRUE)

# Inspect once in R to choose the right ISO column:
# print(names(eu)); head(eu$iso_a3); head(eu$adm0_a3)
# Here assume we want adm0_a3 (3-letter ISO for most countries)
shape_iso_col <- "adm0_a3"   # or "iso_a3" / "iso_a3_eh" depending on your file

# Keep full geometry and join by year inside make_map so countries with no values stay visible in grey.
eu_join <- eu

# ---- 5. Shared bin breaks (all years comparable) + map helper ----
vals_pi_all <- reg_all$beta_PI[is.finite(reg_all$beta_PI)]
vals_math_all <- reg_all$beta_math[is.finite(reg_all$beta_math)]

# PI ~ parent edu binary: fixed bins (same legend every year)
breaks_PI <- c(-Inf, 0, 0.8, 1.6, 2.4, 3.2, Inf)
labs_PI <- c("\u2264 0", "0–0.8", "0.8–1.6", "1.6–2.4", "2.4–3.2", "> 3.2")

# Math ~ parent edu binary: quintiles on pooled country coefficients (5 bins, same cuts all years)
if (length(vals_math_all) < 2) {
  breaks_math <- c(0, 1, 2, 3, 4, 5)
} else {
  breaks_math <- as.numeric(stats::quantile(vals_math_all, probs = seq(0, 1, 0.2), na.rm = TRUE))
  breaks_math <- unique(breaks_math)
  if (length(breaks_math) < 6) {
    rng <- range(vals_math_all, na.rm = TRUE)
    breaks_math <- seq(rng[1], rng[2], length.out = 6)
  } else if (length(breaks_math) > 6) {
    breaks_math <- breaks_math[round(seq(1L, length(breaks_math), length.out = 6L))]
  }
  for (i in 2L:length(breaks_math)) {
    if (breaks_math[i] <= breaks_math[i - 1L]) breaks_math[i] <- breaks_math[i - 1L] + 1e-6
  }
}
labs_math <- vapply(seq_len(5L), function(i) {
  sprintf("%.1f-%.1f", breaks_math[i], breaks_math[i + 1L])
}, character(1))

# Base ramp; length is chosen inside make_map to match n bins (PI uses 6, Math uses 5).
pal_ramp_anchors <- c("#006837", "#31a354", "#fed976", "#fd8d3c", "#bd0026")

make_map <- function(year_value, value_col, title_main, file_stub, breaks_vec, legend_labels) {
  n_bins <- length(legend_labels)
  pal <- if (n_bins <= length(pal_ramp_anchors)) {
    pal_ramp_anchors[seq_len(n_bins)]
  } else {
    grDevices::colorRampPalette(pal_ramp_anchors)(n_bins)
  }

  reg_y <- reg_all %>% dplyr::filter(year == year_value)
  dat_y <- eu_join %>%
    left_join(reg_y, by = setNames("iso3", shape_iso_col))
  vals <- dat_y[[value_col]]
  if (all(is.na(vals))) {
    stop("No data for ", value_col, " in ", year_value)
  }

  dat_y <- dat_y %>%
    mutate(
      bin = cut(
        .data[[value_col]],
        breaks = breaks_vec,
        include.lowest = TRUE,
        right = TRUE,
        labels = legend_labels
      )
    )

  p <- ggplot(dat_y) +
    geom_sf(aes(fill = bin), color = "grey70", linewidth = 0.2) +
    scale_fill_manual(
      values = pal,
      limits = legend_labels,
      drop = FALSE,
      na.value = "grey90",
      name = "Coefficient bin",
      guide = guide_legend(title.position = "top", nrow = 1, byrow = TRUE)
    ) +
    coord_sf(xlim = c(-10, 40), ylim = c(35, 72), expand = FALSE) +
    labs(
      title = paste0(title_main, " (", year_value, ")"),
      subtitle = "Country-level regression coefficients",
      caption = "Green = lowest bin, red = highest. Grey = missing."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.subtitle.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(hjust = 0, lineheight = 1.1),
      plot.subtitle = element_text(hjust = 0),
      plot.caption = element_text(hjust = 0, size = rel(0.85)),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      # Extra right/top so long titles are not clipped by ggsave viewport.
      plot.margin = margin(t = 10, r = 28, b = 6, l = 10, unit = "pt")
    )

  out_path <- file.path(plots_dir, paste0(file_stub, "_", year_value, ".png"))
  ggsave(out_path, p, width = 9, height = 6.5, dpi = 300, bg = "white")
  message("Saved: ", out_path)
  invisible(p)
}

# Line break before "parents'..." so titles fit without clipping on export.
title_math <- "Gradient of mathematics achievement with\nparents' highest education (binary dummy)"
title_pi <- "Gradient of parental investment with\nparents' highest education (binary dummy)"

# ---- 6. Maps (shared breaks per outcome type) ----
make_map(2019, "beta_math", title_math, "map_math_parentEdu_binary", breaks_math, labs_math)
make_map(2019, "beta_PI", title_pi, "map_PI_parentEdu_binary", breaks_PI, labs_PI)

make_map(2023, "beta_math", title_math, "map_math_parentEdu_binary", breaks_math, labs_math)
make_map(2023, "beta_PI", title_pi, "map_PI_parentEdu_binary", breaks_PI, labs_PI)

if (has_2024) {
  make_map(2024, "beta_math", title_math, "map_math_parentEdu_binary", breaks_math, labs_math)
  make_map(2024, "beta_PI", title_pi, "map_PI_parentEdu_binary", breaks_PI, labs_PI)
}
