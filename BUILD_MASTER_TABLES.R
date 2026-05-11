rm(list = ls())

# ---- Configure paths (run from project root) -------------------------------
setwd("d:/CEU/policy lab/WB_TIMMS")

library(dplyr)
library(haven)
library(Hmisc)

raw_data_dir       <- "data/raw_data"
processed_data_dir <- "data/processed_data"
master_dir         <- "data/processed_data/master"

# ---- Scope (grade 4 only, years 2019/2023) ---------------------------------
grades <- c(4)

# ---- Region filtering (edit these lists as needed) ---------------------------
filter_to_europe_balkans <- TRUE
# Belgium: TIMSS reports Flemish (BFL, IDCNTRY 956) and French (BFR, IDCNTRY 957) separately; see data/bin/Data/Configuration/StudyConfiguration.xml
europe_iso3 <- c(
  "AUT","BEL","BFL","BFR","BGR","BIH","CHE","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR",
  "GRC","HRV","HUN","IRL","ISL","ITA","LIE","LTU","LUX","LVA","MKD","MNE","NLD",
  "NOR","POL","PRT","ROU","SRB","SVK","SVN","SWE","UKR","ALB"
)
# Align Is_Balkan with 1_build_master_table.R (six Western Balkans; Kosovo = XKX in UNSD-style lists)
balkan_iso3 <- c("ALB", "BIH", "MKD", "MNE", "SRB", "XKX")

# Parent education variables by year/grade (update here if codebooks change)
parent_edu_vars <- list(
  "2019" = list(
    "4" = list(mother = c("ASBH15B", "ASBH17B", "ASBH16B"), father = c("ASBH15A", "ASBH17A", "ASBH16A"))
  ),
  "2023" = list(
    "4" = list(mother = c("ASBH16B"), father = c("ASBH16A"))
  )
)

# Parental investment variables (kept for later factor analysis)
parent_investment_vars <- sprintf("ASBH01%s", LETTERS[1:18])  # ASBH01A..ASBH01R

# TIMSS Belgium split: Flemish (956) and French (957); codes BFL/BFR per StudyConfiguration.xml
timss_belgium_split <- dplyr::tibble(
  IDCNTRY = c(956, 957),
  ISO3    = c("BFL", "BFR")
)

# UNSD_codes.xlsx: IDCNTRY and ISO-alpha3 Code; used only for region filtering.
read_unsd_codes <- function() {

  xlsx_path <- file.path(processed_data_dir, "UNSD_codes.xlsx")
  if (!file.exists(xlsx_path)) stop("Missing required file: ", xlsx_path)
  df <- readxl::read_excel(xlsx_path)
  df %>%
    dplyr::transmute(
      IDCNTRY = as.numeric(IDCNTRY),
      ISO3 = as.character(`ISO-alpha3 Code`)
    ) %>%
    dplyr::filter(!is.na(IDCNTRY), !is.na(ISO3)) %>%
    dplyr::distinct(IDCNTRY, .keep_all = TRUE) %>%
    dplyr::bind_rows(timss_belgium_split)
}

unsd_codes <- NULL
allowed_idcntry <- NULL
if (filter_to_europe_balkans) {
  unsd_codes <- read_unsd_codes()
  allowed_iso3 <- unique(c(europe_iso3, balkan_iso3))
  allowed_idcntry <- unsd_codes %>%
    dplyr::filter(ISO3 %in% allowed_iso3) %>%
    dplyr::pull(IDCNTRY) %>%
    unique()
}

# Country names: use the TIMSS IDB country dictionary (IDBAnalyzerCountries.R).
# It defines country_labels = c("Austria" = 40, "Belgium (Flemish)" = 956, ...); we map IDCNTRY -> name via that.
load_country_labels <- function() {
  path <- file.path(processed_data_dir, "IDBAnalyzerCountries.R")
  if (!file.exists(path)) return(NULL)
  env <- new.env()
  tryCatch(
    source(path, local = env),
    error = function(e) { message("Could not source IDBAnalyzerCountries.R: ", conditionMessage(e)); return(NULL) }
  )
  if (!exists("country_labels", envir = env)) return(NULL)
  get("country_labels", envir = env)
}

# Map IDCNTRY code to country name using the dictionary (names = country, values = code).
idcntry_to_name <- function(code, country_labels) {
  if (is.null(country_labels) || length(country_labels) == 0) return(NA_character_)
  code <- as.numeric(code)
  idx <- match(code, country_labels)
  if (is.na(idx)) return(NA_character_)
  names(country_labels)[idx]
}

dir.create(processed_data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(master_dir,         showWarnings = FALSE, recursive = TRUE)

# TIMSS cycle code mapping (file name suffix)
# 2019 uses M7 for most countries, Z7 for bridge studies (e.g., NLD).
cycle_map <- list(
  `2019` = c("M7", "Z7"),
  `2023` = c("M8")
)
grade_prefix <- c(`4` = "AS")

detect_available_years <- function() {
  files <- list.files(raw_data_dir, pattern = "(M7|Z7|M8)\\.rdata$", ignore.case = TRUE)
  cycles <- unique(toupper(sub(".*(M7|Z7|M8)\\.rdata$", "\\1", files)))
  years <- as.integer(names(cycle_map)[sapply(cycle_map, function(v) any(v %in% cycles))])
  sort(years)
}

years <- detect_available_years()
if (length(years) == 0) {
  years <- c(2019, 2023)
}
message("Years detected in raw_data: ", paste(years, collapse = ", "))

# First variable from vars that exists in df (for year/prefix-varying names).
pick_var <- function(df, vars) {
  v <- vars[vars %in% names(df)][1]
  if (is.na(v)) return(NA_character_)
  v
}

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

# ---- SES and parental investment ----
# Standardizes variable names across years, keeps original codings in _num, builds SES index
# (TIMSS cut scores for 2019; 2023 uses provided ASDHSES). Recodes parental investment so
# higher = more investment. No change to raw achievement or weights.
calc_ses_and_investment <- function(df, year, prefix) {
  # 1) Map year-specific variable names to standard AS* names
  hedup_var <- pick_var(df, c(sprintf("%sDHEDUP", prefix), sprintf("%sDGEDUP", prefix)))
  hses_var  <- pick_var(df, c(sprintf("%sDHSES", prefix)))
  occ_var   <- pick_var(df, c(sprintf("%sDHOCCP", prefix)))
  # SES component variables (from Home Questionnaire for 2019)
  bh10_var  <- pick_var(df, c(sprintf("%sBH10", prefix)))  # Books in home (Home Q)
  bh11_var  <- pick_var(df, c(sprintf("%sBH11", prefix)))  # Children's books (Home Q)
  bg04_var  <- pick_var(df, c(sprintf("%sBG04", prefix)))  # Books in home (Student Q - fallback)
  bg05a_var <- pick_var(df, c(sprintf("%sBG05A", prefix)))
  bg05b_var <- pick_var(df, c(sprintf("%sBG05B", prefix)))
  bg05c_var <- pick_var(df, c(sprintf("%sBG05C", prefix)))
  bg05d_var <- pick_var(df, c(sprintf("%sBG05D", prefix)))
  bg05e_var <- pick_var(df, c(sprintf("%sBG05E", prefix)))
  bg05f_var <- pick_var(df, c(sprintf("%sBG05F", prefix)))

  parent_vars <- parent_edu_vars[[as.character(year)]][["4"]]
  if (is.null(parent_vars)) stop("Missing parent_edu_vars mapping for year ", year, " grade 4")
  pick_present <- function(vars) { v <- vars[vars %in% names(df)][1]; if (is.na(v)) NA_character_ else v }
  father_var <- pick_present(parent_vars$father)
  mother_var <- pick_present(parent_vars$mother)
  if (is.na(father_var)) stop("Missing father variable: ", paste(parent_vars$father, collapse = ", "), " (year ", year, ")")
  if (is.na(mother_var)) stop("Missing parent B variable: ", paste(parent_vars$mother, collapse = ", "), " (year ", year, ")")

  df$ASDHEDUP <- if (!is.na(hedup_var)) df[[hedup_var]] else NA
  df$ASDHSES  <- if (!is.na(hses_var)) df[[hses_var]] else NA
  df$ASDHOCCP <- if (!is.na(occ_var)) df[[occ_var]] else NA
  # SES component variables
  df$ASBH10   <- if (!is.na(bh10_var)) df[[bh10_var]] else NA  # Books in home (Home Q)
  df$ASBH11   <- if (!is.na(bh11_var)) df[[bh11_var]] else NA  # Children's books (Home Q)
  df$ASBG04   <- if (!is.na(bg04_var)) df[[bg04_var]] else NA  # Books in home (Student Q - fallback)
  df$ASBG05A  <- if (!is.na(bg05a_var)) df[[bg05a_var]] else NA
  df$ASBG05B  <- if (!is.na(bg05b_var)) df[[bg05b_var]] else NA
  df$ASBG05C  <- if (!is.na(bg05c_var)) df[[bg05c_var]] else NA
  df$ASBG05D  <- if (!is.na(bg05d_var)) df[[bg05d_var]] else NA
  df$ASBG05E  <- if (!is.na(bg05e_var)) df[[bg05e_var]] else NA
  df$ASBG05F  <- if (!is.na(bg05f_var)) df[[bg05f_var]] else NA

  # 2) Numeric versions: original coding preserved in _num (9/99 -> NA for ASDHSES only)
  df$ASDHEDUP_num <- as.numeric(as.character(df$ASDHEDUP))
  df$ASDHSES_num  <- as.numeric(as.character(df$ASDHSES))
  df$ASDHSES_num[df$ASDHSES_num %in% c(9, 99)] <- NA_real_
  df$ASDHOCCP_num <- as.numeric(as.character(df$ASDHOCCP))
  df$ASBH10_num   <- as.numeric(as.character(df$ASBH10))   # Books in home (Home Q)
  df$ASBH11_num   <- as.numeric(as.character(df$ASBH11))   # Children's books (Home Q)
  df$ASBG04_num   <- as.numeric(as.character(df$ASBG04))   # Books in home (Student Q - fallback)
  df$ASBG05A_num  <- as.numeric(as.character(df$ASBG05A))
  df$ASBG05B_num  <- as.numeric(as.character(df$ASBG05B))
  df$ASBG05C_num  <- as.numeric(as.character(df$ASBG05C))
  df$ASBG05D_num  <- as.numeric(as.character(df$ASBG05D))
  df$ASBG05E_num  <- as.numeric(as.character(df$ASBG05E))
  df$ASBG05F_num  <- as.numeric(as.character(df$ASBG05F))

  # 3) 2019 only: align desk/room to 2023 names (ASBG05B->ASBG05E, ASBG05C->ASBG05F) for merging
  if (year == 2019) {
    if (!is.na(bg05b_var) && "ASBG05B" %in% names(df)) { df$ASBG05E <- df$ASBG05B; df$ASBG05E_num <- df$ASBG05B_num }
    if (!is.na(bg05c_var) && "ASBG05C" %in% names(df)) { df$ASBG05F <- df$ASBG05C; df$ASBG05F_num <- df$ASBG05C_num }
  }

  map_yesno <- function(x) dplyr::case_when(x == 1 ~ 1, x == 2 ~ 0, TRUE ~ NA_real_)
  
  # 4) SES component scores (higher = higher SES). Four components: parent education, occupation, books, children's books.
  # ASDHEDUP - Parents' Highest Education Level (2019 coding):
  # Raw: 1=Uni+, 2=Post-sec, 3=Upper-sec, 4=Lower-sec, 5=Primary/None, 6=NA, 9=Omitted
  # Score: 5 (highest) for University, 1 (lowest) for Primary/None
  map_hedup_score <- function(x) dplyr::case_when(
    x == 1 ~ 5,  # University or Higher
    x == 2 ~ 4,  # Post-secondary but not University
    x == 3 ~ 3,  # Upper Secondary
    x == 4 ~ 2,  # Lower Secondary
    x == 5 ~ 1,  # Some Primary, Lower Secondary or No School
    x %in% c(6, 9) ~ NA_real_,  # Not Applicable / Omitted
    TRUE ~ NA_real_
  )
  
  # ASDHOCCP - Parents' Highest Occupation Level (2019 coding):
  # Raw: 1=Professional, 2=Small Business, 3=Clerical, 4=Skilled Worker, 5=General Laborer, 6=Never Worked, 7=NA, 9=Omitted
  # Score: 4 (highest) for Professional, 1 (lowest) for General Laborer/Never Worked
  map_occ_score <- function(x) dplyr::case_when(
    x == 1 ~ 4,  # Professional (highest)
    x == 2 ~ 3,  # Small Business Owner
    x == 3 ~ 2,  # Clerical
    x == 4 ~ 1,  # Skilled Worker
    x == 5 ~ 1,  # General Laborer (same as skilled worker for scoring)
    x == 6 ~ 1,  # Never Worked for Pay (lowest)
    x %in% c(7, 9) ~ NA_real_,  # Not Applicable / Omitted
    TRUE ~ NA_real_
  )
  
  # ASBH10 - Books in home (from Home Questionnaire):
  # Raw: 1=0-10, 2=11-25, 3=26-100, 4=101-200, 5=200+
  # Score: 5 (highest) for 200+, 1 (lowest) for 0-10
  map_books_score <- function(x) dplyr::case_when(
    x == 1 ~ 1,  # 0-10 books
    x == 2 ~ 2,  # 11-25 books
    x == 3 ~ 3,  # 26-100 books
    x == 4 ~ 4,  # 101-200 books
    x == 5 ~ 5,  # More than 200 books
    x %in% c(9) ~ NA_real_,  # Omitted or invalid
    TRUE ~ NA_real_
  )
  
  # ASBH11 - Children's books in home (from Home Questionnaire):
  # Raw: 1=0-10, 2=11-25, 3=26-50, 4=51-100, 5=100+
  # Score: 5 (highest) for 100+, 1 (lowest) for 0-10
  map_children_books_score <- function(x) dplyr::case_when(
    x == 1 ~ 1,  # 0-10 children's books
    x == 2 ~ 2,  # 11-25 children's books
    x == 3 ~ 3,  # 26-50 children's books
    x == 4 ~ 4,  # 51-100 children's books
    x == 5 ~ 5,  # More than 100 children's books
    x %in% c(9) ~ NA_real_,  # Omitted or invalid
    TRUE ~ NA_real_
  )

  # Calculate SES component scores per TIMSS methodology
  # Four components: books (ASBH10), children's books (ASBH11), parent education (ASDHEDUP), parent occupation (ASDHOCCP)
  df$parent_edu_score <- map_hedup_score(df$ASDHEDUP_num)           # Scale 1-5
  df$parent_occ_score <- map_occ_score(df$ASDHOCCP_num)             # Scale 1-4
  # Use Home Q books (ASBH10) if available, otherwise fall back to Student Q (ASBG04)
  df$books_score <- dplyr::if_else(
    !is.na(df$ASBH10_num), 
    map_books_score(df$ASBH10_num),
    map_books_score(df$ASBG04_num)
  )
  df$children_books_score <- map_children_books_score(df$ASBH11_num)

  # 5) Home possessions (0/1); not used in official SES index
  df$home_computer <- map_yesno(df$ASBG05A_num)
  df$home_internet <- map_yesno(df$ASBG05D_num)
  df$home_desk <- map_yesno(df$ASBG05E_num)
  df$home_room <- map_yesno(df$ASBG05F_num)
  poss_mat <- cbind(df$home_computer, df$home_desk, df$home_room, df$home_internet)
  df$home_possessions_score <- ifelse(rowSums(!is.na(poss_mat)) == 0, NA_real_, rowSums(poss_mat, na.rm = TRUE))

  # 6) Binary parent education and household SES (for analyses)
  df$parentB_raw <- as.numeric(as.character(df[[mother_var]]))
  df$parentB_high_edu <- dplyr::case_when(
    df$parentB_raw %in% 4:8 ~ 1,
    df$parentB_raw %in% 1:3 ~ 0,
    TRUE ~ NA_real_
  )
  df$ASDHEDUP_binary <- dplyr::case_when(
    df$ASDHEDUP_num %in% 1:3 ~ 1,
    df$ASDHEDUP_num %in% 4:5 ~ 0,
    TRUE ~ NA_real_
  )

  # 7) Impute missing SES components: school mean, then country mean
  impute_component <- function(d, value_col, group_school, group_country) {
    d <- d %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_school))) %>%
      dplyr::mutate(school_mean = mean(.data[[value_col]], na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_country))) %>%
      dplyr::mutate(country_mean = mean(.data[[value_col]], na.rm = TRUE)) %>%
      dplyr::ungroup()
    d[[value_col]] <- ifelse(
      is.na(d[[value_col]]) & !is.na(d$school_mean),
      d$school_mean,
      ifelse(is.na(d[[value_col]]) & !is.na(d$country_mean), d$country_mean, d[[value_col]])
    )
    d %>% dplyr::select(-school_mean, -country_mean)
  }

  group_school <- intersect(c("year", "IDCNTRY", "IDSCHOOL"), names(df))
  group_country <- intersect(c("year", "IDCNTRY"), names(df))
  if (length(group_school) == 0) group_school <- group_country

  # Impute missing SES components with school-year mean, then country-year mean
  for (comp in c("parent_edu_score", "parent_occ_score", "books_score", "children_books_score")) {
    df <- impute_component(df, comp, group_school, group_country)
  }

  # Calculate SES raw score as sum of 4 components
  # Max possible: 5 (edu) + 4 (occ) + 5 (books) + 5 (child books) = 19
  # Min possible: 1 + 1 + 1 + 1 = 4
  ses_mat <- cbind(df$parent_edu_score, df$parent_occ_score, df$books_score, df$children_books_score)
  df$SES_index_raw <- ifelse(rowSums(!is.na(ses_mat)) == 0, NA_real_, rowSums(ses_mat, na.rm = TRUE))

  # TIMSS SES Cut Scores (from official methodology):
  # Higher: score >= 11.1 (>25 books, >25 children's books, uni edu, professional occ)
  # Lower: score < 8.7 (<=25 books, <=25 children's books, <=upper-sec edu, no prof/clerical/business)
  # Middle: 8.7 <= score < 11.1
  SES_CUT_HIGHER <- 11.1
  SES_CUT_LOWER <- 8.7

  # 2023 uses ASDHSES directly; 2019 is computed using TIMSS cut scores
  if (year == 2019) {
    # Apply official TIMSS cut scores
    # Higher SES: score >= 11.1
    # Middle SES: 8.7 <= score < 11.1
    # Lower SES: score < 8.7
    df$SES_index <- dplyr::case_when(
      df$SES_index_raw >= SES_CUT_HIGHER ~ 3L,  # Higher
      df$SES_index_raw >= SES_CUT_LOWER ~ 2L,   # Middle
      df$SES_index_raw < SES_CUT_LOWER ~ 1L,    # Lower
      TRUE ~ NA_integer_
    )
    # Match TIMSS ASDHSES direction (1=Higher, 2=Middle, 3=Lower)
    df$ASDHSES_num <- dplyr::case_when(
      df$SES_index == 3 ~ 1L,  # Higher
      df$SES_index == 2 ~ 2L,  # Middle
      df$SES_index == 1 ~ 3L,  # Lower
      TRUE ~ NA_integer_
    )
    df$ASDHSES <- df$ASDHSES_num
  }

  df$ASDHSES_source <- ifelse(year == 2023, "provided_2023", "computed_2019")
  if (year == 2023) {
    df$SES_index <- dplyr::case_when(
      df$ASDHSES_num == 1 ~ 3, df$ASDHSES_num == 2 ~ 2, df$ASDHSES_num == 3 ~ 1, TRUE ~ NA_real_
    )
  }

  # ============================================================================
  # Recode parental investment variables (ASBH01A-ASBH01R)
  # Original coding: 1=Often, 2=Sometimes, 3=Never
  # Recoded to: 1=Never, 2=Sometimes, 3=Often
  # This ensures higher values = more parental investment
  # Higher beta coefficients will mean higher investment → higher achievement
  # ============================================================================
  recode_parental_investment <- function(x) {
    dplyr::case_when(
      x == 1 ~ 3,  # Often → 3 (highest)
      x == 2 ~ 2,  # Sometimes → 2 (middle)
      x == 3 ~ 1,  # Never → 1 (lowest)
      TRUE ~ as.numeric(x)  # Keep missing codes (9, 99, etc.) and NA as-is
    )
  }
  
  # Apply recoding to all parental investment variables that exist in the data
  for (item in parent_investment_vars) {
    if (item %in% names(df)) {
      df[[item]] <- recode_parental_investment(df[[item]])
    }
  }

  df
}

# ---- PV (plausible value) percentiles ----
# IMPORTANT: TIMSS provides 5 plausible values per
# student per domain (ASMMAT01–05, ASSSCI01–05, etc.) in the RData; they are produced by TIMSS
# using IRT and multiple imputation. We only use those PVs to compute percentile RANKS.
#
# In WB_ANALYSIS.R, regressions use these percentiles as the outcome (e.g. ASMMAT01_ptile_region ~ SES).
# For each of the 5 PVs we get one regression; (1) jackknife (JKZONE, JKREP, JK2) gives the SE
# for each regression; (2) Rubin's rules combine the 5 point estimates and SEs into one estimate
# and total SE (imputation variance + sampling variance).
#
# This function adds two percentile ranks for each PV, using TOTWGT so ranks reflect the population.
# Output (per PV, e.g. ASMMAT01_ptile_region):
#   _ptile_region  = percentile (1–100) within the full sample.
#   _ptile_country = percentile (1–100) within the student's country.
# Method: weighted quantiles at 0%,1%,...,100% → cutpoints → cut() assigns each score to a bin (rank).
add_pv_percentiles <- function(df, prefix) {
  # Weighted quantiles; requires finite x and w, w > 0. Returns NA if too few valid rows.
  safe_wtd_quantile <- function(x, w, probs) {
    ok <- is.finite(x) & is.finite(w) & (w > 0)
    if (sum(ok) < 2) return(rep(NA_real_, length(probs)))
    Hmisc::wtd.quantile(x[ok], weights = w[ok], probs = probs, na.rm = TRUE)
  }

  # Four PV domains: math (MMAT), science (SSCI), math reasoning (MREA), science reasoning (SREA)
  pv_bases <- c(
    paste0(prefix, "MMAT"),
    paste0(prefix, "SSCI"),
    paste0(prefix, "MREA"),
    paste0(prefix, "SREA")
  )
  probs <- seq(0, 1, 0.01)   # 101 points → percentiles 0–100

  for (base in pv_bases) {
    pv_vars <- sprintf("%s%02d", base, 1:5)   # e.g. ASMMAT01 .. ASMMAT05
    if (!all(pv_vars %in% names(df))) next

    # --- Region percentile: rank within full sample (all countries) ---
    for (pv in pv_vars) {
      q <- safe_wtd_quantile(df[[pv]], df$TOTWGT, probs)
      q <- unique(q)   # merge duplicate cutpoints so cut() gets valid breaks
      df[[paste0(pv, "_ptile_region")]] <- if (length(q) < 2) NA_integer_ else cut(df[[pv]], breaks = q, labels = FALSE, include.lowest = TRUE)
    }

    # --- Country percentile: rank within each country (split by IDCNTRY, then recombine) ---
    split_df <- split(df, df$IDCNTRY)
    split_df <- lapply(split_df, function(d) {
      for (pv in pv_vars) {
        q <- safe_wtd_quantile(d[[pv]], d$TOTWGT, probs)
        q <- unique(q)
        d[[paste0(pv, "_ptile_country")]] <- if (length(q) < 2) NA_integer_ else cut(d[[pv]], breaks = q, labels = FALSE, include.lowest = TRUE)
      }
      d
    })
    df <- bind_rows(split_df)
  }
  df
}

# ---- Build one year × grade ----
# Loads all country RData files for the given year/grade, merges achievement + context + home
# questionnaires, attaches country names and (optionally) region flags, then runs SES and PV percentiles.
# Returns one data frame with all countries for that year/grade, or NULL if no data.

# Helper: load and merge one country's data (achievement + context + home). Returns list(merged_df, country_row)
# or NULL if achievement file missing. country_labels = named vector from IDBAnalyzerCountries.R (name = country, value = code).
load_and_merge_one_country <- function(ach_base, ctx_type, home_type, ach_keep, bg_keep, year, grade, country_labels) {
  country_code <- substr(ach_base, 4, 6)
  cycle_code <- substr(ach_base, nchar(ach_base) - 1, nchar(ach_base))
  ctx_base <- paste0(ctx_type, country_code, cycle_code)
  home_base <- if (!is.na(home_type)) paste0(home_type, country_code, cycle_code) else NA_character_

  ach_df <- load_idb_data(ach_base)
  if (is.null(ach_df)) return(NULL)

  # Country name from TIMSS IDB dictionary (IDBAnalyzerCountries.R), not from RData attributes
  idcntry_val <- as.numeric(ach_df$IDCNTRY[1])
  country_name <- idcntry_to_name(idcntry_val, country_labels)
  country_row <- dplyr::tibble(IDCNTRY = idcntry_val, CountryName = as.character(country_name))

  ctx_df <- load_idb_data(ctx_base)
  home_df <- if (!is.na(home_base)) load_idb_data(home_base) else NULL
  ach_df <- dplyr::select(ach_df, dplyr::any_of(ach_keep))
  if (!is.null(ctx_df)) ctx_df <- dplyr::select(ctx_df, dplyr::any_of(bg_keep))
  if (!is.null(home_df)) home_df <- dplyr::select(home_df, dplyr::any_of(bg_keep))

  if (requireNamespace("haven", quietly = TRUE)) {
    ach_df <- ach_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ haven::zap_labels(.x)))
    if (!is.null(ctx_df)) ctx_df <- ctx_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ haven::zap_labels(.x)))
    if (!is.null(home_df)) home_df <- home_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ haven::zap_labels(.x)))
  }

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

  merged_df <- merged_df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
  merged_df$IDCNTRY <- as.numeric(merged_df$IDCNTRY)
  merged_df$year <- year
  merged_df$grade <- grade
  list(merged_df = merged_df, country_row = country_row)
}

# Build country-names lookup from collected labels; Belgium (Flemish/French) get explicit display names.
build_country_names_lookup <- function(country_rows) {
  out <- dplyr::bind_rows(country_rows) %>% dplyr::distinct(IDCNTRY, .keep_all = TRUE)
  belgium <- dplyr::tibble(IDCNTRY = c(956, 957), CountryName = c("Belgium (Flemish)", "Belgium (French)"))
  out %>% dplyr::filter(!IDCNTRY %in% c(956, 957)) %>% dplyr::bind_rows(belgium)
}

build_year_grade <- function(year, grade) {
  # --- 1) Resolve year/grade to TIMSS cycle and variable prefix ---
  cycles <- cycle_map[[as.character(year)]]
  prefix <- grade_prefix[as.character(grade)]
  if (is.null(cycles) || is.na(prefix)) stop("Missing cycle or grade prefix mapping.")
  ach_type <- if (grade == 4) "ASA" else "BSA"
  ctx_type <- if (grade == 4) "ASG" else "BSG"
  home_type <- if (grade == 4) "ASH" else NA_character_

  # --- 2) Find achievement RData files (one per country) ---
  ach_patterns <- sprintf("^%s...%s\\.rdata$", ach_type, cycles)
  ach_files <- unique(unlist(lapply(ach_patterns, function(pat) {
    list.files(raw_data_dir, pattern = pat, full.names = FALSE, ignore.case = TRUE)
  })))
  if (length(ach_files) == 0) {
    message("No achievement files found for year ", year, " grade ", grade)
    return(NULL)
  }

  # --- 3) Columns to keep: achievement (PVs, weights, IDs) and background (SES, home, parent edu) ---
  pv_math <- sprintf("%sMMAT%02d", prefix, 1:5)
  pv_sci  <- sprintf("%sSSCI%02d", prefix, 1:5)
  pv_reason_math <- sprintf("%sMREA%02d", prefix, 1:5)
  pv_reason_sci  <- sprintf("%sSREA%02d", prefix, 1:5)
  ach_keep <- unique(c(
    "IDCNTRY", "IDSCHOOL", "IDSTRATE", "IDSTUD", "TOTWGT", "JKZONE", "JKREP",
    pv_math, pv_sci, pv_reason_math, pv_reason_sci
  ))
  bg_keep <- unique(c(
    "IDCNTRY", "IDSTUD",
    sprintf("%sDHEDUP", prefix), sprintf("%sDGEDUP", prefix), sprintf("%sDHSES", prefix), sprintf("%sDHOCCP", prefix),
    sprintf("%sBH10", prefix), sprintf("%sBH11", prefix), sprintf("%sBG04", prefix),
    sprintf("%sBG05A", prefix), sprintf("%sBG05B", prefix), sprintf("%sBG05C", prefix),
    sprintf("%sBG05D", prefix), sprintf("%sBG05E", prefix), sprintf("%sBG05F", prefix),
    sprintf("%sBH15A", prefix), sprintf("%sBH15B", prefix), sprintf("%sBH16A", prefix),
    sprintf("%sBH16B", prefix), sprintf("%sBH17A", prefix), sprintf("%sBH17B", prefix),
    parent_investment_vars
  ))

  # --- 4) Load country dictionary and merge each country; collect merged data and country-name rows ---
  country_labels <- load_country_labels()
  if (is.null(country_labels)) message("IDBAnalyzerCountries.R not found or invalid; CountryName may be NA.")
  dfs <- list()
  country_names_list <- list()
  for (f in ach_files) {
    ach_base <- toupper(sub("\\.rdata$", "", f, ignore.case = TRUE))
    res <- load_and_merge_one_country(ach_base, ctx_type, home_type, ach_keep, bg_keep, year, grade, country_labels)
    if (is.null(res)) next
    merged_df <- res$merged_df
    country_names_list[[length(country_names_list) + 1]] <- res$country_row
    if (filter_to_europe_balkans && !is.null(allowed_idcntry)) {
      merged_df <- merged_df %>% dplyr::filter(IDCNTRY %in% allowed_idcntry)
      if (nrow(merged_df) == 0) next
    }
    dfs[[length(dfs) + 1]] <- merged_df
  }

  if (length(dfs) == 0) {
    message("No student data found for year ", year, " grade ", grade)
    return(NULL)
  }

  # --- 5) Combine all countries and attach country names (from RData labels; Belgium overridden) ---
  df_all <- dplyr::bind_rows(dfs)
  country_names <- build_country_names_lookup(country_names_list)
  df_all <- df_all %>% dplyr::left_join(country_names, by = "IDCNTRY")

  # --- 6) If region filter is on: add ISO3, Europe, Is_Balkan and keep only matched countries ---
  if (filter_to_europe_balkans && !is.null(unsd_codes)) {
    df_all <- df_all %>%
      dplyr::left_join(unsd_codes, by = "IDCNTRY") %>%
      dplyr::filter(!is.na(ISO3)) %>%
      dplyr::mutate(Europe = 1L, Is_Balkan = ifelse(ISO3 %in% balkan_iso3, 1L, 0L))
  }

  if (nrow(df_all) > 0) {
    message("Head of merged year data (", year, " grade ", grade, "):")
    print(utils::head(df_all, 3))
  }
  dfs <- NULL
  gc(verbose = FALSE)

  # --- 7) SES/index and PV percentiles (same logic for all years) ---
  df_all <- calc_ses_and_investment(df_all, year, prefix)
  df_all <- add_pv_percentiles(df_all, prefix)
  df_all
}

build_master_table <- function(grade) {
  master <- NULL
  for (yr in years) {
    message("Building year ", yr, " grade ", grade)
    df_year <- build_year_grade(yr, grade)
    if (!is.null(df_year)) {
      master <- dplyr::bind_rows(master, df_year)
      df_year <- NULL
      gc(verbose = FALSE)
    }
  }
  if (is.null(master) || nrow(master) == 0) {
    stop("No data found for grade ", grade, " in raw_data.")
  }
  if (requireNamespace("sjlabelled", quietly = TRUE)) {
    master <- sjlabelled::remove_all_labels(master)
  }
  # CountryName already present from RData labels in build_year_grade

  message("Master table complete: ", nrow(master), " rows, ", length(unique(master$IDCNTRY)), " countries")
  master
}

for (g in grades) {
  master_table <- build_master_table(g)
  obj_name <- sprintf("master_table_grade_%d", g)
  assign(obj_name, master_table)
  save(list = obj_name, file = file.path(master_dir, sprintf("master_table_grade_%d.RData", g)))
  message("Saved ", obj_name)
}
