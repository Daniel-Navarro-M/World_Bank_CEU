# =============================================================================
# 2. Objective Builder – recode master, create SES and PI variables
# Loads master_raw (raw), recodes, creates SES_index, SES_binary, parents_edu_bi,
# PI_factor, PI_factor_p, PI_index, PI_read, PI_math, PV percentiles.
# Saves master_processed and master_longitudinal_processed.
# =============================================================================

rm(list = ls())
library(dplyr)
library(haven)
library(lavaan)
library(Hmisc)
library(readr)

master_dir <- "data/processed_data/master"
processed_data_dir <- "data/processed_data"
dir.create(processed_data_dir, showWarnings = FALSE, recursive = TRUE)

parent_investment_vars <- sprintf("ASBH01%s", LETTERS[1:18])
parental_items <- parent_investment_vars

dict_path <- file.path("docs", "dictionary_master_longitudinal.csv")
dict_raw <- readr::read_csv(dict_path, show_col_types = FALSE)
names(dict_raw) <- gsub("[\r\n]+", " ", names(dict_raw))
clean_label <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_+|_+$", "", x)
}
dict_label_clean <- dict_raw %>%
  transmute(
    label_clean = clean_label(`Label_clean`),
    both_years = trimws(as.character(`Both Years`)),
    var_2023 = toupper(trimws(as.character(`Variable Name (2023)`))),
    var_2024 = toupper(trimws(as.character(`Variable Name (2024)`)))
  ) %>%
  filter(
    both_years == "\u2713",
    nzchar(label_clean),
    nzchar(var_2023),
    nzchar(var_2024),
    !(var_2023 %in% parent_investment_vars),
    !(var_2024 %in% parent_investment_vars)
  ) %>%
  pull(label_clean) %>%
  unique()

# IDCNTRY, IDSTUD = Country ID, Student ID
# ASDHEDUP = Parents' Highest Education Level (1: University or Higher; 2: Post-Secondary Education but not University; 
# 3: Upper Secondary; 4: Lower Secondary; 
# 5: Some Primary or Lower Secondary or Did not go to School; 6: Not Applicable; .: Omitted or invalid; .A: Not)

# ASDHSES = Home Socioeconomic Status Index (2023: 1: Higher; 2: Middle; 3: Lower; .: Omitted or invalid; .A: Not administered)

# ASDHOCCP = Parents' Highest Occupation (1: Professional; 2: Small Business Owner; 3: Clerical; 4: Skilled Worker; 
# 5: General Laborer; 6: Never Worked for Pay; 7: Not Applicable; .: Omitted or invalid; .A: Not administered)

# ASBH10 = Books in home, Home Q (1: 0–10; 2: 11–25; 3: 26–100; 4: 101–200; 5: More than 200)

# ASBH11 = Children's books in home, Home Q (1: 0–10; 2: 11–25; 3: 26–50; 4: 51–100; 5: More than 100)

# ASBH15A = 2019 GEN\LVL OF EDUCATION\<PARENT/GUARDIAN A> (1: Did not go to school; 
# 2: Some <Primary education—ISCED Level 1 or Lower secondary education—ISCED Level 2>; 
# 3: <Lower secondary education—ISCED Level 2>; 4: <Upper secondary education—ISCED Level 3>; 
# 5: <Post-secondary, non-tertiary education—ISCED Level 4>; 6: <Short-cycle tertiary education—ISCED Level 5>; 
# 7: <Bachelor’s or equivalent level—ISCED Level 6>; 
# 8: <Postgraduate degree: Master’s—ISCED Level 7 or Doctor—ISCED Level 8>; 9: Not applicable, 99: Omitted or invalid; Sysmis: Not administered)

# ASBH15B = 2019 GEN\LVL OF EDUCATION\<PARENT/GUARDIAN B> (same coding as before)

# ASBH16A = 2023 GEN\LVL OF EDUCATION\<PARENT/GUARDIAN A> (1: Did not go to school; 
# 2: Some <Primary education—ISCED Level 1 or Lower secondary education—ISCED Level 2>; 
# 3: <Lower secondary education—ISCED Level 2>; 4: <Upper secondary education—ISCED Level 3>; 
# 5: <Post-secondary, non-tertiary education—ISCED Level 4>; 6: <Short-cycle tertiary education—ISCED Level 5>; 
# 7: <Bachelor’s or equivalent level—ISCED Level 6>; 
# 8: <Postgraduate degree: Master’s—ISCED Level 7 or Doctor—ISCED Level 8>; 9: Not applicable, 99: Omitted or invalid; Sysmis: Not administered)

# ASBH16B = 2023 GEN\LVL OF EDUCATION\<PARENT/GUARDIAN B> (same coding as before)


# ASBH17A,B = 2019 GEN\WHAT KIND OF MAIN JOB\<PARENT/GUARDIAN A and B> (1: Has never worked for pay; 2: Small Business Owner; 
# 3: Clerical Worker; 4: Service or Sales Worker; 5: Skilled Agricultural or Fishery Worker; 
# 6: Craft or Trade Worker; 7: Plant or Machine Operator; 8: General Laborers; 
# 9: Corporate Manager or Senior Official; 10: Professional; 
# 11: Technician or Associate Professional; 12: Not applicable, 99: Omitted or invalid; Sysmis: Not administered)

# ASBH18A, B = 2023 GEN\WHAT KIND OF MAIN JOB\<PARENT/GUARDIAN A and B> (1: Has never worked for pay; 2: Small Business Owner; 
# 3: Clerical Worker; 4: Service or Sales Worker; 5: Skilled Agricultural or Fishery Worker; 6: Craft or Trade Worker; 
# 7: Plant or Machine Operator; 8: General Laborers; 9: Corporate Manager or Senior Official; 10: Professional; 
# 11: Technician or Associate Professional; 12: Not applicable, 99: Omitted or invalid; Sysmis: Not administered)

# ASBH01A-R = Parental investment items (1=Often, 2=Sometimes, 3=Never, 9: Omitted or invalid; Sysmis: Not administered)

# ---- SES: books (ASBH10), children books (ASBH11), education, occupation ----
# 998=Omitted/invalid, 999=Not administered - keep as distinct codes in recoded vars for EDA; treat as missing only for scoring (SES_index).
# parent_edu_score: 1=primary/none .. 5=university+ (higher=better); 998 and 999 left as-is for EDA

# ---- Build objectives + return refactored master ----
build_master_refactored <- function(dat) {
  # Do not convert CountryName/IDSTUD to numeric.
  # IDSTUD is a global character key "<IDCNTRY>_<IDSTUD>" from build_master_table.
  dat[] <- lapply(dat, function(x) if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x)
  if ("IDSTUD" %in% names(dat)) dat$IDSTUD <- as.character(dat$IDSTUD)
  cols_num <- setdiff(names(dat), c("CountryName", "IDSTUD"))
  dat[cols_num] <- lapply(dat[cols_num], function(x) suppressWarnings(as.numeric(x)))

  # ---- SES: books (ASBH10), children books (ASBH11), education, occupation ----
  dat$parent_edu_score <- case_when(
    dat$ASDHEDUP == 1 ~ 5, dat$ASDHEDUP == 2 ~ 4, dat$ASDHEDUP == 3 ~ 3,
    dat$ASDHEDUP == 4 ~ 2, dat$ASDHEDUP == 5 ~ 1,
    dat$ASDHEDUP == 998 ~ 998, dat$ASDHEDUP == 999 ~ 999,
    TRUE ~ NA_real_
  )

  dat$parent_edu_binary <- case_when(
    dat$parent_edu_score %in% 1:3 ~ 0,
    dat$parent_edu_score %in% 4:5 ~ 1,
    dat$parent_edu_score == 998 ~ 998, dat$parent_edu_score == 999 ~ 999,
    TRUE ~ NA_real_
  )

  dat$parent_occ_score <- case_when(
    dat$ASDHOCCP == 1 ~ 4, dat$ASDHOCCP == 2 ~ 3, dat$ASDHOCCP == 3 ~ 2,
    dat$ASDHOCCP %in% c(4, 5, 6) ~ 1,
    dat$ASDHOCCP == 998 ~ 998, dat$ASDHOCCP == 999 ~ 999,
    TRUE ~ NA_real_
  )

  dat$books_src_tmp <- if ("ASBH10" %in% names(dat)) dat$ASBH10 else dat$home_books_count
  dat$children_books_src_tmp <- if ("ASBH11" %in% names(dat)) dat$ASBH11 else dat$children_books_count

  dat$books_score <- case_when(
    dat$books_src_tmp %in% 1:5 ~ as.numeric(dat$books_src_tmp),
    dat$books_src_tmp == 998 ~ 998, dat$books_src_tmp == 999 ~ 999,
    TRUE ~ NA_real_
  )
  dat$children_books_score <- case_when(
    dat$children_books_src_tmp %in% 1:5 ~ as.numeric(dat$children_books_src_tmp),
    dat$children_books_src_tmp == 998 ~ 998, dat$children_books_src_tmp == 999 ~ 999,
    TRUE ~ NA_real_
  )
  dat$books_src_tmp <- NULL
  dat$children_books_src_tmp <- NULL

  ses_cols <- c("parent_edu_score", "parent_occ_score", "books_score", "children_books_score")
  dat$SES_index_raw <- rowSums(sapply(dat[, ses_cols], function(x) replace(x, x %in% c(998, 999), NA)), na.rm = TRUE)
  # SES cutpoints: higher >= 11.1; middle >= 8.7; else low.
  dat$SES_index <- case_when(
    dat$SES_index_raw >= 11.1 ~ 2,
    dat$SES_index_raw >= 8.7 ~ 1,
    dat$SES_index_raw < 8.7 ~ 0,
    TRUE ~ NA_real_
  )
  dat$SES_binary <- ifelse(dat$SES_index == 2, 1, ifelse(dat$SES_index %in% c(0, 1), 0, NA_real_))

  dat$parentB_edu_bi <- case_when(
    dat$parentB_edu >= 4 & dat$parentB_edu <= 8 ~ 1,
    dat$parentB_edu >= 1 & dat$parentB_edu <= 3 ~ 0,
    dat$parentB_edu == 998 ~ 998, dat$parentB_edu == 999 ~ 999,
    TRUE ~ NA_real_
  )

  # ---- PI: recode raw 1=Often, 2=Sometimes, 3=Never -> 2, 1, 0 (higher = more); 998 and 999 kept for EDA ----
  for (item in parent_investment_vars) {
    if (!item %in% names(dat)) next
    dat[[item]] <- case_when(
      dat[[item]] == 1 ~ 2, dat[[item]] == 2 ~ 1, dat[[item]] == 3 ~ 0,
      dat[[item]] == 998 ~ 998, dat[[item]] == 999 ~ 999,
      TRUE ~ as.numeric(dat[[item]])
    )
  }

  # PI_index: treat 998/999 as missing and sum valid "often" answers only.
  pi_index_mat <- as.data.frame(dat[, parent_investment_vars])
  pi_index_mat[] <- lapply(pi_index_mat, function(x) replace(as.numeric(x), x %in% c(998, 999), NA_real_))
  dat$PI_index <- rowSums(pi_index_mat == 2, na.rm = TRUE)
  # Allow partial non-missing PI responses; all-missing rows remain NA.
  any_valid_pi_index <- rowSums(!is.na(pi_index_mat), na.rm = TRUE) > 0
  dat$PI_index[!any_valid_pi_index] <- NA_real_

  dat$PI_binary <- case_when(
    dat$PI_index > 9 ~ 1,
    dat$PI_index >= 0 & dat$PI_index <= 9 ~ 0,
    dat$PI_index == 998 ~ 998, dat$PI_index == 999 ~ 999,
    TRUE ~ NA_real_
  )

  pi_read_vars <- paste0("ASBH01", LETTERS[1:9])
  pi_math_vars <- paste0("ASBH01", LETTERS[10:18])

  count_2_read <- rowSums(dat[, pi_read_vars] == 2, na.rm = TRUE)
  any_valid_read <- rowSums(!is.na(dat[, pi_read_vars]) &
                              !dat[, pi_read_vars] %in% c(998, 999), na.rm = TRUE) > 0
  dat$PI_read <- ifelse(any_valid_read, count_2_read, NA)
  has_998_r <- rowSums(dat[, pi_read_vars] == 998, na.rm = TRUE) > 0
  has_999_r <- rowSums(dat[, pi_read_vars] == 999, na.rm = TRUE) > 0
  dat$PI_read[has_998_r] <- 998
  dat$PI_read[has_999_r & !has_998_r] <- 999

  count_2_math <- rowSums(dat[, pi_math_vars] == 2, na.rm = TRUE)
  any_valid_math <- rowSums(!is.na(dat[, pi_math_vars]) &
                              !dat[, pi_math_vars] %in% c(998, 999), na.rm = TRUE) > 0
  dat$PI_math <- ifelse(any_valid_math, count_2_math, NA)
  has_998_m <- rowSums(dat[, pi_math_vars] == 998, na.rm = TRUE) > 0
  has_999_m <- rowSums(dat[, pi_math_vars] == 999, na.rm = TRUE) > 0
  dat$PI_math[has_998_m] <- 998
  dat$PI_math[has_999_m & !has_998_m] <- 999

  # ---- PI_factor + PI_factor_p ----
  # Factor-like score based on average intensity across PI items (0-2), with 998/999 treated as missing.
  pi_mat <- as.data.frame(dat[, parent_investment_vars])
  pi_mat[] <- lapply(pi_mat, function(x) replace(as.numeric(x), x %in% c(998, 999), NA_real_))
  any_valid_pi <- rowSums(!is.na(pi_mat)) > 0
  dat$PI_factor <- ifelse(any_valid_pi, rowMeans(pi_mat, na.rm = TRUE), NA_real_)

  # Percentile bins (1-100) of PI_factor within each year (weighted).
  dat$PI_factor_p <- NA_integer_
  probs <- seq(0, 1, 0.01)
  for (yr in sort(unique(dat$year))) {
    idx <- which(dat$year == yr & !is.na(dat$TOTWGT) & dat$TOTWGT > 0 & is.finite(dat$PI_factor))
    if (length(idx) < 100) next
    q <- try(unique(Hmisc::wtd.quantile(dat$PI_factor[idx], dat$TOTWGT[idx], probs, na.rm = TRUE)), silent = TRUE)
    if (inherits(q, "try-error") || length(q) < 2) next
    dat$PI_factor_p[idx] <- as.integer(cut(dat$PI_factor[idx], breaks = q, include.lowest = TRUE, labels = FALSE))
  }

  # ---- PV percentiles ----
  pv_bases <- c("ASMMAT", "ASSSCI", "ASMREA", "ASSREA")
  probs <- seq(0, 1, 0.01)
  for (base in pv_bases) {
    pv_vars <- sprintf("%s%02d", base, 1:5)
    for (pv in pv_vars) {
      q <- try(unique(Hmisc::wtd.quantile(dat[[pv]], dat$TOTWGT, probs, na.rm = TRUE)), silent = TRUE)
      dat[[paste0(pv, "_ptile_region")]] <- if (inherits(q, "try-error")) NA_integer_ else as.integer(cut(dat[[pv]], breaks = q, include.lowest = TRUE, labels = FALSE))
    }
    country_splits <- split(dat, dat$IDCNTRY)
    for (i in seq_along(country_splits)) {
      d <- country_splits[[i]]
      for (pv in pv_vars) {
        q <- try(unique(Hmisc::wtd.quantile(d[[pv]], d$TOTWGT, probs, na.rm = TRUE)), silent = TRUE)
        d[[paste0(pv, "_ptile_country")]] <- if (inherits(q, "try-error")) NA_integer_ else as.integer(cut(d[[pv]], breaks = q, include.lowest = TRUE, labels = FALSE))
      }
      country_splits[[i]] <- d
    }
    dat <- bind_rows(country_splits)
  }

  # Ensure dictionary-driven columns exist in both processed outputs for schema consistency.
  missing_dict_cols <- setdiff(dict_label_clean, names(dat))
  for (v in missing_dict_cols) dat[[v]] <- NA_real_

  # ---- master_refactored: refactored columns only ----
  id_cols <- c("IDCNTRY", "IDSCHOOL", "IDSTUD", "CountryName", "year", "grade", "Is_balkan")
  wgt_cols <- c("TOTWGT", "JKZONE", "JKREP")
  pv_ptile <- grep("_ptile_", names(dat), value = TRUE)
  pv_math_raw <- sprintf("ASMMAT%02d", 1:5)
  refactored_cols <- c(
    "SES_index", "SES_binary", "parent_edu_score", "parent_occ_score", "books_score", "children_books_score",
    "parentB_edu_bi", "PI_factor", "PI_factor_p", "PI_index", "PI_binary", "PI_read", "PI_math",
    "parent_edu_binary", "parentA_edu", "parentB_edu", "parentA_occ", "parentB_occ", "ASDHEDUP", "ASDHOCCP",
    "home_books_count", "children_books_count", "resources_computer", "resources_tablet", "resources_internet",
    "home_resources_learning_rev",
    "env_discuss_rev", "env_read_info_rev", "env_save_resources_rev", "env_time_nature_rev", "env_encourage_action_rev",
    "gen_agree_included_rev", "gen_agree_safe_env_rev", "gen_agree_cares_progress_rev", "gen_agree_keeps_informed_rev",
    "gen_agree_promotes_standards_rev", "gen_agree_helps_reading_rev", "gen_agree_helps_math_rev", "gen_agree_helps_science_rev",
    parent_investment_vars
  )
  legacy_coded_cols <- c(
    "ASDGHRL",
    "ASBH10", "ASBH11", "ASBH12A", "ASBH12B", "ASBH12C",
    "ASBH09A", "ASBH09B", "ASBH09C", "ASBH09D", "ASBH09E",
    "ASBH09A_rev", "ASBH09B_rev", "ASBH09C_rev", "ASBH09D_rev", "ASBH09E_rev",
    "ASBH08A", "ASBH08B", "ASBH08C", "ASBH08D", "ASBH08E", "ASBH08F", "ASBH08G", "ASBH08H",
    "ASBH08A_rev", "ASBH08B_rev", "ASBH08C_rev", "ASBH08D_rev", "ASBH08E_rev", "ASBH08F_rev", "ASBH08G_rev", "ASBH08H_rev"
  )
  dict_extra_cols <- setdiff(dict_label_clean, c(id_cols, wgt_cols, refactored_cols, pv_ptile, pv_math_raw))
  dat %>% select(any_of(c(id_cols, wgt_cols, refactored_cols, legacy_coded_cols, dict_extra_cols, pv_ptile, pv_math_raw)))
}

# ---- Load master_raw and build master_processed ----
load(file.path(master_dir, "master_raw.RData"))
dat <- master_raw

master_processed <- build_master_refactored(dat)
outfile_master <- file.path(master_dir, "master_processed.RData")
save(master_processed, file = outfile_master)
print(paste("Saved master_processed to", outfile_master))
print(paste("Rows:", nrow(master_processed), "Columns:", ncol(master_processed)))


# ---- Longitudinal master (2023 & 2024 only; countries present in both years) ----

long_infile <- file.path(master_dir, "master_longitudinal_raw.RData")

if (file.exists(long_infile)) {
  load(long_infile) # loads object: master_longitudinal_raw
  # Keep longitudinal-only context/weight columns by joining them back after refactoring.
  long_extra_cols <- c(
    "ACDLGMRS", "SCHWGT",
    "ACBG07", "ACBG09",
    "ACBG11AA", "ACBG11BA", "ACBG11BB", "ACBG11BC", "ACBG11BD", "ACBG11BE",
    "IDTEACH", "IDSUBJ", "MATSUBJ", "SCISUBJ",
    "TCHWGT_23", "TCHWGT_24", "MATWGT_23", "MATWGT_24", "SCIWGT_23", "SCIWGT_24",
    "ATBG06E", "ATBLG06E",
    "teacher_parental_involvement_raw", "teacher_parental_involvement_rev"
  )
  long_extra <- master_longitudinal_raw %>%
    dplyr::mutate(IDSTUD = as.character(IDSTUD)) %>%
    dplyr::select(dplyr::any_of(c("IDCNTRY", "IDSCHOOL", "IDSTUD", "year", long_extra_cols)))

  # Longitudinal regression-ready cleaning:
  # convert project sentinel missing codes (998/999) to NA for analysis variables.
  long_regression_na_cols <- c(
    "parent_edu_score", "parent_edu_binary", "parent_occ_score",
    "books_score", "children_books_score",
    "home_books_count", "children_books_count", "resources_computer", "resources_tablet", "resources_internet",
    "PI_binary", "PI_read", "PI_math",
    "home_resources_learning_rev",
    "env_discuss_rev", "env_read_info_rev", "env_save_resources_rev", "env_time_nature_rev", "env_encourage_action_rev",
    "gen_agree_included_rev", "gen_agree_safe_env_rev", "gen_agree_cares_progress_rev", "gen_agree_keeps_informed_rev",
    "gen_agree_promotes_standards_rev", "gen_agree_helps_reading_rev", "gen_agree_helps_math_rev", "gen_agree_helps_science_rev",
    "ACDLGMRS", "ACBG09", "ACBG11AA", "ACBG11BA", "ACBG11BB", "ACBG11BC", "ACBG11BD", "ACBG11BE",
    "ATBG06E", "ATBLG06E",
    "teacher_parental_involvement_raw", "teacher_parental_involvement_rev"
  )

  master_longitudinal_processed <- build_master_refactored(master_longitudinal_raw) %>%
    dplyr::mutate(IDSTUD = as.character(IDSTUD)) %>%
    dplyr::left_join(long_extra, by = c("IDCNTRY", "IDSCHOOL", "IDSTUD", "year")) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(long_regression_na_cols),
        ~ {
          x_num <- suppressWarnings(as.numeric(.x))
          dplyr::if_else(x_num %in% c(998, 999), NA_real_, x_num)
        }
      )
    )
  out_long_master <- file.path(master_dir, "master_longitudinal_processed.RData")
  save(master_longitudinal_processed, file = out_long_master)
  print(paste("Saved master_longitudinal_processed to", out_long_master))
  print(paste("Rows:", nrow(master_longitudinal_processed), "Columns:", ncol(master_longitudinal_processed)))
} else {
  stop("Missing longitudinal raw master: ", long_infile, " (run 1_build_master_table.R first).")
}