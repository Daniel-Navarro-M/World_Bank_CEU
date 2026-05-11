rm(list = ls())

library(dplyr)
library(readr)
library(tidyr)

master_dir <- "data/processed_data/master"
out_dir <- "output/longitudinal_exploration"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dict_path <- file.path("docs", "dictionary_master_longitudinal.csv")
dict_raw <- readr::read_csv(dict_path, show_col_types = FALSE)
names(dict_raw) <- gsub("[\r\n]+", " ", names(dict_raw))

clean_label <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_+|_+$", "", x)
}

parent_investment_vars <- sprintf("ASBH01%s", LETTERS[1:18])

dict_map <- dict_raw %>%
  transmute(
    label_clean = clean_label(`Label_clean`),
    var_2023 = toupper(trimws(as.character(`Variable Name (2023)`))),
    var_2024 = toupper(trimws(as.character(`Variable Name (2024)`))),
    both_years = trimws(as.character(`Both Years`)),
    question_description = as.character(`Question Description`),
    value_scheme = as.character(`Value Scheme Detailed`)
  ) %>%
  filter(
    both_years == "\u2713",
    nzchar(label_clean),
    nzchar(var_2023),
    nzchar(var_2024),
    !(var_2023 %in% parent_investment_vars),
    !(var_2024 %in% parent_investment_vars)
  ) %>%
  distinct(label_clean, .keep_all = TRUE)

load(file.path(master_dir, "master_longitudinal_processed.RData"))


d <- master_longitudinal_processed %>%
  mutate(
    IDSTUD = as.character(IDSTUD),
    year = as.integer(year)
  ) %>%
  filter(year %in% c(2023L, 2024L))

scale_z <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u2013\u2014\u2212]", "-", x)
  x <- gsub("[\u2018\u2019]", "'", x)
  x <- gsub("\\s+", " ", x)
  trimws(tolower(x))
}

is_binary_scheme <- function(scheme) {
  sc <- normalize_text(scheme)
  grepl("^1:\\s*yes\\s*;\\s*2:\\s*no\\s*$", sc) ||
    grepl("^1:\\s*no\\s*;\\s*2:\\s*yes\\s*$", sc) ||
    grepl("^1:\\s*checked\\s*;\\s*2:\\s*not checked\\s*$", sc)
}

recode_binary_01 <- function(x, scheme) {
  x <- suppressWarnings(as.numeric(x))
  x[x %in% c(998, 999)] <- NA_real_
  sc <- normalize_text(scheme)
  if (grepl("^1:\\s*yes\\s*;\\s*2:\\s*no\\s*$", sc)) return(dplyr::case_when(x == 1 ~ 1, x == 2 ~ 0, TRUE ~ NA_real_))
  if (grepl("^1:\\s*no\\s*;\\s*2:\\s*yes\\s*$", sc)) return(dplyr::case_when(x == 1 ~ 0, x == 2 ~ 1, TRUE ~ NA_real_))
  if (grepl("^1:\\s*checked\\s*;\\s*2:\\s*not checked\\s*$", sc)) return(dplyr::case_when(x == 1 ~ 1, x == 2 ~ 0, TRUE ~ NA_real_))
  x
}

split_scale_labels <- function(scheme) {
  if (is.na(scheme) || !nzchar(trimws(scheme))) return(character())
  scheme <- gsub("[\u2013\u2014\u2212]", "-", scheme)
  scheme <- gsub("[\u2018\u2019]", "'", scheme)
  parts <- strsplit(tolower(scheme), ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- gsub("^[0-9]+\\s*:\\s*", "", parts)
  parts[nzchar(parts)]
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u2013\u2014\u2212]", "-", x)
  x <- gsub("[\u2018\u2019]", "'", x)
  x <- gsub("\\s+", " ", x)
  trimws(tolower(x))
}

classify_orientation <- function(var_name, scheme, question_description) {
  if (is.na(scheme) || !nzchar(trimws(as.character(scheme)))) {
    return(list(reverse = FALSE, rule = "curated_no_scale"))
  }
  txt <- normalize_text(paste(var_name, question_description, scheme))
  scheme_clean <- normalize_text(scheme)

  if (grepl("_rev$", var_name)) return(list(reverse = FALSE, rule = "already_reversed_variable"))

  # Hand-curated scale mappings.
  if (scheme_clean == "1: agree a lot; 2: agree a little; 3: disagree a little; 4: disagree a lot") {
    return(list(reverse = TRUE, rule = "curated_agreement"))
  }
  if (scheme_clean == "1: yes; 2: no") {
    return(list(reverse = TRUE, rule = "curated_yes_no"))
  }
  if (scheme_clean == "1: mostly taught before this year; 2: mostly taught this year; 3: not yet taught") {
    return(list(reverse = TRUE, rule = "curated_taught_progress"))
  }
  if (scheme_clean == "1: every or almost every lesson; 2: about half the lessons; 3: some lessons; 4: never") {
    return(list(reverse = TRUE, rule = "curated_positive_teaching_frequency"))
  }
  if (scheme_clean == "1: very high; 2: high; 3: medium; 4: low; 5: very low") {
    return(list(reverse = TRUE, rule = "curated_high_low"))
  }
  if (scheme_clean == "1: not at all; 2: a little; 3: some; 4: a lot") {
    if (grepl("shortage|inadequacy|affected by", txt)) return(list(reverse = TRUE, rule = "curated_shortage"))
    stop("Unmapped 'not at all -> a lot' context for variable: ", var_name)
  }
  if (scheme_clean == "1: at least once a week; 2: once or twice a month; 3: a few times a year; 4: never or almost never") {
    if (grepl("other students|hurt|threat|steal|damage|force", txt)) return(list(reverse = FALSE, rule = "curated_bullying_freq"))
    return(list(reverse = TRUE, rule = "curated_positive_activity_freq"))
  }
  if (scheme_clean == "1: at least once a week; 2: once or twice a month; 3: a few times a year; 4: never") {
    return(list(reverse = FALSE, rule = "curated_bullying_freq_never"))
  }
  if (scheme_clean == "1: a lot; 2: some; 3: none") {
    return(list(reverse = TRUE, rule = "curated_amount_positive"))
  }
  if (scheme_clean == "") {
    return(list(reverse = FALSE, rule = "curated_no_scale"))
  }
  if (scheme_clean == "1: every day; 2: almost every day; 3: sometimes; 4: never") {
    return(list(reverse = TRUE, rule = "curated_everyday_frequency"))
  }
  if (scheme_clean == "1: not at all; 2: some; 3: a lot") {
    return(list(reverse = TRUE, rule = "curated_limiting_factor"))
  }
  if (scheme_clean == "1: very often; 2: often; 3: sometimes; 4: never or almost never") {
    return(list(reverse = TRUE, rule = "curated_positive_feelings"))
  }
  if (scheme_clean == "1: checked; 2: not checked") {
    return(list(reverse = TRUE, rule = "curated_checked"))
  }
  if (scheme_clean == "1: always or almost always; 2: sometimes; 3: never or almost never") {
    return(list(reverse = TRUE, rule = "curated_homework_followup"))
  }
  if (grepl("^1: has never worked for pay; 2: small business owner;", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_parent_job_status"))
  }
  if (grepl("^1: did not go to school; 2: some <primary education", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_parent_education"))
  }
  if (scheme_clean == "1: 0 to 10%; 2: 11 to 25%; 3: 26 to 50%; 4: more than 50%") {
    if (grepl("disadvantage", txt)) return(list(reverse = TRUE, rule = "curated_disadvantaged_share"))
    return(list(reverse = FALSE, rule = "curated_affluent_share"))
  }
  if (scheme_clean == "1: checked") {
    return(list(reverse = FALSE, rule = "curated_single_checked"))
  }
  if (scheme_clean == "1: once a week; 2: once every two weeks; 3: once a month; 4: once every two months; 5: never or almost never") {
    return(list(reverse = FALSE, rule = "curated_absence_frequency"))
  }
  if (scheme_clean == "1: 6 days; 2: 5 1/2 days; 3: 5 days; 4: 4 1/2 days; 5: 4 days; 6: other") {
    return(list(reverse = TRUE, rule = "curated_instruction_days"))
  }
  if (grepl("^1: did not complete <bachelor", scheme_clean) && grepl("4: <doctor or equivalent level", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_teacher_education_short"))
  }
  if (scheme_clean == "1: always; 2: almost always; 3: sometimes; 4: never") {
    return(list(reverse = TRUE, rule = "curated_language_home_frequency"))
  }
  if (grepl("^1: 0-10; 2: 11-25; 3: 26-50; 4: 51-100; 5: more than 100$", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_children_books"))
  }
  if (grepl("^1: 0-10; 2: 11-25; 3: 26-100; 4: 101-200; 5: more than 200$", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_home_books"))
  }
  if (grepl("^1: under 25; 2: 25-29; 3: 30-39; 4: 40-49; 5: 50-59; 6: 60 or more$", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_age_neutral"))
  }
  if (grepl("^1: none or very few \\(0-10 books\\); 2: enough to fill one shelf", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_teacher_home_books"))
  }
  if (grepl("^1: did not complete <upper secondary education", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_teacher_education_long"))
  }
  if (scheme_clean == "1: female; 2: male; 3: <other>") {
    return(list(reverse = FALSE, rule = "curated_gender_neutral_teacher"))
  }
  if (scheme_clean == "1: more than 90%; 2: 76 to 90%; 3: 51 to 75%; 4: 26 to 50%; 5: 25% or less") {
    return(list(reverse = TRUE, rule = "curated_language_majority"))
  }
  if (grepl("^1: i always speak <language of test> at home; 2: i almost always speak", scheme_clean)) {
    return(list(reverse = TRUE, rule = "curated_speak_test_language_home"))
  }
  if (grepl("^1: finish <lower secondary education", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_child_expected_education"))
  }
  if (scheme_clean == "1: girl; 2: boy; 3: <other>") {
    return(list(reverse = FALSE, rule = "curated_gender_neutral_student"))
  }
  if (grepl("^1: i do not assign mathematics homework;", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_math_homework_frequency"))
  }
  if (grepl("^1: i do not assign science homework;", scheme_clean)) {
    return(list(reverse = FALSE, rule = "curated_science_homework_frequency"))
  }

  stop("Unmapped orientation for variable: ", var_name, " | scheme: ", scheme)
}

orient_high_good <- function(x, var_name, scheme, question_description) {
  x <- suppressWarnings(as.numeric(x))
  x[x %in% c(998, 999)] <- NA_real_
  orient <- classify_orientation(var_name, scheme, question_description)
  if (!isTRUE(orient$reverse)) return(list(x = x, reversed = FALSE, rule = orient$rule))
  vals <- sort(unique(x[is.finite(x)]))
  if (length(vals) < 2 || any(vals %% 1 != 0) || max(vals, na.rm = TRUE) > 20) {
    return(list(x = x, reversed = FALSE, rule = paste0(orient$rule, "_skipped_non_ordinal")))
  }
  list(
    x = (max(vals, na.rm = TRUE) + min(vals, na.rm = TRUE)) - x,
    reversed = TRUE,
    rule = orient$rule
  )
}

vars <- intersect(dict_map$label_clean, names(d))

res_list <- lapply(vars, function(v) {
  scheme <- dict_map$value_scheme[match(v, dict_map$label_clean)]
  qdesc <- dict_map$question_description[match(v, dict_map$label_clean)]
  is_binary <- is_binary_scheme(scheme)
  if (is_binary) {
    x <- recode_binary_01(d[[v]], scheme)
    orient <- list(rule = "curated_binary_direct_01", reversed = FALSE)
  } else {
    orient <- orient_high_good(d[[v]], v, scheme, qdesc)
    x <- orient$x
  }
  pi <- suppressWarnings(as.numeric(d$PI_index))
  pi_z <- scale_z(pi)

  if (is_binary) {
    corr_idx <- is.finite(x) & is.finite(pi_z)
    correlation_pi <- if (sum(corr_idx) > 2) cor(x[corr_idx], pi_z[corr_idx]) else NA_real_
  } else {
    x_z <- scale_z(x)
    corr_idx <- is.finite(x_z) & is.finite(pi_z)
    correlation_pi <- if (sum(corr_idx) > 2) cor(x_z[corr_idx], pi_z[corr_idx]) else NA_real_
  }

  dv <- d %>%
    transmute(IDSTUD, year, value = x) %>%
    filter(year %in% c(2023L, 2024L)) %>%
    distinct(IDSTUD, year, .keep_all = TRUE) %>%
    pivot_wider(names_from = year, values_from = value, names_prefix = "y")

  both_idx <- is.finite(dv$y2023) & is.finite(dv$y2024)
  n_2023 <- sum(is.finite(dv$y2023))
  n_2024 <- sum(is.finite(dv$y2024))
  n_pairs <- sum(both_idx)
  pct_change <- if (sum(both_idx) > 0) mean(dv$y2023[both_idx] != dv$y2024[both_idx]) * 100 else NA_real_
  delta <- if (sum(both_idx) > 0) mean(dv$y2024[both_idx] - dv$y2023[both_idx]) else NA_real_

  data.frame(
    variable_name = v,
    n_2023 = n_2023,
    n_2024 = n_2024,
    n_pairs = n_pairs,
    pct_change = pct_change,
    delta = delta,
    mean = mean(x, na.rm = TRUE),
    std = sd(x, na.rm = TRUE),
    value_scheme_detailed = scheme,
    orientation_rule = orient$rule,
    reversed_applied = orient$reversed,
    is_binary = is_binary,
    correlation_PI = correlation_pi,
    stringsAsFactors = FALSE
  )
})

out <- bind_rows(res_list) %>%
  arrange(desc(abs(correlation_PI)))

outfile <- file.path(out_dir, "longitudinal_exploration_table.csv")
readr::write_csv(out, outfile)
message("Saved: ", outfile)

orientation_audit <- out %>%
  select(variable_name, value_scheme_detailed, orientation_rule, reversed_applied) %>%
  arrange(desc(reversed_applied), variable_name)
readr::write_csv(orientation_audit, file.path(out_dir, "longitudinal_orientation_audit.csv"))
message("Saved: ", file.path(out_dir, "longitudinal_orientation_audit.csv"))
