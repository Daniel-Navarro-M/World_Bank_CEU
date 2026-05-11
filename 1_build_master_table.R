# =============================================================================
# 1. Build Master Table (raw) – load achievement + HOME questionnaire only
# No student questionnaire (ASG). No recoding here; that is in 3_objective_builder.R
# Output: master_raw saved to data/processed_data/master/master_raw.RData
# =============================================================================

rm(list = ls())

library(dplyr)
library(haven)
library(readr)

raw_data_dir       <- "data/raw_data"
processed_data_dir <- "data/processed_data"
master_dir         <- "data/processed_data/master"
dir.create(processed_data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(master_dir,         showWarnings = FALSE, recursive = TRUE)

source(file.path(processed_data_dir, "IDBAnalyzerCountries.R")) # Load from TIMMS the country mapping

cycle_map <- list(`2019` = c("M7"), `2023` = c("M8"), `2024` = c("M8L"))

parent_investment_vars <- sprintf("ASBH01%s", LETTERS[1:18]) # for 2024 we keep results from 2023, this is a fixed effect

dict_path <- file.path("docs", "dictionary_master_longitudinal.csv")
dict_raw <- readr::read_csv(dict_path, show_col_types = FALSE)
names(dict_raw) <- gsub("[\r\n]+", " ", names(dict_raw))

clean_label <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  x
}

dict_map <- dict_raw %>%
  transmute(
    label_clean = clean_label(`Label_clean`),
    var_2023 = toupper(trimws(as.character(`Variable Name (2023)`))),
    var_2024 = toupper(trimws(as.character(`Variable Name (2024)`))),
    questionnaire = trimws(as.character(`Questionnaire Label`)),
    both_years = trimws(as.character(`Both Years`)),
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

dict_home_2023 <- dict_map %>% filter(questionnaire == "Home") %>% pull(var_2023)
dict_home_2024 <- dict_map %>% filter(questionnaire == "Home") %>% pull(var_2024)
dict_student_2023 <- dict_map %>% filter(questionnaire == "Student") %>% pull(var_2023)
dict_student_2024 <- dict_map %>% filter(questionnaire == "Student") %>% pull(var_2024)
dict_school_2023 <- dict_map %>% filter(questionnaire == "School") %>% pull(var_2023)
dict_school_2024 <- dict_map %>% filter(questionnaire == "School") %>% pull(var_2024)
dict_teacher_2023 <- dict_map %>% filter(questionnaire == "Teacher") %>% pull(var_2023)
dict_teacher_2024 <- dict_map %>% filter(questionnaire == "Teacher") %>% pull(var_2024)

europe_iso3 <- c("AUT","BEL","BFL","BFR","BGR","BIH","CHE","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR",
  "GRC","HRV","HUN","IRL","ISL","ITA","LIE","LTU","LUX","LVA","MKD","MLT", "MNE","NLD","NOR","POL","PRT","ROM","SRB","SVK","SVN","SWE","UKR","ALB","XKX")

ach_keep <- c("IDCNTRY","IDSCHOOL","IDSTRATE","IDSTUD","TOTWGT","JKZONE","JKREP",
  "ASMMAT01","ASMMAT02","ASMMAT03","ASMMAT04","ASMMAT05","ASSSCI01","ASSSCI02","ASSSCI03","ASSSCI04","ASSSCI05",
  "ASMREA01","ASMREA02","ASMREA03","ASMREA04","ASMREA05","ASSREA01","ASSREA02","ASSREA03","ASSREA04","ASSREA05")

ach_keep_2024 <- c("IDCNTRY","IDSCHOOL","IDSTRATE","IDSTUD","TOTWGT","JKZONE","JKREP",
  "ASMMAT21","ASMMAT22","ASMMAT23","ASMMAT24","ASMMAT25","ASSSCI21","ASSSCI22","ASSSCI23","ASSSCI24","ASSSCI25",
  "ASMREA21","ASMREA22","ASMREA23","ASMREA24","ASMREA25","ASSREA21","ASSREA22","ASSREA23","ASSREA24","ASSREA25")


# bg_keep: HOME questionnaire only (books, children books, education, occupation, parental investment)

bg_keep <- c("IDCNTRY","IDSTUD","ASDHEDUP","ASDHSES","ASDHOCCP","ASDGHRL","ASBH10","ASBH11",
  "ASBH12A","ASBH12B","ASBH12C",
  "ASBH09A","ASBH09B","ASBH09C","ASBH09D","ASBH09E",
  "ASBH08A","ASBH08B","ASBH08C","ASBH08D","ASBH08E","ASBH08F","ASBH08G","ASBH08H",
  "ASBH15A","ASBH15B","ASBH16A","ASBH16B","ASBH17A","ASBH17B","ASBH18A","ASBH18B",
  parent_investment_vars, dict_home_2023)

bg_keep_2024 <- c("IDCNTRY","IDSTUD","ASDLHEDUP","ASDLHSES","ASDLHOCCP","ASDLGHRL","ASBLH08","ASBLH09", # home questionnaire variables are different in 2024 so we keep the 2023 variables in case we want to display early material access across time 
  "ASBLH10A","ASBLH10B","ASBLH10C",
  "ASBLH07A","ASBLH07B","ASBLH07C","ASBLH07D","ASBLH07E",
  "ASBLH01A","ASBLH01B","ASBLH01C","ASBLH01D","ASBLH01E","ASBLH01F","ASBLH01G","ASBLH01H",
  "ASBLH13A","ASBLH13B","ASBLH15A","ASBLH15B",
  parent_investment_vars, dict_home_2024)

# Some longitudinal home-resource fields are in student questionnaire (ASG), not HOME (ASH).
sg_keep <- c("IDCNTRY", "IDSTUD", "ASDGHRL")
sg_keep_2024 <- c("IDCNTRY", "IDSTUD", "ASDLGHRL", dict_student_2024)
sg_keep <- c(sg_keep, dict_student_2023)


# ASBLH15A	GEN\WHAT KIND OF MAIN JOB\<PARENT/GUARDIAN A> (2024)
# ASBLH15B	GEN\WHAT KIND OF MAIN JOB\<PARENT/GUARDIAN B> (2024)


# ASBLH13A	GEN\LVL OF EDUCATION\<PARENT/GUARDIAN A> (2024)
# ASBLH13B	GEN\LVL OF EDUCATION\<PARENT/GUARDIAN B> (2024)



# Helper: load one file by name as in folder. Try exact name, then lowercase, then uppercase.
load_one <- function(filename) {
  p <- file.path(raw_data_dir, filename)
  if (!file.exists(p)) p <- file.path(raw_data_dir, tolower(filename))
  if (!file.exists(p)) p <- file.path(raw_data_dir, toupper(filename))
  if (!file.exists(p)) stop("Missing input file: ", filename)
  obj_names <- load(p)
  if (length(obj_names) == 0) stop("Loaded file has no objects: ", p)
  df <- get(obj_names[1])
  rm(list = obj_names)
  if (!is.data.frame(df)) stop("Loaded object is not a data frame: ", p)
  names(df) <- toupper(names(df))
  df
}

zap_and_num <- function(df) {
  df[] <- lapply(df, function(col) {
    if (inherits(col, "haven_labelled")) col <- zap_labels(col)
    suppressWarnings(as.numeric(col))
  })
  df
}

extract_raw_metadata <- function(df, year, source_file, questionnaire) {
  vars <- intersect(c(dict_map$var_2023, dict_map$var_2024), names(df))
  if (length(vars) == 0) return(data.frame())
  out <- lapply(vars, function(v) {
    x <- df[[v]]
    lbl <- attr(x, "label")
    val_labels <- attr(x, "labels")
    data.frame(
      year = year,
      source_file = source_file,
      questionnaire = questionnaire,
      variable_raw = v,
      variable_label = if (is.null(lbl)) NA_character_ else as.character(lbl),
      value_labels = if (is.null(val_labels)) NA_character_ else paste(names(val_labels), as.numeric(val_labels), sep = "=", collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  bind_rows(out)
}

# IDSTUD by itself is not globally unique across countries.
# We create a stable unique student key from the moment raw files are loaded:
#   IDSTUD = "<IDCNTRY>_<IDSTUD>"
add_student_uid <- function(df) {
  if (all(c("IDCNTRY", "IDSTUD") %in% names(df))) {
    df$IDSTUD <- paste0(as.integer(df$IDCNTRY), "_", as.integer(df$IDSTUD))
  }
  df
}

# ---- Load and combine: result in "data" ----
# Use exact filenames from the folder; match to europe_iso3 by 3-letter code.
all_data_list <- list()
country_mapping_list <- list()
raw_metadata_list <- list()

for (year in c(2019, 2023, 2024)) {
  cycles <- cycle_map[[as.character(year)]]
  if (is.null(cycles)) stop("Missing cycle map for year: ", year)

  all_files <- list.files(raw_data_dir, pattern = "\\.rdata$", ignore.case = TRUE)
  ach_files <- all_files[grepl(paste0("^asa...", paste(tolower(cycles), collapse = "|"), "\\.rdata$"), tolower(all_files))]

  for (f in ach_files) {
    base_no_ext <- sub("\\.rdata$", "", f, ignore.case = TRUE)
    country_iso <- toupper(substr(base_no_ext, 4, 6))
    if (!country_iso %in% europe_iso3) next

    ach_df <- load_one(f)
    raw_metadata_list[[length(raw_metadata_list) + 1]] <- extract_raw_metadata(ach_df, year, f, "Achievement")
    # Standardize student ID name (raw may be IDSTUD or IDSTUDENT) before select
    stud_col_ach <- names(ach_df)[grepl("^IDSTUD", names(ach_df), ignore.case = TRUE)][1]
    if (!is.na(stud_col_ach) && stud_col_ach != "IDSTUD") ach_df[["IDSTUD"]] <- ach_df[[stud_col_ach]]

    idcntry_val <- as.numeric(ach_df$IDCNTRY[1])
    country_name <- names(country_labels)[match(idcntry_val, country_labels)]
    country_mapping_list[[length(country_mapping_list) + 1]] <- data.frame(
      IDCNTRY = idcntry_val, CountryName = country_name, stringsAsFactors = FALSE
    )

    if (year == 2024) {
      ach_df <- ach_df %>% select(any_of(ach_keep_2024))
      ach_df <- zap_and_num(ach_df)
      ach_df <- add_student_uid(ach_df)
      # Rename 2024 PV columns to 2019/2023 names for consistency (ASMMAT21->ASMMAT01 etc.)
      names(ach_df) <- gsub("ASMMAT2", "ASMMAT0", names(ach_df))
      names(ach_df) <- gsub("ASSSCI2", "ASSSCI0", names(ach_df))
      names(ach_df) <- gsub("ASMREA2", "ASMREA0", names(ach_df))
      names(ach_df) <- gsub("ASSREA2", "ASSREA0", names(ach_df))
    } else {
      ach_df <- ach_df %>% select(any_of(ach_keep))
      ach_df <- zap_and_num(ach_df)
      ach_df <- add_student_uid(ach_df)
    }

    home_file <- sub("^asa", "ash", f, ignore.case = TRUE)
    home_df <- load_one(home_file)
    raw_metadata_list[[length(raw_metadata_list) + 1]] <- extract_raw_metadata(home_df, year, home_file, "Home")
    stud_col_home <- names(home_df)[grepl("^IDSTUD", names(home_df), ignore.case = TRUE)][1]
    if (!is.na(stud_col_home) && stud_col_home != "IDSTUD") home_df[["IDSTUD"]] <- home_df[[stud_col_home]]
    if (year == 2024) {
      home_df <- home_df %>% select(any_of(bg_keep_2024))
    } else {
      home_df <- home_df %>% select(any_of(bg_keep))
    }
    home_df <- zap_and_num(home_df)
    home_df <- add_student_uid(home_df)

    # IDSTUD is globally unique (<IDCNTRY>_<IDSTUD>), so we merge on IDSTUD.
    merge_keys <- "IDSTUD"
    if (!all(merge_keys %in% names(ach_df)) || !all(merge_keys %in% names(home_df))) {
      stop("Missing IDSTUD after harmonisation in file: ", f)
    }
    overlap <- setdiff(intersect(names(ach_df), names(home_df)), merge_keys)
    home_df <- home_df[, !names(home_df) %in% overlap, drop = FALSE]
    merged <- left_join(ach_df, home_df, by = merge_keys)

    # Student questionnaire merge (ASG): required for ASDGHRL/ASDLGHRL in longitudinal analyses.
    studq_file <- sub("^asa", "asg", f, ignore.case = TRUE)
    studq_df <- load_one(studq_file)
    if (!is.null(studq_df)) {
      raw_metadata_list[[length(raw_metadata_list) + 1]] <- extract_raw_metadata(studq_df, year, studq_file, "Student")
      stud_col_stq <- names(studq_df)[grepl("^IDSTUD", names(studq_df), ignore.case = TRUE)][1]
      if (!is.na(stud_col_stq) && stud_col_stq != "IDSTUD") studq_df[["IDSTUD"]] <- studq_df[[stud_col_stq]]
      if (year == 2024) {
        studq_df <- studq_df %>% select(any_of(sg_keep_2024))
      } else {
        studq_df <- studq_df %>% select(any_of(sg_keep))
      }
      studq_df <- zap_and_num(studq_df)
      studq_df <- add_student_uid(studq_df)
      overlap_sg <- setdiff(intersect(names(merged), names(studq_df)), merge_keys)
      studq_df <- studq_df[, !names(studq_df) %in% overlap_sg, drop = FALSE]
      merged <- left_join(merged, studq_df, by = merge_keys)
    }

    # Homogeneous column names as they differ by years
    if (year == 2019) {
      if ("ASBH15A" %in% names(merged)) merged$parentA_edu <- merged$ASBH15A
      if ("ASBH15B" %in% names(merged)) merged$parentB_edu <- merged$ASBH15B
      if ("ASBH17A" %in% names(merged)) merged$parentA_occ <- merged$ASBH17A
      if ("ASBH17B" %in% names(merged)) merged$parentB_occ <- merged$ASBH17B
    } else if (year == 2023) {
      if ("ASBH16A" %in% names(merged)) merged$parentA_edu <- merged$ASBH16A
      if ("ASBH16B" %in% names(merged)) merged$parentB_edu <- merged$ASBH16B
      if ("ASBH18A" %in% names(merged)) merged$parentA_occ <- merged$ASBH18A
      if ("ASBH18B" %in% names(merged)) merged$parentB_occ <- merged$ASBH18B
    } else if (year == 2024) {
      if ("ASBLH13A" %in% names(merged)) merged$parentA_edu <- merged$ASBLH13A
      if ("ASBLH13B" %in% names(merged)) merged$parentB_edu <- merged$ASBLH13B
      if ("ASBLH15A" %in% names(merged)) merged$parentA_occ <- merged$ASBLH15A
      if ("ASBLH15B" %in% names(merged)) merged$parentB_occ <- merged$ASBLH15B
      if ("ASDLHEDUP" %in% names(merged)) merged$ASDHEDUP <- merged$ASDLHEDUP
      if ("ASDLHSES" %in% names(merged)) merged$ASDHSES <- merged$ASDLHSES
      if ("ASDLHOCCP" %in% names(merged)) merged$ASDHOCCP <- merged$ASDLHOCCP
      if ("ASDLGHRL" %in% names(merged)) merged$ASDGHRL <- merged$ASDLGHRL
      if ("ASBLH08" %in% names(merged)) merged$ASBH10 <- merged$ASBLH08
      if ("ASBLH09" %in% names(merged)) merged$ASBH11 <- merged$ASBLH09
      if ("ASBLH10A" %in% names(merged)) merged$ASBH12A <- merged$ASBLH10A
      if ("ASBLH10B" %in% names(merged)) merged$ASBH12B <- merged$ASBLH10B
      if ("ASBLH10C" %in% names(merged)) merged$ASBH12C <- merged$ASBLH10C
      if ("ASBLH07A" %in% names(merged)) merged$ASBH09A <- merged$ASBLH07A
      if ("ASBLH07B" %in% names(merged)) merged$ASBH09B <- merged$ASBLH07B
      if ("ASBLH07C" %in% names(merged)) merged$ASBH09C <- merged$ASBLH07C
      if ("ASBLH07D" %in% names(merged)) merged$ASBH09D <- merged$ASBLH07D
      if ("ASBLH07E" %in% names(merged)) merged$ASBH09E <- merged$ASBLH07E
      if ("ASBLH01A" %in% names(merged)) merged$ASBH08A <- merged$ASBLH01A
      if ("ASBLH01B" %in% names(merged)) merged$ASBH08B <- merged$ASBLH01B
      if ("ASBLH01C" %in% names(merged)) merged$ASBH08C <- merged$ASBLH01C
      if ("ASBLH01D" %in% names(merged)) merged$ASBH08D <- merged$ASBLH01D
      if ("ASBLH01E" %in% names(merged)) merged$ASBH08E <- merged$ASBLH01E
      if ("ASBLH01F" %in% names(merged)) merged$ASBH08F <- merged$ASBLH01F
      if ("ASBLH01G" %in% names(merged)) merged$ASBH08G <- merged$ASBLH01G
      if ("ASBLH01H" %in% names(merged)) merged$ASBH08H <- merged$ASBLH01H

      # Audit check: after 2024 harmonisation, copied variables must match source values exactly.
      harmon_pairs <- list(
        c("ASBLH09", "ASBH11"),
        c("ASBLH08", "ASBH10"),
        c("ASBLH10A", "ASBH12A"),
        c("ASBLH10B", "ASBH12B"),
        c("ASBLH10C", "ASBH12C"),
        c("ASBLH07A", "ASBH09A"),
        c("ASBLH07B", "ASBH09B"),
        c("ASBLH07C", "ASBH09C"),
        c("ASBLH07D", "ASBH09D"),
        c("ASBLH07E", "ASBH09E"),
        c("ASBLH01A", "ASBH08A"),
        c("ASBLH01B", "ASBH08B"),
        c("ASBLH01C", "ASBH08C"),
        c("ASBLH01D", "ASBH08D"),
        c("ASBLH01E", "ASBH08E"),
        c("ASBLH01F", "ASBH08F"),
        c("ASBLH01G", "ASBH08G"),
        c("ASBLH01H", "ASBH08H")
      )
      for (pr in harmon_pairs) {
        src <- pr[1]; dst <- pr[2]
        if (all(c(src, dst) %in% names(merged))) {
          mismatch_n <- sum(!(is.na(merged[[src]]) & is.na(merged[[dst]])) & merged[[src]] != merged[[dst]], na.rm = TRUE)
          if (mismatch_n > 0) stop("Harmonisation mismatch in ", f, ": ", dst, " differs from ", src, " in ", mismatch_n, " rows.")
        }
      }
    }
    # Build dictionary aliases before dropping harmonisation-only source columns.
    for (i in seq_len(nrow(dict_map))) {
      src <- if (year == 2024L) dict_map$var_2024[i] else dict_map$var_2023[i]
      dst <- dict_map$label_clean[i]
      if (src %in% names(merged) && nzchar(dst)) merged[[dst]] <- merged[[src]]
    }

    merged <- merged %>% select(-any_of(c("ASBH15A","ASBH15B","ASBH16A","ASBH16B","ASBH17A","ASBH17B","ASBH18A","ASBH18B","ASBLH13A","ASBLH13B","ASBLH15A","ASBLH15B","ASDLHEDUP","ASDLHSES","ASDLHOCCP","ASDLGHRL","ASBLH08","ASBLH09","ASBLH10A","ASBLH10B","ASBLH10C","ASBLH07A","ASBLH07B","ASBLH07C","ASBLH07D","ASBLH07E","ASBLH01A","ASBLH01B","ASBLH01C","ASBLH01D","ASBLH01E","ASBLH01F","ASBLH01G","ASBLH01H")))

    merged$year <- year
    merged$grade <- 4
    all_data_list[[length(all_data_list) + 1]] <- merged
    message("Loaded ", f, " (", country_name, ")")
  }
  gc()
}

data <- bind_rows(all_data_list)
rm(all_data_list)
gc()

# How country_mapping is built:
# - We only add a row when we successfully load an achievement file (and get past europe_iso3 filter).
# - For each such file we take IDCNTRY from the data (ach_df$IDCNTRY[1]) and look up the name with
#   match(idcntry_val, country_labels): this returns the first position in country_labels where value == IDCNTRY.
#   So CountryName = names(country_labels)[match(...)]. If IDCNTRY is not in country_labels, match returns NA, CountryName is NA.
# - bind_rows(country_mapping_list) stacks all (IDCNTRY, CountryName) rows; distinct(IDCNTRY, .keep_all = TRUE)
#   keeps one row per IDCNTRY (the first seen, so 2019 before 2023 if both cycles loaded).
# Missing countries can happen if: (1) no achievement file was loaded for that country (wrong cycle, file missing, or ISO not in europe_iso3),
# (2) IDCNTRY in the data is not present in country_labels (then CountryName will be NA).

country_mapping <- bind_rows(country_mapping_list) %>% distinct(IDCNTRY, .keep_all = TRUE)
print("\n--- Country mapping (IDCNTRY -> CountryName from IDBAnalyzerCountries.R) ---\n")
print(country_mapping)

data <- left_join(data, country_mapping, by = "IDCNTRY")
balkan_countries <- c("Albania", "Bosnia and Herzegovina", "North Macedonia", "Montenegro", "Serbia", "Kosovo") 
## Chosen as Quoted in Page 2: Van Staden, Et. Al (2025) "Chapter 1: assesing Reading Achievement in the Dinaric Region: An Introduction". 
## Dinaric Perspectives on PIRLS 2021:  IEA Research for Education 17.  https://doi.org/10.1007/978-3-031-88002-5 
 
print(unique(data$CountryName))

data$Is_balkan <- data$CountryName %in% balkan_countries

unique(data$ASDHEDUP)
unique(data$ASDHOCCP)

unique(data$ASDHSES)
df19 <- data %>% filter(year==2019)
length(df19$ASDHSES)
sum(is.na(df19$ASDHSES))
sum(is.na(df19$ASDHSES))/length(df19$ASDHSES)

df23 <- data %>% filter(year==2023)
length(df23$ASDHSES)
sum(is.na(df23$ASDHSES))
sum(is.na(df23$ASDHSES))/length(df23$ASDHSES)


# Recode NA vs Not Administered: 9,99 (omitted/invalid) -> 998; sysmis (not administered) -> 999
# Applied to questionnaire vars so EDA can distinguish
CODE_NA <- 998L
CODE_NOT_ADMIN <- 999L
recode_na_notadmin <- function(x, omit_vals = c(9, 99)) {
  x <- as.numeric(x)
  x[x %in% omit_vals] <- CODE_NA
  x[is.na(x)] <- CODE_NOT_ADMIN
  x}

# NA (998) vs Not Administered (999) - recode at source for consistent EDA

data$ASDHEDUP <- recode_na_notadmin(data$ASDHEDUP, c(6, 9, 99))
data$ASDHOCCP <- recode_na_notadmin(data$ASDHOCCP, c(7, 9, 99))
for (v in c("parentA_edu", "parentB_edu")) if (v %in% names(data)) data[[v]] <- recode_na_notadmin(data[[v]], c(9, 99))
for (v in c("parentA_occ", "parentB_occ")) if (v %in% names(data)) data[[v]] <- recode_na_notadmin(data[[v]], c(12, 99))
for (v in c("ASBH10", "ASBH11", "ASBH12A", "ASBH12B", "ASBH12C")) if (v %in% names(data)) data[[v]] <- recode_na_notadmin(data[[v]], c(9, 99))
for (v in c("ASDGHRL", "ASBH09A", "ASBH09B", "ASBH09C", "ASBH09D", "ASBH09E",
            "ASBH08A", "ASBH08B", "ASBH08C", "ASBH08D", "ASBH08E", "ASBH08F", "ASBH08G", "ASBH08H")) if (v %in% names(data)) data[[v]] <- recode_na_notadmin(data[[v]], c(9, 99))
for (v in parent_investment_vars) if (v %in% names(data)) data[[v]] <- recode_na_notadmin(data[[v]], c(9, 99))

print("Head of data (first 3 rows, selected cols):\n")
print(head(data[c("IDCNTRY", "CountryName", "year", "IDSTUD")], 3))
print("Unique countries in data (IDCNTRY, CountryName):\n")
print(unique(data[c("IDCNTRY", "CountryName")]))

# ---- Keep only countries that appear in BOTH 2019 and 2023 for those years; 2024 keeps all its countries ----
data_all <- data

# The common set is defined ONLY from 2019 and 2023 – 2024 must not affect which 2019/2023 countries we keep.

data_19_23 <- data %>% filter(year %in% c(2019, 2023))
cty_2019   <- unique(data_19_23$IDCNTRY[data_19_23$year == 2019])
cty_2023   <- unique(data_19_23$IDCNTRY[data_19_23$year == 2023])
cty_both   <- intersect(cty_2019, cty_2023)


# Diagnostic: which countries dropped (vs expected 25 and 23) – usually due to raw files (M7/M8/M8L) changing when 2024 was added
cty_2019_only <- setdiff(cty_2019, cty_2023)
cty_2023_only <- setdiff(cty_2023, cty_2019)

nm <- setNames(country_mapping$CountryName, country_mapping$IDCNTRY)

message("\n--- Country counts (before filter): 2019 = ", length(cty_2019), ", 2023 = ", length(cty_2023), ", both = ", length(cty_both))

if (length(cty_2019_only) > 0) message("  In 2019 only (no 2023): ", paste(nm[as.character(cty_2019_only)], collapse = ", "))
if (length(cty_2023_only) > 0) message("  In 2023 only (no 2019): ", paste(nm[as.character(cty_2023_only)], collapse = ", "))

# 2019/2023: keep only cty_both; 2024: keep all (no intersection with 2019/2023)

data <- data %>% filter((year %in% c(2019, 2023) & IDCNTRY %in% cty_both) | (year == 2024))
print("\n--- Countries: 2019/2023 in both; 2024 all ---\n")
print(length(cty_both))
print(sort(unique(data$CountryName[data$year %in% c(2019, 2023)])))
if (any(data$year == 2024)) print(sort(unique(data$CountryName[data$year == 2024])))

# ---- Save raw masters (recoding in 2_objective_builder.R) ----
master_raw <- data
outfile <- file.path(master_dir, "master_raw.RData")
save(master_raw, file = outfile)
print("\n========== Master (raw) saved ==========\n")

message("Saved to ", outfile)

raw_variable_metadata <- bind_rows(raw_metadata_list) %>%
  distinct(year, source_file, questionnaire, variable_raw, .keep_all = TRUE)
metadata_outfile <- file.path(master_dir, "raw_variable_metadata.RData")
save(raw_variable_metadata, file = metadata_outfile)
message("Saved to ", metadata_outfile)

# ---- Save raw longitudinal master: ONLY countries present in BOTH 2023 and 2024 ----
message("Building longitudinal base...")
data_23_24 <- data_all %>% filter(year %in% c(2023, 2024))

cty_2023_l <- unique(data_23_24$IDCNTRY[data_23_24$year == 2023])
cty_2024_l <- unique(data_23_24$IDCNTRY[data_23_24$year == 2024])
cty_both_23_24 <- intersect(cty_2023_l, cty_2024_l)
master_longitudinal_raw <- data_23_24 %>% filter(IDCNTRY %in% cty_both_23_24)
outfile_long <- file.path(master_dir, "master_longitudinal_raw.RData")

# ---- Longitudinal additions: school + teacher context + weights (2023/2024) ----

# Longitudinal participants appear in M8L raw files. Use these ISO codes to limit IO.
m8l_acg <- list.files(raw_data_dir, pattern = "^acg...m8l\\.rdata$", ignore.case = TRUE)
iso_m8l <- unique(toupper(substr(m8l_acg, 4, 6)))

# Helper: load and keep uppercase names (load_one already uppercases).
load_one_upper <- function(filename) {
  load_one(filename)
}

# -------------------------
# 1) School context (ACG)
# Key point: IDSCHOOL is NOT unique across countries, so we always join by IDCNTRY + IDSCHOOL (+ year).
# -------------------------
school_rows <- list()
message("Loading school context...")
if (length(iso_m8l) > 0) {
  for (iso in iso_m8l) {
    # 2023 school context (M8)
    f23 <- paste0("acg", tolower(iso), "m8.rdata")
    s23 <- load_one_upper(f23) %>%
      select(any_of(c(
        "IDCNTRY", "IDSCHOOL",
        "SCHWGT",
        "ACBG07",   # total computers (numeric)
        "ACBG09",   # LMS
        "ACBG11AA", "ACBG11BA", "ACBG11BB", "ACBG11BC", "ACBG11BD", "ACBG11BE",
        dict_school_2023
      ))) %>%
      mutate(year = 2023L)
    school_rows[[length(school_rows) + 1]] <- s23

    # 2024 school context (M8L)
    f24 <- paste0("acg", tolower(iso), "m8l.rdata")
    s24 <- load_one_upper(f24) %>%
      select(any_of(c(
        "IDCNTRY", "IDSCHOOL",
        "SCHWGT",
        "ACDLGMRS",
        "ACBLG05",  # total computers (2024)
        "ACBLG08",  # LMS (2024)
        "ACBLG10AA", "ACBLG10BA", "ACBLG10BB", "ACBLG10BC", "ACBLG10BD", "ACBLG10BE",
        dict_school_2024
      ))) %>%
      rename(
        ACBG07   = ACBLG05,
        ACBG09   = ACBLG08,
        ACBG11AA = ACBLG10AA,
        ACBG11BA = ACBLG10BA,
        ACBG11BB = ACBLG10BB,
        ACBG11BC = ACBLG10BC,
        ACBG11BD = ACBLG10BD,
        ACBG11BE = ACBLG10BE
      ) %>%
      mutate(year = 2024L)
    school_ren <- dict_map %>% filter(questionnaire == "School") %>% select(var_2024, var_2023)
    for (k in seq_len(nrow(school_ren))) {
      src <- school_ren$var_2024[k]
      dst <- school_ren$var_2023[k]
      if (src %in% names(s24) && !(dst %in% names(s24))) names(s24)[names(s24) == src] <- dst
    }
    school_rows[[length(school_rows) + 1]] <- s24
  }
}
school_ctx <- bind_rows(school_rows)
message("Joining school context...")
master_longitudinal_raw <- master_longitudinal_raw %>%
  left_join(school_ctx, by = c("IDCNTRY", "IDSCHOOL", "year"))

# -------------------------
# 2) Teacher linkage (AST): bring teacher weights and IDs to student-level
# Join by IDCNTRY + IDSCHOOL + IDSTUD + year (student-level key, year).
# -------------------------
link_rows <- list()
message("Loading teacher linkage...")
if (length(iso_m8l) > 0) {
  for (iso in iso_m8l) {
    f <- paste0("ast", tolower(iso), "m8l.rdata")
    link_rows[[length(link_rows) + 1]] <- load_one_upper(f) %>%
      select(any_of(c(
        "IDCNTRY", "IDSCHOOL", "IDSTUD", "ITYEAR",
        "IDTEACH", "IDSUBJ", "MATSUBJ", "SCISUBJ",
        "TCHWGT_23", "TCHWGT_24", "MATWGT_23", "MATWGT_24", "SCIWGT_23", "SCIWGT_24"
      )))
  }
}
link_df <- bind_rows(link_rows)

if (nrow(link_df) > 0) {
  message("Deduplicating and joining teacher linkage...")
  # Keep join key types aligned with master_longitudinal_raw:
  # IDSTUD is stored as global character key "<IDCNTRY>_<IDSTUD>".
  link_df <- link_df %>%
    mutate(
      IDCNTRY = suppressWarnings(as.numeric(IDCNTRY)),
      IDSCHOOL = suppressWarnings(as.numeric(IDSCHOOL)),
      IDSTUD = paste0(as.integer(IDCNTRY), "_", as.integer(IDSTUD)),
      year = suppressWarnings(as.integer(ITYEAR))
    )

  # If multiple teacher links per student-year exist, keep one row.
  # Prefer a math-teacher link when available (MATSUBJ == 1).
  link_df <- link_df %>%
    mutate(.is_math = as.integer(!is.na(MATSUBJ) & MATSUBJ == 1)) %>%
    arrange(desc(.is_math)) %>%
    group_by(IDCNTRY, IDSCHOOL, IDSTUD, year) %>%
    summarise(
      IDTEACH = dplyr::first(IDTEACH),
      IDSUBJ  = dplyr::first(IDSUBJ),
      MATSUBJ = dplyr::first(MATSUBJ),
      SCISUBJ = dplyr::first(SCISUBJ),
      TCHWGT_23 = dplyr::first(TCHWGT_23),
      TCHWGT_24 = dplyr::first(TCHWGT_24),
      MATWGT_23 = dplyr::first(MATWGT_23),
      MATWGT_24 = dplyr::first(MATWGT_24),
      SCIWGT_23 = dplyr::first(SCIWGT_23),
      SCIWGT_24 = dplyr::first(SCIWGT_24),
      .groups = "drop"
    )

  master_longitudinal_raw <- master_longitudinal_raw %>%
    left_join(link_df, by = c("IDCNTRY", "IDSCHOOL", "IDSTUD", "year"))
}


# -------------------------
# 3) Teacher context (ATG): parental involvement (reverse-coded)
# Join by IDCNTRY + IDSCHOOL + IDTEACH + year.
# -------------------------
teach_rows <- list()
message("Loading teacher context...")
if (length(iso_m8l) > 0) {
  for (iso in iso_m8l) {
    f <- paste0("atg", tolower(iso), "m8l.rdata")
    teach_rows[[length(teach_rows) + 1]] <- load_one_upper(f) %>%
      select(any_of(c("IDCNTRY", "IDSCHOOL", "IDTEACH", "ITYEAR", "ATBG06E", "ATBLG06E", dict_teacher_2023, dict_teacher_2024)))
  }
}
teach_df <- bind_rows(teach_rows)

if (nrow(teach_df) > 0) {
  message("Deduplicating and joining teacher context...")
  teach_ren <- dict_map %>% filter(questionnaire == "Teacher") %>% select(var_2024, var_2023)
  for (k in seq_len(nrow(teach_ren))) {
    src <- teach_ren$var_2024[k]
    dst <- teach_ren$var_2023[k]
    if (src %in% names(teach_df) && !(dst %in% names(teach_df))) names(teach_df)[names(teach_df) == src] <- dst
  }

  teach_df <- teach_df %>%
    mutate(
      IDCNTRY = suppressWarnings(as.numeric(IDCNTRY)),
      IDSCHOOL = suppressWarnings(as.numeric(IDSCHOOL)),
      year = suppressWarnings(as.integer(ITYEAR))
    )

  # ATG can have multiple records per teacher-year; keep one row per key.
  teach_df <- teach_df %>%
    distinct(IDCNTRY, IDSCHOOL, IDTEACH, year, .keep_all = TRUE)

  master_longitudinal_raw <- master_longitudinal_raw %>%
    left_join(teach_df, by = c("IDCNTRY", "IDSCHOOL", "IDTEACH", "year")) %>%
    mutate(
      .teacher_pi_src = if_else(
        year == 2023L,
        as.numeric(ATBG06E),
        dplyr::coalesce(as.numeric(ATBLG06E), as.numeric(ATBG06E))
      ),
      # Keep only valid response categories (1:5). Omitted/invalid (e.g., 9) and
      # non-administered values are treated as missing at this stage.
      teacher_parental_involvement_raw = case_when(
        .teacher_pi_src %in% 1:5 ~ .teacher_pi_src,
        TRUE ~ NA_real_
      ),
      teacher_parental_involvement_rev = if_else(
        is.na(teacher_parental_involvement_raw),
        NA_real_,
        6 - teacher_parental_involvement_raw
      )
    ) %>%
    select(-.teacher_pi_src)
}

# -------------------------
# 4) Recode missingness (9/99 -> 998; sysmis -> 999) for selected context vars
# -------------------------
ctx_vars <- c(
  "ACDLGMRS", "ACBG09", "ACBG11AA", "ACBG11BA", "ACBG11BB", "ACBG11BC", "ACBG11BD", "ACBG11BE",
  "teacher_parental_involvement_raw", "teacher_parental_involvement_rev"
)
for (v in ctx_vars) if (v %in% names(master_longitudinal_raw))
  master_longitudinal_raw[[v]] <- recode_na_notadmin(master_longitudinal_raw[[v]], c(9, 99))

# -------------------------
# 5) Additional longitudinal home variables (harmonised + reverse coded)
# -------------------------
# Home resources for learning: 1=Many,2=Some,3=Few  -> reverse so 3=Many
if ("ASDGHRL" %in% names(master_longitudinal_raw)) {
  master_longitudinal_raw <- master_longitudinal_raw %>%
    mutate(
      home_resources_learning_rev = case_when(
        ASDGHRL %in% c(1, 2, 3) ~ 4 - ASDGHRL,
        ASDGHRL %in% c(998, 999) ~ ASDGHRL,
        TRUE ~ NA_real_
        )
    )
}

# Environmental activities: 1=Every day ... 4=Never -> reverse so Every day=4
env_vars <- c("ASBH09A", "ASBH09B", "ASBH09C", "ASBH09D", "ASBH09E")
for (v in env_vars) {
  if (!v %in% names(master_longitudinal_raw)) stop("Missing expected env var: ", v)
  v_rev <- paste0(v, "_rev")
  master_longitudinal_raw[[v_rev]] <- case_when(
    master_longitudinal_raw[[v]] %in% c(1, 2, 3, 4) ~ 5 - master_longitudinal_raw[[v]],
    master_longitudinal_raw[[v]] %in% c(998, 999) ~ master_longitudinal_raw[[v]],
    TRUE ~ NA_real_
  )
}

# Parent agrees: 1=Agree a lot ... 4=Disagree a lot -> reverse so higher=more agreement
agree_vars <- c("ASBH08A","ASBH08B","ASBH08C","ASBH08D","ASBH08E","ASBH08F","ASBH08G","ASBH08H")
for (v in agree_vars) {
  if (!v %in% names(master_longitudinal_raw)) stop("Missing expected agree var: ", v)
  v_rev <- paste0(v, "_rev")
  master_longitudinal_raw[[v_rev]] <- case_when(
    master_longitudinal_raw[[v]] %in% c(1, 2, 3, 4) ~ 5 - master_longitudinal_raw[[v]],
    master_longitudinal_raw[[v]] %in% c(998, 999) ~ master_longitudinal_raw[[v]],
    TRUE ~ NA_real_
  )
}

# Home digital resource availability: 1=Yes, 2=No -> binary 1/0
for (v in c("ASBH12A", "ASBH12B", "ASBH12C")) {
  if (!v %in% names(master_longitudinal_raw)) stop("Missing expected digital resource var: ", v)
  master_longitudinal_raw[[v]] <- case_when(
    master_longitudinal_raw[[v]] == 1 ~ 1,
    master_longitudinal_raw[[v]] == 2 ~ 0,
    master_longitudinal_raw[[v]] %in% c(998, 999) ~ master_longitudinal_raw[[v]],
    TRUE ~ NA_real_
  )
}

# Dictionary-driven aliases for variables available in both 2023/2024.
for (i in seq_len(nrow(dict_map))) {
  src23 <- dict_map$var_2023[i]
  src24 <- dict_map$var_2024[i]
  dst <- dict_map$label_clean[i]
  if (nzchar(dst)) {
    current <- if (dst %in% names(master_longitudinal_raw)) master_longitudinal_raw[[dst]] else NA_real_
    v23 <- if (src23 %in% names(master_longitudinal_raw)) master_longitudinal_raw[[src23]] else NA_real_
    v24 <- if (src24 %in% names(master_longitudinal_raw)) master_longitudinal_raw[[src24]] else NA_real_
    master_longitudinal_raw[[dst]] <- dplyr::coalesce(current, v23, v24)
  }
}

# Readable aliases
alias_map <- c(
  # Home resources
  ASBH10 = "home_books_count",
  ASBH11 = "children_books_count",
  ASBH12A = "resources_computer",
  ASBH12B = "resources_tablet",
  ASBH12C = "resources_internet",
  # Environmental engagement items (reversed)
  ASBH09A_rev = "env_discuss_rev",
  ASBH09B_rev = "env_read_info_rev",
  ASBH09C_rev = "env_save_resources_rev",
  ASBH09D_rev = "env_time_nature_rev",
  ASBH09E_rev = "env_encourage_action_rev",
  # Parent agrees items (reversed)
  ASBH08A_rev = "gen_agree_included_rev",
  ASBH08B_rev = "gen_agree_safe_env_rev",
  ASBH08C_rev = "gen_agree_cares_progress_rev",
  ASBH08D_rev = "gen_agree_keeps_informed_rev",
  ASBH08E_rev = "gen_agree_promotes_standards_rev",
  ASBH08F_rev = "gen_agree_helps_reading_rev",
  ASBH08G_rev = "gen_agree_helps_math_rev",
  ASBH08H_rev = "gen_agree_helps_science_rev"
)
for (src in names(alias_map)) {
  dst <- alias_map[[src]]
  if (src %in% names(master_longitudinal_raw)) master_longitudinal_raw[[dst]] <- master_longitudinal_raw[[src]]
}


save(master_longitudinal_raw, file = outfile_long)
message("Saved longitudinal raw to ", outfile_long)
print("\n========== Master longitudinal (raw) saved ==========\n")
message("Saved to ", outfile_long)