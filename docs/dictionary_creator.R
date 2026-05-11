# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)   # for coalesce

setwd("D:/CEU/policy lab/WB_TIMMS/docs")

# 1. Load files (preserve exact column names)
var_avail <- read.csv("variable_availability_longitudinal.csv",
                      stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
codebook <- read.csv("longitudinal_codebook.csv",
                     stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
ses <- read.csv("SES_PI_Math_outcome_Questions.csv",
                stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))

# 2. Helper: clean label (remove " (1234)", spaces -> underscores)
clean_label <- function(x) {
  x <- gsub(" \\([0-9]{4}\\)", "", x)   # remove " (2023)", " (1999)"
  x <- gsub(" ", "_", x)                # spaces to underscores
  x <- trimws(x)                        # trim whitespace
  return(x)
}

# 3. Prepare codebook: cleaned Label and unique rows
codebook$Label_clean <- clean_label(codebook$Label)
codebook_unique <- codebook[!duplicated(codebook$Label_clean), ]

# 4. Create a named vector for fast lookup: Variable -> Label
label_lookup <- setNames(codebook$Label, codebook$Variable)

# Function: return Label if variable exists in codebook, else NA
lookup_label <- function(var_name) {
  if (is.na(var_name) || var_name == "") return(NA_character_)
  lbl <- label_lookup[var_name]
  if (is.na(lbl)) return(NA_character_) else return(lbl)
}

# 5. Enrich var_avail: get Label with fallback
col2023 <- "Variable Name\n(2023)"
col2024 <- "Variable Name\n(2024)"

# Ensure empty strings are NA (already done by na.strings, but double-check)
var_avail[[col2023]][var_avail[[col2023]] == ""] <- NA
var_avail[[col2024]][var_avail[[col2024]] == ""] <- NA

# Create the Label column
var_avail$Label <- NA_character_
for (i in 1:nrow(var_avail)) {
  v2023 <- var_avail[i, col2023]
  v2024 <- var_avail[i, col2024]
  
  # Try 2023
  lbl <- lookup_label(v2023)
  if (is.na(lbl) && !is.na(v2024)) {
    # If 2023 fails, try 2024
    lbl <- lookup_label(v2024)
  }
  if (is.na(lbl)) {
    # No match in codebook: use the original variable name (prefer 2023, else 2024)
    lbl <- ifelse(!is.na(v2023), v2023, v2024)
  }
  var_avail$Label[i] <- lbl
}

# Clean the Label column
var_avail$Label_clean <- clean_label(var_avail$Label)

# 6. Add metadata from codebook (using cleaned label)
meta_cols <- c("Value Scheme Detailed", "Missing Scheme Detailed: SPSS",
               "Field Code: SPSS", "Domain", "Level")
for (col in meta_cols) {
  var_avail[[col]] <- NA_character_
  for (i in 1:nrow(var_avail)) {
    lblc <- var_avail$Label_clean[i]
    if (!is.na(lblc) && lblc != "") {
      idx <- match(lblc, codebook_unique$Label_clean)
      if (!is.na(idx)) {
        var_avail[i, col] <- codebook_unique[idx, col]
      }
    }
  }
}

# 7. Process SES: add Helper_clean and Column_category
#    --- EDIT THIS LINE to select the correct category column from codebook ---
col_category_source <- "Domain"   # Change to "Variable Class", "Level", etc. if needed
if (!(col_category_source %in% names(codebook_unique))) {
  stop(paste("Column", col_category_source, "not found in codebook. Available:",
             paste(names(codebook_unique), collapse = ", ")))
}

ses$Helper_clean <- clean_label(ses$Helper)
ses$Column_category <- NA_character_
for (i in 1:nrow(ses)) {
  hlp <- ses$Helper_clean[i]
  if (!is.na(hlp) && hlp != "") {
    idx <- match(hlp, codebook_unique$Label_clean)
    if (!is.na(idx)) {
      ses$Column_category[i] <- codebook_unique[idx, col_category_source]
    }
  }
}

# 8. Join SES into var_avail by matching Label_clean with Helper_clean
#    Bring in Helper (original), Helper_clean, and Column_category
join_cols <- c("Helper_clean", "Helper", "Column_category")
var_avail <- merge(var_avail, ses[, join_cols],
                   by.x = "Label_clean", by.y = "Helper_clean",
                   all.x = TRUE, suffixes = c("", ".from_ses"))

# If Column_category.from_ses was created (in case of name conflict), rename it
if ("Column_category.from_ses" %in% names(var_avail)) {
  var_avail$Column_category <- var_avail$Column_category.from_ses
  var_avail$Column_category.from_ses <- NULL
}

View(var_avail)
# 9. Save final output
write.csv(var_avail, "dictionary_master_longitudinal.csv",
          row.names = FALSE, na = "")
