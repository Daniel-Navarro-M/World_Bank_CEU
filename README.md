# WB_TIMMS pipeline

This folder contains five R scripts that build and analyze TIMSS Grade 4 data (Europe and Balkans, 2019 and 2023). The scripts must be run in numerical order: each step depends on the outputs of the previous one.

Paths are relative to the folder containing the scripts. The pipeline expects a `data/raw_data` structure with TIMSS achievement and home-questionnaire files; it creates `data/processed_data` and `output` (tables and figures) as it runs.

---

## 1_build_master_table.R

Loads TIMSS achievement and home-questionnaire data for Grade 4, merges them by country and student ID, and applies minimal recoding (e.g. 998/999 for omitted or not administered). It keeps only the variables needed for later steps (plausible values, weights, replication weights, parent education/occupation, books, parental investment items). Country names are attached via a mapping script. The result is a single "raw" master table with one row per student.

**Output:** `data/processed_data/master/master_table_grade_4.RData` (object `master`).

---

## 2_objective_builder.R

Reads the master table from step 1 and builds all analysis variables: parent education and occupation scores, parent_edu_binary, SES_index and SES_binary, parental investment (PI) items recoded to 0/1/2, PI_index and PI_binary, PV percentiles within region, and similar. It does not overwrite the raw master; it writes a new dataset that includes these derived variables.

**Output:** `data/processed_data/master_refactored.RData` (object `master_refactored`).

---

## 3_eda.R

Uses the master table (with the recoded variables from step 1) to produce exploratory tables and one figure. It computes weighted percentage distributions by country and year for parent education (A, B, and highest), occupation (A, B, and highest), and parental investment items. Each table is saved as HTML and CSV. It also builds a missing-data heatmap.

**Output:** All in `output/`. Tables (HTML and CSV) go to `output/tables/`: eda_parentA_edu, eda_parentB_edu, eda_highest_edu, eda_parentA_occ, eda_parentB_occ, eda_highest_occ, eda_pi_items. The heatmap is saved as `output/figures/eda_missing_heatmap.png`.

---

## 4_regressions.R

Loads `master_refactored` and runs survey-weighted regressions by group (each country, Europe, and Balkans) for 2019 and 2023. It fits Math (PV percentiles) on parent_edu_binary, SES_binary, and PI_binary, and PI_binary on SES_binary. Results are combined with Rubin rules for plausible values and written as HTML and CSV. It also prints a short diagnostic of parent_edu_binary by country (share of 1s and non-missing count). Coefficient tables for 2019 and 2023 are saved for use in step 5.

**Output:** `output/tables/`: regression tables (e.g. reg_math_ses_parentEduBi_2019, reg_math_ses_binary_2019, reg_math_pi_binary_2019, reg_math_parent_edu_binary_2023, and analogues for 2023), plus reg_coefficients_2019.csv and reg_coefficients_2023.csv.

---

## 5_correlations.R

Reads the coefficient CSVs produced by step 4 and computes the correlation between the SES_binary and PI_index (PI_binary) coefficients on math achievement across countries, separately for 2019 and 2023. It saves a small correlation table and scatter plots of the two coefficients by country.

**Output:** `output/tables/`: cor_pi_ses_coefficients.csv and cor_pi_ses_coefficients.html. `output/figures/`: cor_pi_ses_coef_2019.png and cor_pi_ses_coef_2023.png.
