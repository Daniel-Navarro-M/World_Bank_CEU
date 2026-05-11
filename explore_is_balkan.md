# Where Is_balkan and Balkans/Europe grouping are defined

## 1. Is_balkan flag (master table)

**File:** `1_build_master_table.R`

```r
balkan_countries <- c("Albania", "Bosnia and Herzegovina", "North Macedonia", "Montenegro", "Serbia", "Kosovo")
data$Is_balkan <- data$CountryName %in% balkan_countries
```

- Built **after** `country_mapping` is joined (so `CountryName` comes from `IDBAnalyzerCountries.R`).
- So `Is_balkan` is 1 when `CountryName` exactly matches one of the six names above; otherwise 0 (or NA if `CountryName` is NA).

**Propagation:** `master_raw` is saved with `Is_balkan`. In `2_objective_builder.R`, `id_cols` includes `"Is_balkan"`, so `master_processed` keeps the column.

**Parallel:** `BUILD_MASTER_TABLES.R` sets `Is_Balkan` from ISO3 using `balkan_iso3` (`ALB`, `BIH`, `MKD`, `MNE`, `SRB`, `XKX` for Kosovo). `3_eda.R` uses `balkan_countries_long` with the same country names when `Is_balkan` is missing on longitudinal data.

---

## 2. Balkans group in regressions (current logic)

**File:** `4_regressions.R`

- `design_subset(..., "Balkans")` filters with `design$variables$Is_balkan == 1` (not a separate name list).

---

## 3. Code to explore the flag and names (run manually)

Run after loading `master_processed` (or equivalent):

```r
load("data/processed_data/master_processed.RData")

stopifnot("Is_balkan" %in% names(master_processed))
stopifnot("CountryName" %in% names(master_processed))

library(dplyr)
master_processed %>%
  mutate(CountryName = as.character(CountryName)) %>%
  group_by(year, CountryName, Is_balkan) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(year, CountryName) %>%
  print(n = Inf)

master_processed %>%
  filter(Is_balkan == 1) %>%
  distinct(year, IDCNTRY, CountryName) %>%
  arrange(year, IDCNTRY)

sort(unique(master_processed$CountryName))

balkan_countries <- c("Albania", "Bosnia and Herzegovina", "North Macedonia", "Montenegro", "Serbia", "Kosovo")
data_names <- sort(unique(master_processed$CountryName))
data_names[data_names %in% balkan_countries]
data_names[!data_names %in% balkan_countries]
```

---

## 4. Verification

- **1_build_master_table.R:** `Is_balkan` is created and saved in `master_raw`.
- **2_objective_builder.R:** `id_cols` includes `"Is_balkan"`.
- **4_regressions.R:** Balkans subset uses `Is_balkan == 1`.

Countries with `Is_balkan == 1` should be exactly: Albania, Bosnia and Herzegovina, North Macedonia, Montenegro, Serbia, Kosovo.
