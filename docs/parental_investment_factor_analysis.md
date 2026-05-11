# Parental Investment Latent Factor Analysis

## Overview

This document describes the methodological approach for creating a latent factor measure of parental investment using Confirmatory Factor Analysis (CFA) with ordered categorical variables. The analysis is implemented in the TIMSS Grade 4 dataset (2019 and 2023) for European and Balkan countries.

## Research Context

The goal is to measure parental investment as a latent construct using 18 items from the TIMSS Home Questionnaire (ASBH01A through ASBH01R). These items capture various aspects of parental involvement in children's education, such as frequency of activities like reading together, helping with homework, discussing school, etc.

## Replicability and Project Structure

### RProject Setup

The project uses an RProject file (`WB_TIMMS.RProj`) to ensure replicability. This approach is standard in applied research and provides:

- **Automatic working directory management**: No need for `setwd()` calls in scripts
- **Relative paths**: All file paths are relative to the project root
- **Portability**: The project can be moved to different computers while maintaining structure

### Code Organization

The code follows a function-based structure with a main entry point, which is a common pattern in empirical research:

- **Helper functions**: Modular functions for specific tasks (e.g., `get_latent_variable()`, `run_regression_jk()`)
- **Main function**: `run_wb_analysis()` orchestrates the entire workflow
- **Benefits**: 
  - Easy to test individual components
  - Clear separation of concerns
  - Facilitates code reuse
  - Makes the analysis pipeline transparent

This structure aligns with best practices in reproducible research (Gentzkow & Shapiro, 2014; Peng, 2011).

## Methodological Approach

### Step 1: Anchor Variable Selection

**Script**: `PARENTAL_INVESTMENT_ANCHOR.R`

**Objective**: Identify which of the 18 items (ASBH01A-ASBH01R) should serve as the anchor (reference indicator) for the factor model.

**Method**: Stability analysis across countries
- For each item, compute country-level means across all countries and years
- Calculate the standard deviation (SD) of country means
- **Selection criterion**: Choose the item with the **lowest SD** across countries
- **Rationale**: An anchor should have similar means across countries to ensure the latent factor scale is comparable internationally

**Results** (from actual analysis):
- **Suggested anchor**: ASBH01E (lowest SD of country means)
- Total observations: 311,597
- Complete cases: 241,582 (77.5%)
- Partial cases: 70,015 (22.5%)
- Item with most missing data: ASBH01C (57,431 missing, 18.4%)

**Note on Anchor Selection**: While ASBH01E has the lowest SD across countries (making it a stable anchor), its factor loading (0.453) is relatively weak compared to other items. This is acceptable because the anchor's primary role is identification (fixing the scale), not strength of relationship. The stability across countries ensures international comparability of the latent factor scale.

**Output**: 
- Country-by-item means table
- Stability summary with SD and IQR for each item
- Saved to: `output/parental_investment_country_means/parental_investment_country_means.xlsx`

### Step 2: Factor Model Specification

**Model Type**: Confirmatory Factor Analysis (CFA) with ordered categorical indicators

**Identification Strategy**: Marker variable method
- The anchor item (ASBH01E) has its loading fixed to 1.0
- The anchor item's intercept is fixed to 0
- This identifies the latent factor scale and location

**Model Syntax**:
```
I =~ 1*ASBH01E + ASBH01A + ASBH01B + ASBH01C + ... + ASBH01R
ASBH01E ~ 0*1
```

Where:
- `I` = latent parental investment factor
- `1*ASBH01E` = anchor with loading fixed to 1
- `ASBH01E ~ 0*1` = anchor intercept fixed to 0

### Step 3: Estimation

**Estimator**: WLSMV (Weighted Least Squares Mean and Variance adjusted)
- Default for ordered categorical variables in lavaan
- Appropriate for Likert-type scales (1=Never, 2=Sometimes, 3=Often)

**Missing Data Handling**: Pairwise deletion
- Uses all available pairs of variables
- More efficient than listwise deletion
- Retains observations with partial data
- For our data: 77.5% complete cases, 22.5% with partial data

**Implementation** (in `WB_ANALYSIS.R`, function `get_latent_variable()`):
```r
fit <- cfa(PI_model, data = df_year, ordered = items, std.lv = FALSE)
```

### Step 4: Variable Recoding

**Original Coding** (in raw TIMSS data):
- 1 = Often
- 2 = Sometimes  
- 3 = Never or almost never
- 9 = Omitted or invalid
- NA/Sysmis = Not administered

**Actual Values Observed**: The data contains only values 1, 2, 3 (no missing codes observed in the master table after processing), suggesting missing values are already coded as NA.

**Recoded** (in `BUILD_MASTER_TABLES.R`):
- 1 = Never (lowest investment) ← originally 3
- 2 = Sometimes ← unchanged
- 3 = Often (highest investment) ← originally 1

**Rationale**: Ensures that higher values = more parental investment, so positive beta coefficients in regressions mean "higher investment → higher achievement."

### Step 5: Latent Factor Score Prediction

**Method**: Factor scores using `lavPredict()`
- Predicts latent factor scores for each observation
- Handles missing data using the fitted model
- Returns continuous scores on the latent factor scale

**Implementation**:
```r
scores_raw <- lavPredict(fit, type = "lv")[, "I"]
df_year$parental_investment_score <- as.numeric(scores_raw)
```

### Step 6: Percentile Conversion

**Purpose**: Convert continuous latent scores to percentiles for use as an independent variable

**Method**: Weighted regional percentiles
- For each year separately
- Uses `TOTWGT` (student sampling weights)
- Creates 100 percentile bins (1-100)
- Regional = across all European/Balkan countries for that year

**Implementation**:
```r
probs <- seq(0, 1, 0.01)
q <- safe_wtd_quantile(df_year$parental_investment_score, df_year$TOTWGT, probs)
df_year$parental_investment_index <- cut(df_year$parental_investment_score, 
                                         breaks = q, labels = FALSE, include.lowest = TRUE)
```

**Result**: `parental_investment_index` (1-100) used as independent variable in regressions

### Step 7: Tertile Labels (PI_labels)

**Purpose**: Provide a categorical (low / medium / high) version of parental investment for regressions, in addition to the continuous percentile index.

**Method**: Weighted tertiles of the latent factor score
- **Bottom 33%** of the distribution (by weighted quantile) → **PI_labels = 1** (Low)
- **33%–66%** → **PI_labels = 2** (Medium)
- **Top 33%** → **PI_labels = 3** (High)

**Implementation** (in `get_latent_variable()`):
```r
q33_66 <- safe_wtd_quantile(df_year$parental_investment_score, df_year$TOTWGT, c(1/3, 2/3))
df_year$PI_labels <- cut(..., breaks = c(-Inf, q33_66[1], q33_66[2], Inf), labels = 1:3, ...)
```

**Use in regressions**: `PI_labels` is included as a numeric predictor (1, 2, 3). The regression coefficient is the average change in achievement percentile per one-category increase (e.g. Low→Medium or Medium→High). So a positive beta means higher parental investment category is associated with higher achievement.

**Result**: `PI_labels` (1 = Low, 2 = Medium, 3 = High) appears in output tables as column **PI_labels**.

## Factor Loadings Results

From the analysis of 2019 and 2023 data:

### Average Factor Loadings (across years):

| Item | Mean Loading | Interpretation |
|------|--------------|----------------|
| ASBH01K | 0.775 | Strong relationship |
| ASBH01P | 0.760 | Strong relationship |
| ASBH01H | 0.732 | Strong relationship |
| ASBH01D | 0.732 | Strong relationship |
| ASBH01L | 0.672 | Moderate-strong |
| ASBH01G | 0.659 | Moderate-strong |
| ASBH01Q | 0.649 | Moderate |
| ASBH01M | 0.636 | Moderate |
| ASBH01J | 0.616 | Moderate |
| ASBH01I | 0.596 | Moderate |
| ASBH01N | 0.564 | Moderate |
| ASBH01F | 0.556 | Moderate |
| ASBH01B | 0.538 | Moderate |
| ASBH01R | 0.517 | Moderate |
| ASBH01O | 0.490 | Weak-moderate |
| ASBH01A | 0.460 | Weak |
| **ASBH01E** | **0.453** | **Weak (anchor)** |
| ASBH01C | 0.437 | Weak |

### Interpretation:

- **Loadings > 0.7**: Strong relationship with latent factor (4 items: K, P, H, D)
- **Loadings 0.5-0.7**: Moderate relationship (9 items)
- **Loadings < 0.5**: Weak relationship (5 items, including anchor ASBH01E)

**Note**: The anchor (ASBH01E) has a relatively weak loading (0.453), but was chosen for its stability across countries rather than its strength of relationship. This is acceptable for identification purposes.

## Integration into Regression Analysis

### Independent Variables (Predictors)

The regression tables report **five** independent variables for each country (and for Europe vs. Balkans). These are the columns in the HTML and Excel outputs:

| Column in output | Variable name | Description | Interpretation of beta |
|------------------|---------------|-------------|-------------------------|
| **CountryName** | — | Country (or Europe/Balkans) | Row label |
| **ParentB_HighEdu** | `parentB_high_edu` | Binary: 1 = Parent B has upper secondary+ | Change in achievement percentile when Parent B is high vs. low edu |
| **HighestEdu** | `ASDHEDUP_binary` | Binary: 1 = highest parental education is upper secondary+ | Change in achievement percentile when household edu is high vs. low |
| **SES** | `SES_index` | 1 = Lower, 2 = Middle, 3 = Higher (harmonized 2019/2023) | Change in achievement percentile per one-category increase in SES |
| **Parental_Invest** | `parental_investment_index` | Percentile (1–100) of latent parental investment in the region | Change in achievement percentile per 1 percentile point increase in parental investment |
| **PI_labels** | `PI_labels` | Tertile: 1 = Low (bottom 33%), 2 = Medium, 3 = High (top 33%) | Change in achievement percentile per one-category increase (e.g. Low→Medium) |

All coefficients are from **separate** regressions (one predictor at a time per country), so each column is the slope of achievement percentile on that predictor only.

### Regression Specification

For each outcome (math, math reasoning, science, science reasoning):
- **Dependent variable**: Achievement percentile (regional, 1–100), from the five plausible values combined via Rubin's rules
- **Independent variables**: Each of the five above, run one at a time (no multivariate model in the tables)
- **Method**: Survey-weighted Jackknife (JK2) regression using TIMSS replicate weights
- **Plausible values**: One regression per PV; results combined with Rubin's rules (mean coefficient, combined SE)

### Reading the Tables

- **Cell format**: Each cell shows **beta** (with significance stars) and **(standard error)** below. Significance: *** p<0.01, ** p<0.05, * p<0.1.
- **Excel**: Same information in numeric form: columns like `ParentB_HighEdu_beta`, `ParentB_HighEdu_sd`, etc., for use in further analysis or plots.
- **PI_labels**: A positive coefficient means that moving from a lower to a higher tertile of parental investment (e.g. from Low to Medium) is associated with higher achievement on average. The coefficient is in **percentile points** (same scale as the outcome).

### Expected Results

**Positive beta coefficients** for parental investment variables indicate:
- **Parental_Invest**: Higher percentile of parental investment → Higher achievement (continuous effect).
- **PI_labels**: Higher tertile (Low &lt; Medium &lt; High) → Higher achievement (stepwise effect).
- Standard errors account for sampling and PV imputation uncertainty.

## Comparison with SES Index

A separate script (`COMPARE_REGRESSION_RESULTS.R`) compares:
- **SES gaps** (regression coefficients using SES_index)
- **Parental Investment gaps** (regression coefficients using parental_investment_index)

This allows examination of:
- Correlation between SES and parental investment effects
- Whether countries with larger SES gaps also have larger parental investment gaps
- The relative importance of material resources (SES) vs. behavioral investment

Plots are saved to `output/figures/` (e.g. `ses_vs_parental_invest_overall.png`).

## Missing Data

### Patterns Observed

- **Complete cases**: 77.5% of observations have all 18 items
- **Partial cases**: 22.5% have at least one missing item
- **Most problematic item**: ASBH01C (18.4% missing)

### Handling Strategy

**Pairwise deletion** (default for WLSMV):
- Uses all available variable pairs
- Observations with partial data still contribute
- More efficient than listwise deletion
- Standard approach for categorical CFA

**Impact**: The model can still provide factor scores for many observations with partial data, maximizing sample size.

## Output Files

### Regression Tables (HTML and Excel)

- **Country-level**: `output/tables/summary_country_[outcome]_[year].xlsx` and `.html`  
  One row per country; columns: **CountryName**, **ParentB_HighEdu**, **HighestEdu**, **SES**, **Parental_Invest**, **PI_labels**. Each cell shows beta (with significance) and standard error.

- **Europe vs. Balkans**: `output/tables/summary_eu_balkan_[outcome]_[year].xlsx` and `.html`  
  Two rows (Europe, Balkans); same column structure.

- **Excel format**: Numeric columns are stored as `[Predictor]_beta` and `[Predictor]_sd` (e.g. `PI_labels_beta`, `PI_labels_sd`) for use in further analysis or comparison scripts.

### Figures

- **Latent factor distribution**: `output/figures/parental_investment_score_dist_[year].png`  
  Histogram and density of the raw latent factor score (for checking shape and ceiling effects).

- **Comparison plots**: `output/figures/ses_vs_parental_invest_*.png`  
  Scatter plots of SES vs. Parental Investment gaps (from `COMPARE_REGRESSION_RESULTS.R`).

### Anchor Analysis

- `output/parental_investment_country_means/parental_investment_country_means.xlsx`:
  - Missing data summaries
  - Country means by item
  - Stability analysis
  - Factor loadings by year

### Comparison Summaries (Excel)

- `output/tables/education_measures_summary.xlsx`: Correlation of education measure betas.
- `output/tables/ses_parental_investment_summary.xlsx`: Correlation of SES vs. Parental Investment betas.

## References

- **Factor Analysis with Categorical Variables**: See `EduNet_Latent_variables (2).pdf` and `A Note on the Relation Between Factor Analytic and Item Response Theory Models (1).pdf`
- **lavaan Documentation**: Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. *Journal of Statistical Software*, 48(2), 1-36.

## Technical Notes

### Software Requirements
- R with packages: `lavaan`, `dplyr`, `tidyr`, `survey`, `Hmisc`, `kableExtra`, `webshot2`
- RProject structure for path management

### Data Requirements
- Master table with Grade 4 data (2019, 2023)
- All 18 parental investment items (ASBH01A-ASBH01R)
- Student sampling weights (TOTWGT)
- Jackknife replicate weights (JKZONE, JKREP)

### Running the Analysis

1. **Anchor selection**: Run `PARENTAL_INVESTMENT_ANCHOR.R` to identify anchor and examine factor loadings.
2. **Main analysis**: Run `WB_ANALYSIS.R` to compute latent factors (including PI_labels), run regressions, save tables to `output/tables/` and distribution plot to `output/figures/`.
3. **Comparisons**: Run `COMPARE_REGRESSION_RESULTS.R` to generate SES vs. parental investment correlation plots (saved to `output/figures/`).

All scripts assume you're running from the RProject root directory.
