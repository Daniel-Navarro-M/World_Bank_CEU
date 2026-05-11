TIMSS Variable Notes (draft)

Context
- These notes summarize variable availability and coding based on TIMSS
  documentation. They inform which variables are used in the master tables and
  which countries/years are excluded due to missing files or missing values.
- Not all countries participate in each year, and not all background files are
  available for all countries. We record and acknowledge this in analysis
  rather than imputing across entire missing countries.
- Current scope: **Grade 4 only**, years **2019 and 2023**, Europe + Balkans.

Key sources reviewed
- TIMSS 2019 G4 codebook: `TIMMS GUIDES/TIMSS2019_IDB_SPSS_G4/.../Codebooks/T19_G4_Codebook.xlsx`
- TIMSS 2023 G4 codebook: `TIMMS GUIDES/TIMSS2023_IDB_R_G4/.../T23_Codebook_G4.xlsx`
- Working variable extraction table: `WB_TIMMS/variable_names.txt`

---

## Summary of Completed Analyses

### Parental Education Measure Comparison

We compared two binary parental education measures as predictors of student achievement:
1. **Parent B / Mother's Education** (`parentB_high_edu` for 2019/2023; `mother_high_edu` for 2015)
2. **Maximum of Both Parents' Education** (`ASDHEDUP_binary`)

Both measures are recoded to binary: 1 = upper secondary or higher (ISCED 4+), 0 = below upper secondary.

#### Correlation Results

| Year | Correlation (r) | N (country-outcome pairs) |
|------|-----------------|---------------------------|
| 2015 | ~0.98           | ~60                       |
| 2019 | ~0.97           | ~80                       |
| 2023 | ~0.98           | ~80                       |

**Key Finding**: The correlations between beta coefficients from the two measures are
extremely high (r > 0.95) across all years and all four achievement domains (Math,
Science, Math Reasoning, Science Reasoning). This indicates that:

1. **The two measures produce nearly identical SES gap estimates** - Using mother's/Parent B
   education alone captures almost all the variation that the "max of two parents" measure captures.
2. **Little additional information from the second parent** - In practice, the educational
   attainment of the primary responding parent (typically the mother) is highly correlated
   with the household's maximum educational level.
3. **Either measure is acceptable for research purposes** - Given the high correlation,
   the choice between measures is unlikely to affect substantive conclusions.

#### Missing Data Analysis: Why "Max of Two" Has Better Coverage

| Year | Variable           | % Valid (approx) | Reason for Missingness |
|------|--------------------|------------------|------------------------|
| 2019 | Parent B Edu       | ~85%             | Parent B not present or didn't respond |
| 2019 | Max of Two (ASDHEDUP) | ~92%          | Derived from either parent's response |
| 2023 | Parent B Edu       | ~84%             | Parent B not present or didn't respond |
| 2023 | Max of Two (ASDHEDUP) | ~91%          | Derived from either parent's response |
| 2015 | Mother Edu (ASBH20B)  | ~80%          | Mother didn't respond to home survey |
| 2015 | Max of Two (ASDHEDUP) | ~88%          | Derived variable, better imputation |

**Explanation**: The `ASDHEDUP` variable (Parents' Highest Education Level) is a TIMSS-derived
variable that takes the maximum education level reported by either parent. This means:
- If only Parent A responds, that value is used
- If only Parent B responds, that value is used
- If both respond, the higher value is used
- The variable is only missing when neither parent provided education information

This design makes `ASDHEDUP` more robust to single-parent households and partial survey
responses, resulting in ~7-10% better coverage than the single-parent variables.

---

## Justification for Excluding 2015 Data

While we conducted a preliminary analysis of 2015 data to compare parental education measures,
we recommend **using only 2019 and 2023 data** for the main analysis. The justifications are:

### 1. Country Participation Differs Substantially

**Source**: Country participation verified against `Timms website` and raw data files.

**European/Balkan countries in each cycle (Grade 4):**

| Country         | ISO3 | 2015 | 2019 | 2023 |
|-----------------|------|------|------|------|
| Albania         | ALB  |  -   |  ✓   |  ✓   |
| Austria         | AUT  |  -   |  ✓   |  -   |
| Bosnia-Herzegovina | BIH | - |  ✓   |  ✓   |
| Bulgaria        | BGR  |  ✓   |  ✓   |  ✓   |
| Croatia         | HRV  |  ✓   |  ✓   |  -   |
| Cyprus          | CYP  |  ✓   |  ✓   |  ✓   |
| Czech Republic  | CZE  |  ✓   |  ✓   |  ✓   |
| Denmark         | DNK  |  ✓   |  ✓   |  ✓   |
| Finland         | FIN  |  ✓   |  ✓   |  ✓   |
| France          | FRA  |  ✓   |  ✓   |  ✓   |
| Germany         | DEU  |  ✓   |  ✓   |  ✓   |
| Hungary         | HUN  |  ✓   |  ✓   |  ✓   |
| Ireland         | IRL  |  ✓   |  ✓   |  ✓   |
| Italy           | ITA  |  ✓   |  ✓   |  ✓   |
| Kosovo          | XKX  |  -   |  ✓   |  ✓   |
| Latvia          | LVA  |  -   |  ✓   |  ✓   |
| Lithuania       | LTU  |  ✓   |  ✓   |  ✓   |
| Malta           | MLT  |  -   |  ✓   |  -   |
| Montenegro      | MNE  |  -   |  ✓   |  ✓   |
| Netherlands     | NLD  |  ✓   |  ✓   |  ✓   |
| North Macedonia | MKD  |  -   |  ✓   |  ✓   |
| Norway          | NOR  |  ✓   |  ✓   |  ✓   |
| Poland          | POL  |  ✓   |  ✓   |  ✓   |
| Portugal        | PRT  |  ✓   |  ✓   |  ✓   |
| Romania         | ROU  |  -   |  -   |  ✓   |
| Serbia          | SRB  |  ✓   |  ✓   |  ✓   |
| Slovakia        | SVK  |  ✓   |  ✓   |  ✓   |
| Slovenia        | SVN  |  ✓   |  -   |  ✓   |
| Spain           | ESP  |  ✓   |  ✓   |  ✓   |
| Sweden          | SWE  |  ✓   |  ✓   |  ✓   |
| UK (England)    | ENG  |  ✓   |  ✓   |  ✓   |
| Belgium (Flemish) | BFL |  ✓   |  ✓   |  ✓   |
| Belgium (French)  | BFR |  ✓   |  ✓   |  ✓   |
| Georgia         | GEO  |  ✓   |  ✓   |  ✓   |

**Belgium (BFL/BFR):** In TIMSS raw data, Belgium is reported as two entities: **Belgium (Flemish)** (IDCNTRY 956, code BFL) and **Belgium (French)** (IDCNTRY 957, code BFR). This is documented in `data/bin/Data/Configuration/StudyConfiguration.xml` and in IDBAnalyzer metadata. The master tables include both as separate “countries” with ISO3 = BFL and BFR and CountryName “Belgium (Flemish)” and “Belgium (French)”. Standard ISO3 BEL (IDCNTRY 56) is kept in the allowed list for compatibility.

**Summary counts (Grade 4, European/Balkan focus):**
- 2015: 21 countries
- 2019: 27 countries (includes new Balkan entrants)
- 2023: 26 countries

**Critical observation**: Key Balkan countries (Albania, Bosnia-Herzegovina, Kosovo, 
Montenegro, North Macedonia) only joined TIMSS Grade 4 in 2019. This makes 2015 data 
**incomplete for Balkan regional analysis**, which is a core focus of this study.

### 2. No Official SES Index in 2015

- **2023**: Provides `ASDHSES` (Home Socioeconomic Status Index) as an official derived variable
  with three categories: Higher, Middle, Lower.
- **2019**: Does not provide `ASDHSES`, but the same component variables are available
  (`ASDHEDUP`, `ASDHOCCP`, `ASBG04`, `ASBG05A-F`), allowing us to replicate the 2023 methodology.
- **2015**: The SES component variables have different names and coding schemes:
  - Parental occupation variable (`ASDHOCCP`) exists but uses different categories
  - Home possessions questions differ in structure
  - No official methodology to create a comparable SES index

This makes SES harmonization across 2015 and 2019/2023 problematic and introduces
measurement error in trend analysis.

### 3. Different Questionnaire Structure

The 2015 Home Questionnaire differs from 2019/2023 in several ways:
- **Parental education questions**: 2015 asks about "Mother" and "Father" specifically,
  while 2019/2023 use "Parent/Guardian A" and "Parent/Guardian B" to accommodate
  diverse family structures.
- **Education coding scale**: 2015 uses a different ISCED mapping (8 categories + NA),
  while 2019/2023 use a 5-category derived variable.
- **Home resources**: 2015 has different items for measuring home educational resources.

### 4. Reasoning PVs Methodology

The Math Reasoning (`ASMREA`) and Science Reasoning (`ASSREA`) plausible values were
introduced with updated item frameworks in 2019. While 2015 has some reasoning items,
the construct definitions and scaling may not be directly comparable.

### Recommendation

**Use 2019 and 2023 for the main analysis** because:
1. Consistent country coverage, especially for Balkan countries
2. Harmonizable SES index construction
3. Comparable questionnaire structure (Parent A/B framework)
4. Methodologically aligned reasoning achievement scales

The 2015 analysis serves as a **sensitivity check** to demonstrate that the high correlation
between mother's education and max-of-two education measures is stable over time, supporting
the robustness of using either measure.

Education variable (ASDHEDUP)
- Label (2019 G4 codebook, sheet `ASHM7`): "Parents' Highest Education Level".
- Coding (2019 G4):
  - 1 University or Higher
  - 2 Post-secondary but not University
  - 3 Upper Secondary
  - 4 Lower Secondary
  - 5 Some Primary, Lower Secondary or No School
  - 6 Not Applicable
  - 9 Omitted or invalid (system missing when not administered)
- Interpretation: lower codes correspond to higher education for 2011/2015/2019.
- For 2023, the parent education scale is different and ordered from low to
  high; we map that to a high-is-high scale in code.
Grade 8 is not used in the current analysis.

Home Socioeconomic Status/IDX (ASDHSES, 2023)
- TIMSS 2023 provides `ASDHSES` with categories: **1 Higher, 2 Middle, 3 Lower** 
  (from `variable_names.txt`: label **"Home Socioeconomic Status/IDX"**, value
  scheme **"1: Higher; 2: Middle; 3: Lower"**).
- This is the official home SES index based on parents' education/occupation
  and home resources. We keep it as provided and also map it to a numeric
  `SES_index` where higher values mean higher SES (3 = Higher, 1 = Lower).

Home SES replication for 2019
- 2019 does not include `ASDHSES`, so we compute an aligned index using the
  **official TIMSS methodology** with four indicators:
  
  **Four SES Components (2019 coding):**
  | Component | Variable | Source | Raw Scale | Score (higher=better) |
  |-----------|----------|--------|-----------|----------------------|
  | Books in home | `ASBH10` | Home Q | 1=0-10, 2=11-25, 3=26-100, 4=101-200, 5=200+ | 1-5 (same as raw) |
  | Children's books | `ASBH11` | Home Q | 1=0-10, 2=11-25, 3=26-50, 4=51-100, 5=100+ | 1-5 (same as raw) |
  | Parent education | `ASDHEDUP` | Derived | 1=Uni+, 2=Post-sec, 3=Upper-sec, 4=Lower-sec, 5=Primary/None | 1-5 (reversed) |
  | Parent occupation | `ASDHOCCP` | Derived | 1=Professional, 2=Small Business, 3=Clerical, 4=Skilled, 5=Laborer, 6=Never worked | 1-4 (reversed) |
  
  **Scale Construction:**
  - Sum of four component scores: range 4 (lowest) to 19 (highest)
  - Missing component values imputed with **school-year mean**, then **country-year mean**
  
  **Cut Score Classification (official TIMSS):**
  | Category | Cut Score | Interpretation |
  |----------|-----------|----------------|
  | Higher | >= 11.1 | >25 books, >25 children's books, uni edu, professional occ (on average) |
  | Middle | 8.7 to <11.1 | Between Higher and Lower |
  | Lower | < 8.7 | <=25 books, <=25 children's books, <=upper-sec edu, no prof/clerical/business |
  
- The final `SES_index` uses: **1 = Lower, 2 = Middle, 3 = Higher** (higher values = higher SES)

Parent education variables (mother/father)
- The mother/father education variables differ by year and grade.
- We keep an explicit mapping in `BUILD_MASTER_TABLES.R` under
  `parent_edu_vars`. Update that list if documentation changes.
- If a primary variable is missing in the raw data for a year/grade, the code
  falls back to the next listed candidate (still explicit and ordered).
- `parentB_high_edu` is a binary variable for Parent/Guardian B education
  (ASBH15B in 2019, ASBH16B in 2023). It is coded 1 for **upper secondary or
  higher**, 0 otherwise.

Reasoning plausible values
- TIMSS 2019 G4 codebook, sheet `ASGM7`, lists:
  - `ASMREA01`-`ASMREA05`: Math Reasoning PVs
  - `ASSREA01`-`ASSREA05`: Science Reasoning PVs
- We keep these variables in the master table for reasoning-focused analysis.

Notes on missingness
- Some countries do not have the ASH files for a given year (e.g., `ASH*` file
  missing). We log missing files and proceed; these countries will have missing
  background components.
- SES components are now imputed when at least one component exists: missing
  values are filled with the **school-year mean**, falling back to the
  **country-year mean**. SES is **only NA when all components are missing**.

Parental investment variables
- `home_study_supports_index`: sum of `*BG05A` (computer/tablet) and `*BG05B`
  (internet). Values range 0–2.
- `books_at_home`: raw categories from `*BG04`.
- These variables are kept in the master table and used as predictors.
  (Note: in the current scope we keep these variables in the master table but
  do **not** run regressions on them yet.)

Outcomes used (achievement PVs)
- Math PVs: `*MMAT01`–`*MMAT05`
- Science PVs: `*SSCI01`–`*SSCI05`
- Math reasoning PVs: `*MREA01`–`*MREA05`
- Science reasoning PVs: `*SREA01`–`*SREA05`

Methodological notes (regressions)
Methodological note: SES harmonization + achievement-gap regressions
- **Scope**: Grade 4 only, years 2019 and 2023, Europe + Balkans.
- **Outcome construction**:
  - TIMSS provides 5 plausible values (PVs) per domain: Math (`ASMMAT01–05`),
    Science (`ASSSCI01–05`), Math reasoning (`ASMREA01–05`), Science reasoning
    (`ASSREA01–05`).
  - For each year and outcome, we create percentile ranks two ways:
    - **Region percentiles**: computed on all Europe+Balkan students together.
    - **Country percentiles**: computed within each country.
  - These percentile columns are generated once in `BUILD_MASTER_TABLES.R` and
    reused in `WB_ANALYSIS.R`.
- **Predictors used to estimate SES/education gaps**:
  - `SES_index`: harmonized so **higher values mean higher SES** across both years
    (2023 uses `ASDHSES` directly; 2019 uses the documented component-based replication).
  - `parentB_high_edu`: binary Parent/Guardian B education (1 = upper secondary+).
  - `ASDHEDUP_binary`: binary parents’ highest education (1 = upper secondary+).
  - `parental_investment_index`: latent parental investment factor as **regional percentiles (1–100)**; see `docs/parental_investment_factor_analysis.md`.
  - `PI_labels`: **tertile** of parental investment (1 = Low, 2 = Medium, 3 = High) from the same latent factor; coefficient = change in achievement percentile per one-category increase.
- **Model**:
  - For each country, year, outcome, and predictor, we run a survey-weighted
    regression of achievement percentiles on the predictor.
  - We use the TIMSS sampling weights (`TOTWGT`) and school/strata identifiers
    (`IDSCHOOL`, `IDSTRATE`) with a JK replicate design (JKn).
  - Regressions are run separately using **region percentiles** and **country
    percentiles**; both estimates are saved side-by-side to assess robustness
    to the percentile definition.
- **Europe vs Balkans contrast**:
  - For each year/outcome/predictor, we also run the regression separately on:
    - students in countries flagged `Is_Balkan == 0` (Europe)
    - students in countries flagged `Is_Balkan == 1` (Balkans)
  - This is done using the **region percentiles** so Europe/Balkan gaps are
    comparable on the same percentile scale.

---

## Scripts and Outputs Reference

### Main Analysis Scripts
| Script | Purpose |
|--------|---------|
| `BUILD_MASTER_TABLES.R` | Builds master Rdata files with cleaned variables and percentiles |
| `WB_ANALYSIS.R` | Runs JK-weighted regressions, generates country-level and EU/Balkan tables |
| `ANALYZE_2015.R` | Processes 2015 data, runs comparison analysis (mother vs max education) |
| `COMPARE_EDUCATION_MEASURES.R` | Creates scatter plots comparing beta coefficients across measures |
| `DATA_QUALITY_PLOTS.R` | Generates data quality and missingness visualizations |

### Output Files
| Folder | Contents |
|--------|----------|
| `output/tables/` | Regression result tables (XLSX, HTML) by year, outcome, and region; education and SES comparison summaries |
| `output/figures/` | Distribution plot of parental investment latent factor; comparison scatter plots (SES vs. parental investment) |
| `data/processed_data/` | Master tables (`master_table_grade_4.RData`, etc.); analysis subsets in `output/master_table_analysis/` |

### Key Output Tables for Report
1. **Country-level regression results**: `output/tables/summary_country_{outcome}_{year}.xlsx` (and `.html`)
   - One row per country; columns: **CountryName**, **ParentB_HighEdu**, **HighestEdu**, **SES**, **Parental_Invest**, **PI_labels**
   - Beta coefficients and JK standard errors (Excel: `_beta` and `_sd` columns)

2. **EU vs Balkan comparison**: `output/tables/summary_eu_balkan_{outcome}_{year}.xlsx` (and `.html`)
   - Two rows (Europe, Balkans); same column structure

3. **Education measures correlation**: `output/tables/education_measures_summary.xlsx`
   - Correlation of education measure betas by year and outcome

4. **SES vs Parental Investment**: `output/tables/ses_parental_investment_summary.xlsx`
   - Correlation of SES vs. Parental Investment gap estimates

---

## Appendix: Full European/Balkan Country List

Countries included in the analysis (ISO3 codes):

**EU/European countries**:
AUT, BEL, BGR, CHE, CZE, DEU, DNK, ESP, EST, FIN, FRA, GBR (ENG), GRC, HRV, HUN,
IRL, ISL, ITA, LIE, LTU, LUX, LVA, NLD, NOR, POL, PRT, SVK, SVN, SWE, UKR

**Balkan countries** (subset of above):
ALB, BIH, BGR, HRV, MNE, MKD, ROU, SRB

Note: Some countries may be missing data for specific years. See participation table above.
