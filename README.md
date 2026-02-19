# HW3 Workflow README

This README documents how `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/HomeWork/HW3/HW3_ScriptTemplate.R` runs, in execution order, with pseudocode and notes on the variable construction, plotting, regression, and outlier-analysis patterns used.

## 1. What the Script Produces

- Cleaned analysis data in memory (`justice_data`, `analysis_data`)
- Missingness report file (`HW3_MissingValues_3_1.txt`)
- Draft answer file for Q1-Q4 (`HW3_Section3_1_Q1_Q4_DraftAnswers.txt`)
- Six required figures (`HW3 Fig1.png` to `HW3 Fig6.png`)
- Three required regression tables (`HW3 Table1.txt`, `HW3 Table2.txt`, `HW3 Table3.txt`)
- Outlier diagnostics summary file (`HW3_OutlierSummary.txt`)
- Draft answer file for Q5-Q17 (`HW3_DraftAnswers_Q5_Q17.txt`)

## 2. Input Files

- `justice_results.tab` (required analysis dataset; tab-delimited, ISO-8859-1 encoding)

Reference documents used for interpretation (not required by the script runtime):
- `HW3 Instructions - Updated 2.0.pdf`
- `Codebook_HW3.docx`
- `dietrich_updated_supplemental_information.pdf`

## 3. High-Level Execution Flow (Pseudocode)

```text
START  // Begin end-to-end HW3 workflow

CHECK working directory and required data file exist  // Fail early if path/file setup is wrong
LOAD libraries: dplyr, tidyr, ggplot2, stargazer  // Attach packages used for wrangling, plots, and tables
DEFINE helper functions:  // Create reusable utilities before analysis steps
  build_court_period(term)  // Map term year to Burger/Rehnquist/Roberts period
  first_mode(x)  // Return most frequent category for reference-value plotting

READ justice_results.tab into justice_data (tab + ISO-8859-1)  // Ingest assignment dataset with required parsing settings
PRINT structure and summary  // Inspect variable types and baseline distributions

COMPUTE missingness table and total missing count  // Quantify NA patterns for diagnostics and write-up
WRITE HW3_MissingValues_3_1.txt  // Persist nonzero-missing summary for submission support

BUILD draft text answers for Q1-Q4 using computed values  // Auto-fill preliminary written responses from live data outputs
WRITE HW3_Section3_1_Q1_Q4_DraftAnswers.txt  // Save draft responses for PDF integration

SECTION 3.2:  // Descriptive statistics and required visualizations
  summarize key variables  // Report core distributional stats for requested variables
  create high_pitch_diff and court_period  // Engineer binary/period features used in plots and models
  verify factor counts with table()  // Confirm category balance and coding correctness
  build chief-justice plot dataset for sgpetac  // Subset to required justices and fields for Figure 1
  compute proportions and save HW3 Fig1.png  // Create amicus-vote proportion chart and export image
  build pitch-differential plot dataset  // Prepare Figure 2 analysis subset
  compute proportions and save HW3 Fig2.png  // Create pitch-group vote chart by court period and export image

SECTION 3.3:  // Regression analyses and interaction effects
  create pr_petitioner_pos with division-by-zero protection  // Compute relative positivity rate safely
  fit m3_1, m3_2, m3_3 and write HW3 Table1.txt  // Estimate baseline + fixed-effects models and export table
  fit period interaction models and save HW3 Fig3.png  // Model pitch-by-period interaction and visualize predicted values
  fit progressive models m_prog1..m_prog6 and write HW3 Table2.txt  // Run nested model sequence and export comparison table
  save interaction plots HW3 Fig4.png and HW3 Fig5.png  // Export two required interaction-effect visuals

SECTION 3.4:  // Outlier diagnostics and robustness check
  run outlier diagnostics on final_model (m_prog6):  // Generate influence metrics from final specification
    studentized residual, leverage, Cook's D, DFFITS  // Compute four assignment-required diagnostic measures
  compute assignment thresholds  // Calculate cutoff rules using n and k formulas
  flag outliers and egregious outliers  // Label rows exceeding any/all diagnostic thresholds
  save diagnostic scatter plot HW3 Fig6.png  // Visualize leverage vs |DFFITS| with threshold lines
  fit full vs outlier-excluded models  // Re-estimate final model before and after dropping flagged rows
  write HW3 Table3.txt and HW3_OutlierSummary.txt  // Export robustness comparison + diagnostics summary

GENERATE Q5-Q17 draft answers from computed stats/models/diagnostics  // Convert numeric outputs into draft narrative responses
WRITE HW3_DraftAnswers_Q5_Q17.txt  // Save draft answers for final PDF editing

END  // Workflow complete
```

## 4. Variable Construction Strategy Used

### Court period mapping

```r
build_court_period <- function(term_vec) {
  dplyr::case_when(
    term_vec >= 1969 & term_vec <= 1985 ~ "Burger",
    term_vec >= 1986 & term_vec <= 2004 ~ "Rehnquist",
    term_vec >= 2005 ~ "Roberts",
    TRUE ~ NA_character_
  )
}
```

- Standardized mapping logic is reused in descriptive and regression sections.
- Court period is converted to an ordered factor (`Burger`, `Rehnquist`, `Roberts`).

### Relative positivity measure

```r
pr_petitioner_pos = dplyr::if_else(
  petitioner_wc > 0 & respondent_wc > 0,
  (petitioner_harvard_pos / petitioner_wc) - (respondent_harvard_pos / respondent_wc),
  NA_real_
)
```

- Computes petitioner-positive-word rate minus respondent-positive-word rate.
- Guards against division by zero by assigning `NA` when either word count is zero.

## 5. Data Cleaning and Missingness Pattern Used

### Missingness audit

```r
missing_by_var <- colSums(is.na(justice_data))
missing_table_nonzero <- missing_table %>%
  dplyr::filter(missing_n > 0) %>%
  dplyr::arrange(desc(missing_n), variable)
```

- Produces a variable-level missingness profile and a total missing-value count.
- Writes only nonzero-missing variables to `HW3_MissingValues_3_1.txt`.

### Complete-case behavior in analysis

- Plot datasets use `tidyr::drop_na()` after selecting needed columns.
- Regressions use `lm(...)`, which applies complete-case behavior by default.
- Outlier filtering is applied only after aligning with `model.frame(final_model)` rows.

## 6. Regression Structure

## 6.1 Core models (Table 1)

- `m3_1`: `petitioner_vote ~ pitch_diff + pr_petitioner_pos`
- `m3_2`: add justice fixed effects (`factor(justiceName)`)
- `m3_3`: add term fixed effects (`factor(term)`)

Output: `HW3 Table1.txt`

## 6.2 Progressive models (Table 2)

- `m_prog1`: pitch only
- `m_prog2`: + `pr_petitioner_pos`
- `m_prog3`: + `sgpetac`
- `m_prog4`: + `court_period`
- `m_prog5`: + `pitch_diff * court_period`
- `m_prog6`: + `pitch_diff * pr_petitioner_pos` (without Model 5 interaction)

Output: `HW3 Table2.txt`

## 6.3 Interaction-plot pattern

```r
grid <- expand.grid(
  pitch_diff = seq(min(...), max(...), length.out = 120),
  court_period = levels(...),
  pr_petitioner_pos = mean(...),
  sgpetac = mean(...)
)
grid$pred <- predict(model, newdata = grid)
```

- Uses deterministic prediction grids and `predict()` for smooth model-based lines.
- Saves `HW3 Fig3.png`, `HW3 Fig4.png`, and `HW3 Fig5.png`.

## 7. Outlier Diagnostics Logic (Section 3.4)

### Diagnostics and thresholds

```r
thr_resid <- 2
thr_lev <- (2 * k_model + 2) / n_model
thr_cook <- 4 / n_model
thr_dffits <- 2 * sqrt(k_model / n_model)
```

- Diagnostics: Studentized residuals, leverage, Cook's D, DFFITS.
- Outlier flag: exceeds any threshold.
- Egregious flag: exceeds all thresholds.

### Full vs clean model comparison

- `analysis_data_used` is built from the exact rows used by `final_model`.
- `clean_data` removes flagged outliers only from those model rows.
- Compares full and clean models in `HW3 Table3.txt`.

## 8. Auto-Drafted Answer Logic (Q5-Q17)

- Q5-Q9 derive from descriptive summaries and figure summary tables.
- Q10-Q14 derive from regression coefficients, p-values, and adjusted R2 values.
- Q16-Q17 derive from outlier diagnostics and full-vs-clean coefficient comparisons.

Support helpers:
- `fmt_num()` for fixed-decimal formatting in narrative text.
- `get_comp()` for standardized coefficient/SE comparison strings.

Output: `HW3_DraftAnswers_Q5_Q17.txt`

## 9. Output Files to Check After Running

- `HW3_MissingValues_3_1.txt`
- `HW3_Section3_1_Q1_Q4_DraftAnswers.txt`
- `HW3 Fig1.png`
- `HW3 Fig2.png`
- `HW3 Fig3.png`
- `HW3 Fig4.png`
- `HW3 Fig5.png`
- `HW3 Fig6.png`
- `HW3 Table1.txt`
- `HW3 Table2.txt`
- `HW3 Table3.txt`
- `HW3_OutlierSummary.txt`
- `HW3_DraftAnswers_Q5_Q17.txt`

## 10. Run Command

```bash
Rscript HW3_ScriptTemplate.R
```
