# HW3 Checklist (Based on `HW3 Instructions - Updated 2.0.pdf`)

Use this file as a running task tracker. Check items off as you complete them.

## 1) Setup and Submission Rules
- [x] Confirm output filenames follow instructions.
- [ ] Include acknowledgments (collaborators + any AI/tool use) at the start of the PDF.
- [x] Ensure your R script runs start-to-finish as a standalone file.
- [x] Comment out `setwd()` before final submission.

## 2) Section 3.1 Preliminaries
- [x] Load required packages.
- [x] Load `justice_results.tab` as `justice_data` with tab delimiter and ISO-8859-1 encoding.
- [x] Check dataset structure (`str`).
- [x] Generate summary statistics.
- [x] Count unique cases using `docket`.
- [x] Check missing values across variables.
- [x] Answer Q1.
- [x] Answer Q2.
- [x] Answer Q3.
- [x] Answer Q4.

## 3) Section 3.2 Descriptive Statistics and Plots
- [x] Summarize key variables: `petitioner_vote`, `pitch_diff`, `petitioner_harvard_pos`.
- [x] Create `high_pitch_diff` (1 if `pitch_diff` > mean, else 0).
- [x] Create `court_period` (Burger/Rehnquist/Roberts).
- [x] Verify categorical variables with `table()`.
- [x] Build plot dataset for Chief Justices (Burger, Rehnquist, Roberts) with `justiceName`, `petitioner_vote`, `sgpetac`.
- [x] Compute vote proportions by justice and `sgpetac`.
- [x] Create Figure 1 bar plot (blue = No Amicus, red = Amicus, y from 0 to 1, faceted by justice).
- [x] Save `HW3 Fig1.png` (6x4 inches, 300 DPI).
- [x] Build second plot dataset with `justiceName`, `petitioner_vote`, `pitch_diff`, `term`.
- [x] Recreate `high_pitch_diff` labels (Below vs Above Avg. Pitch Differential).
- [x] Recreate `court_period`.
- [x] Compute vote proportions by `court_period` and `high_pitch_diff`.
- [x] Create Figure 2 bar plot (blue = Below Avg., red = Above Avg., y from 0 to 1, faceted by court period).
- [x] Save `HW3 Fig2.png` (6x4 inches, 300 DPI).
- [x] Answer Q5.
- [x] Answer Q6.
- [x] Answer Q7.
- [x] Answer Q8.
- [x] Answer Q9.

## 4) Section 3.3 Regression Analyses
- [x] Create `pr_petitioner_pos` (petitioner positive proportion minus respondent positive proportion).
- [x] Estimate `m3_1`: `petitioner_vote ~ pitch_diff + pr_petitioner_pos`.
- [x] Estimate `m3_2`: add justice fixed effects (`factor(justiceName)`).
- [x] Estimate `m3_3`: add term indicators.
- [x] Save formatted regression table for `m3_1` to `m3_3` as `HW3 Table1.txt`.
- [x] Create/update `court_period`.
- [x] Run baseline model with `pitch_diff + pr_petitioner_pos`.
- [x] Run interaction model with `pitch_diff * court_period`.
- [x] Visualize interaction and save `HW3 Fig3.png`.
- [x] Build progressive Model 1 (pitch only).
- [x] Build progressive Model 2 (+ `pr_petitioner_pos`).
- [x] Build progressive Model 3 (+ `sgpetac`).
- [x] Build progressive Model 4 (+ `court_period` indicators).
- [x] Build progressive Model 5 (+ `pitch_diff * court_period`).
- [x] Build progressive Model 6 (+ `pitch_diff * pr_petitioner_pos`, without Model 5 interaction).
- [x] Save progressive model table as `HW3 Table2.txt`.
- [x] Create interaction plot for `pitch_diff * court_period`; save `HW3 Fig4.png`.
- [x] Create interaction plot for `pitch_diff * pr_petitioner_pos`; save `HW3 Fig5.png`.
- [x] Answer Q10.
- [x] Answer Q11.
- [x] Answer Q12.
- [x] Answer Q13.
- [x] Answer Q14.
- [x] Skip Q15 (explicitly marked IGNORE/DO NOT ANSWER).

## 5) Section 3.4 Outlier Analysis and Validity
- [x] On final model, compute Studentized Residuals.
- [x] Compute Leverage.
- [x] Compute Cook's Distance.
- [x] Compute DFFITS.
- [x] Apply outlier thresholds:
- [x] `|studentized residual| > 2`
- [x] `leverage > (2k + 2) / n`
- [x] `Cook's D > 4 / n`
- [x] `|DFFITS| > 2 * sqrt(k / n)`
- [x] Flag outliers (any threshold exceeded).
- [x] Flag egregious outliers (all thresholds exceeded).
- [x] Plot leverage vs `|DFFITS|` with outliers highlighted and egregious points labeled.
- [x] Add dashed threshold reference lines.
- [x] Save `HW3 Fig6.png`.
- [x] Estimate full-data version of Model 6.
- [x] Create `clean_data` excluding flagged outliers.
- [x] Re-estimate Model 6 on `clean_data`.
- [x] Save comparison table as `HW3 Table3.txt`.
- [x] Answer Q16.
- [x] Answer Q17.
- [ ] Bonus: Answer Q18 (optional).
- [ ] Bonus: Answer Q19 (optional).
- [ ] Bonus: Answer Q20 (optional).

## 6) Final Deliverables Checklist
- [ ] PDF response file (per naming rules in instructions).
- [ ] R script file (per naming rules in instructions).
- [x] `HW3 Fig1.png`
- [x] `HW3 Fig2.png`
- [x] `HW3 Fig3.png`
- [x] `HW3 Fig4.png`
- [x] `HW3 Fig5.png`
- [x] `HW3 Fig6.png`
- [x] `HW3 Table1.txt`
- [x] `HW3 Table2.txt`
- [x] `HW3 Table3.txt`
- [ ] All figures/tables referenced and interpreted in the PDF answers.
- [x] Final script tested one last time from top to bottom.
