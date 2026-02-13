# HW3 Checklist (Based on `HW3 Instructions - Updated 2.0.pdf`)

Use this file as a running task tracker. Check items off as you complete them.

## 1) Setup and Submission Rules
- [ ] Confirm output filenames follow instructions.
- [ ] Include acknowledgments (collaborators + any AI/tool use) at the start of the PDF.
- [ ] Ensure your R script runs start-to-finish as a standalone file.
- [ ] Comment out `setwd()` before final submission.

## 2) Section 3.1 Preliminaries
- [ ] Load required packages.
- [ ] Load `justice_results.tab` as `justice_data` with tab delimiter and ISO-8859-1 encoding.
- [ ] Check dataset structure (`str`).
- [ ] Generate summary statistics.
- [ ] Count unique cases using `docket`.
- [ ] Check missing values across variables.
- [ ] Answer Q1.
- [ ] Answer Q2.
- [ ] Answer Q3.
- [ ] Answer Q4.

## 3) Section 3.2 Descriptive Statistics and Plots
- [ ] Summarize key variables: `petitioner_vote`, `pitch_diff`, `petitioner_harvard_pos`.
- [ ] Create `high_pitch_diff` (1 if `pitch_diff` > mean, else 0).
- [ ] Create `court_period` (Burger/Rehnquist/Roberts).
- [ ] Verify categorical variables with `table()`.
- [ ] Build plot dataset for Chief Justices (Burger, Rehnquist, Roberts) with `justiceName`, `petitioner_vote`, `sgpetac`.
- [ ] Compute vote proportions by justice and `sgpetac`.
- [ ] Create Figure 1 bar plot (blue = No Amicus, red = Amicus, y from 0 to 1, faceted by justice).
- [ ] Save `HW3 Fig1.png` (6x4 inches, 300 DPI).
- [ ] Build second plot dataset with `justiceName`, `petitioner_vote`, `pitch_diff`, `term`.
- [ ] Recreate `high_pitch_diff` labels (Below vs Above Avg. Pitch Differential).
- [ ] Recreate `court_period`.
- [ ] Compute vote proportions by `court_period` and `high_pitch_diff`.
- [ ] Create Figure 2 bar plot (blue = Below Avg., red = Above Avg., y from 0 to 1, faceted by court period).
- [ ] Save `HW3 Fig2.png` (6x4 inches, 300 DPI).
- [ ] Answer Q5.
- [ ] Answer Q6.
- [ ] Answer Q7.
- [ ] Answer Q8.
- [ ] Answer Q9.

## 4) Section 3.3 Regression Analyses
- [ ] Create `pr_petitioner_pos` (petitioner positive proportion minus respondent positive proportion).
- [ ] Estimate `m3_1`: `petitioner_vote ~ pitch_diff + pr_petitioner_pos`.
- [ ] Estimate `m3_2`: add justice fixed effects (`factor(justiceName)`).
- [ ] Estimate `m3_3`: add term indicators.
- [ ] Save formatted regression table for `m3_1` to `m3_3` as `HW3 Table1.txt`.
- [ ] Create/update `court_period`.
- [ ] Run baseline model with `pitch_diff + pr_petitioner_pos`.
- [ ] Run interaction model with `pitch_diff * court_period`.
- [ ] Visualize interaction and save `HW3 Fig3.png`.
- [ ] Build progressive Model 1 (pitch only).
- [ ] Build progressive Model 2 (+ `pr_petitioner_pos`).
- [ ] Build progressive Model 3 (+ `sgpetac`).
- [ ] Build progressive Model 4 (+ `court_period` indicators).
- [ ] Build progressive Model 5 (+ `pitch_diff * court_period`).
- [ ] Build progressive Model 6 (+ `pitch_diff * pr_petitioner_pos`, without Model 5 interaction).
- [ ] Save progressive model table as `HW3 Table2.txt`.
- [ ] Create interaction plot for `pitch_diff * court_period`; save `HW3 Fig4.png`.
- [ ] Create interaction plot for `pitch_diff * pr_petitioner_pos`; save `HW3 Fig5.png`.
- [ ] Answer Q10.
- [ ] Answer Q11.
- [ ] Answer Q12.
- [ ] Answer Q13.
- [ ] Answer Q14.
- [ ] Skip Q15 (explicitly marked IGNORE/DO NOT ANSWER).

## 5) Section 3.4 Outlier Analysis and Validity
- [ ] On final model, compute Studentized Residuals.
- [ ] Compute Leverage.
- [ ] Compute Cook's Distance.
- [ ] Compute DFFITS.
- [ ] Apply outlier thresholds:
- [ ] `|studentized residual| > 2`
- [ ] `leverage > (2k + 2) / n`
- [ ] `Cook's D > 4 / n`
- [ ] `|DFFITS| > 2 * sqrt(k / n)`
- [ ] Flag outliers (any threshold exceeded).
- [ ] Flag egregious outliers (all thresholds exceeded).
- [ ] Plot leverage vs `|DFFITS|` with outliers highlighted and egregious points labeled.
- [ ] Add dashed threshold reference lines.
- [ ] Save `HW3 Fig6.png`.
- [ ] Estimate full-data version of Model 6.
- [ ] Create `clean_data` excluding flagged outliers.
- [ ] Re-estimate Model 6 on `clean_data`.
- [ ] Save comparison table as `HW3 Table3.txt`.
- [ ] Answer Q16.
- [ ] Answer Q17.
- [ ] Bonus: Answer Q18 (optional).
- [ ] Bonus: Answer Q19 (optional).
- [ ] Bonus: Answer Q20 (optional).

## 6) Final Deliverables Checklist
- [ ] PDF response file (per naming rules in instructions).
- [ ] R script file (per naming rules in instructions).
- [ ] `HW3 Fig1.png`
- [ ] `HW3 Fig2.png`
- [ ] `HW3 Fig3.png`
- [ ] `HW3 Fig4.png`
- [ ] `HW3 Fig5.png`
- [ ] `HW3 Fig6.png`
- [ ] `HW3 Table1.txt`
- [ ] `HW3 Table2.txt`
- [ ] `HW3 Table3.txt`
- [ ] All figures/tables referenced and interpreted in the PDF answers.
- [ ] Final script tested one last time from top to bottom.
