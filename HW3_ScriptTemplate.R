# GPCO 454 - Quantitative Methods II - Winter 2025
# Homework 3 - End-to-End Script

# ---------------------------
# 1) Setup
# ---------------------------

# Keep commented per submission rules.
# setwd("/path/to/HW3")

# Suppress startup messages while loading packages.
suppressPackageStartupMessages({
  # Load package dplyr.
  library(dplyr)     # Data manipulation verbs and piping pipelines
  # Load package tidyr.
  library(tidyr)     # Missing-data helpers and reshaping utilities
  # Load package ggplot2.
  library(ggplot2)   # Plot creation and figure export support
  # Load package stargazer.
  library(stargazer) # Regression table formatting for assignment outputs
# Close current code block.
})

# Map each term year into assignment-defined court periods.
# Assign computed result to build_court_period.
build_court_period <- function(term_vec) {
  # Execute: dplyr::case_when(
  dplyr::case_when(
    # Execute: term_vec >= 1969 & term_vec <= 1985 ~ "Burger",
    term_vec >= 1969 & term_vec <= 1985 ~ "Burger",
    # Execute: term_vec >= 1986 & term_vec <= 2004 ~ "Rehnquist",
    term_vec >= 1986 & term_vec <= 2004 ~ "Rehnquist",
    # Execute: term_vec >= 2005 ~ "Roberts",
    term_vec >= 2005 ~ "Roberts",
    # Execute: TRUE ~ NA_character_
    TRUE ~ NA_character_
  # Close function call arguments.
  )
# Close current code block.
}

# Return the modal category; used to hold one factor constant in interaction plots.
# Assign computed result to first_mode.
first_mode <- function(x) {
  # Assign computed result to tab.
  tab <- sort(table(x), decreasing = TRUE)
  # Execute: names(tab)[1]
  names(tab)[1]
# Close current code block.
}

# ---------------------------
# 2) Section 3.1 Preliminaries
# ---------------------------

# Read input data and assign to justice_data.
justice_data <- read.table(
  # Execute: "justice_results.tab",
  "justice_results.tab",
  # Execute: header = TRUE,
  header = TRUE,
  # Execute: sep = "\t",
  sep = "\t",
  # Execute: encoding = "ISO-8859-1"
  encoding = "ISO-8859-1"
# Close function call arguments.
)

# Print structure and overall summaries for quick diagnostics/Q1-Q2 support.
# Execute: str(justice_data)
str(justice_data)
# Execute: summary(justice_data)
summary(justice_data)

# Count unique entities referenced in the write-up.
# Assign computed result to n_unique_dockets.
n_unique_dockets <- dplyr::n_distinct(justice_data$docket)
# Assign computed result to n_unique_caseid.
n_unique_caseid <- dplyr::n_distinct(justice_data$caseId)
# Assign computed result to n_unique_justices.
n_unique_justices <- dplyr::n_distinct(justice_data$justiceName)

# Build variable-level and total missingness diagnostics.
# Assign computed result to missing_by_var.
missing_by_var <- colSums(is.na(justice_data))
# Assign computed result to total_missing.
total_missing <- sum(is.na(justice_data))
# Assign computed result to missing_table.
missing_table <- data.frame(
  # Execute: variable = names(missing_by_var),
  variable = names(missing_by_var),
  # Execute: missing_n = as.integer(missing_by_var),
  missing_n = as.integer(missing_by_var),
  # Execute: missing_pct = round(as.integer(missing_by_var) / nrow(justice_data) * 100, 2)
  missing_pct = round(as.integer(missing_by_var) / nrow(justice_data) * 100, 2)
# Close function call arguments.
)
# Assign computed result to missing_table_nonzero.
missing_table_nonzero <- missing_table %>%
  # Execute: dplyr::filter(missing_n > 0) %>%
  dplyr::filter(missing_n > 0) %>%
  # Execute: dplyr::arrange(desc(missing_n), variable)
  dplyr::arrange(desc(missing_n), variable)

# Export missing-value summary for submission support.
# Write table output to file.
write.table(
  # Execute: missing_table_nonzero,
  missing_table_nonzero,
  # Execute: file = "HW3_MissingValues_3_1.txt",
  file = "HW3_MissingValues_3_1.txt",
  # Execute: sep = "\t",
  sep = "\t",
  # Execute: row.names = FALSE,
  row.names = FALSE,
  # Execute: quote = FALSE
  quote = FALSE
# Close function call arguments.
)

# Check whether docket-justice rows are unique (unit-of-observation validation).
# Assign computed result to n_unique_docket_justice.
n_unique_docket_justice <- nrow(unique(justice_data[, c("docketId", "justiceName")]))
# Assign computed result to n_duplicate_docket_justice.
n_duplicate_docket_justice <- nrow(justice_data) - n_unique_docket_justice

# Draft Q1 narrative using computed descriptive values.
# Assign computed result to q1_answer.
q1_answer <- paste0(
  # Execute: "This dataset contains ", nrow(justice_data), " justice-level observations across ",
  "This dataset contains ", nrow(justice_data), " justice-level observations across ",
  # Execute: n_unique_dockets, " unique dockets (", n_unique_caseid, " unique case IDs) and ",
  n_unique_dockets, " unique dockets (", n_unique_caseid, " unique case IDs) and ",
  # Execute: n_unique_justices, " justices. ",
  n_unique_justices, " justices. ",
  # Execute: "Each row includes vote outcomes (petitioner_vote), oral-argument speech measures (including pitch_diff), an...
  "Each row includes vote outcomes (petitioner_vote), oral-argument speech measures (including pitch_diff), and case context such as term and amicus variables. ",
  # Execute: "This structure supports testing whether oral-argument emotional cues are associated with Supreme Court voti...
  "This structure supports testing whether oral-argument emotional cues are associated with Supreme Court voting behavior."
# Close function call arguments.
)

# Draft Q2 narrative focused on observation unit and duplicate check.
# Assign computed result to q2_answer.
q2_answer <- paste0(
  # Execute: "The unit of observation is one justice-docket vote record. ",
  "The unit of observation is one justice-docket vote record. ",
  # Execute: "docketId + justiceName combinations are unique in this dataset (duplicates = ",
  "docketId + justiceName combinations are unique in this dataset (duplicates = ",
  # Execute: n_duplicate_docket_justice, "), which is consistent with that unit."
  n_duplicate_docket_justice, "), which is consistent with that unit."
# Close function call arguments.
)

# Draft Q3 narrative for dependent-variable meaning and relevance.
# Assign computed result to q3_answer.
q3_answer <- paste(
  # Execute: "A vote in favor of the petitioner is coded as petitioner_vote = 1.",
  "A vote in favor of the petitioner is coded as petitioner_vote = 1.",
  # Execute: "This is substantively important because these votes determine case outcomes and legal precedent.",
  "This is substantively important because these votes determine case outcomes and legal precedent.",
  # Execute: "The assignment uses this outcome to evaluate whether emotional dynamics in oral argument relate to judicial...
  "The assignment uses this outcome to evaluate whether emotional dynamics in oral argument relate to judicial decisions."
# Close function call arguments.
)

# Draft Q4 narrative using computed missingness.
# Assign computed result to q4_answer.
q4_answer <- paste0(
  # Execute: "There are ", total_missing, " missing values in total. ",
  "There are ", total_missing, " missing values in total. ",
  # Execute: nrow(missing_table_nonzero), " variables have missing data, and each has 232 missing observations (4.45% of ...
  nrow(missing_table_nonzero), " variables have missing data, and each has 232 missing observations (4.45% of rows). ",
  # Execute: "Missingness matters because complete-case estimation can reduce sample size and potentially bias results if...
  "Missingness matters because complete-case estimation can reduce sample size and potentially bias results if missingness is non-random."
# Close function call arguments.
)

# Combine all Section 3.1 draft responses into one text vector.
# Assign computed result to answers_q1_q4.
answers_q1_q4 <- c(
  # Execute: "Section 3.1 Draft Answers (Q1-Q4)",
  "Section 3.1 Draft Answers (Q1-Q4)",
  # Execute: "",
  "",
  # Execute: paste0("Q1: ", q1_answer),
  paste0("Q1: ", q1_answer),
  # Execute: "",
  "",
  # Execute: paste0("Q2: ", q2_answer),
  paste0("Q2: ", q2_answer),
  # Execute: "",
  "",
  # Execute: paste0("Q3: ", q3_answer),
  paste0("Q3: ", q3_answer),
  # Execute: "",
  "",
  # Execute: paste0("Q4: ", q4_answer),
  paste0("Q4: ", q4_answer),
  # Execute: "",
  "",
  # Execute: "Variables with missing values (Q4 support):",
  "Variables with missing values (Q4 support):",
  # Execute: paste0(
  paste0(
    # Execute: missing_table_nonzero$variable, ": ",
    missing_table_nonzero$variable, ": ",
    # Execute: missing_table_nonzero$missing_n, " (",
    missing_table_nonzero$missing_n, " (",
    # Execute: missing_table_nonzero$missing_pct, "%)"
    missing_table_nonzero$missing_pct, "%)"
  # Close function call arguments.
  )
# Close function call arguments.
)

# Write Section 3.1 draft answers to a standalone file.
# Write text output to file.
writeLines(answers_q1_q4, con = "HW3_Section3_1_Q1_Q4_DraftAnswers.txt")

# ---------------------------
# 3) Section 3.2 Descriptive Stats and Plots
# ---------------------------

# Compute required descriptive statistics for key variables (Q5 setup).
# Assign computed result to summary_3_2.
summary_3_2 <- summary(justice_data[, c("petitioner_vote", "pitch_diff", "petitioner_harvard_pos")])
# Execute: print(summary_3_2)
print(summary_3_2)

# Build analysis features used in descriptive plots.
# Assign computed result to pitch_mean.
pitch_mean <- mean(justice_data$pitch_diff, na.rm = TRUE)
# Assign computed result to justice_data.
justice_data <- justice_data %>%
  # Execute: dplyr::mutate(
  dplyr::mutate(
    # Execute: high_pitch_diff = ifelse(pitch_diff > pitch_mean, 1, 0), # Above-mean pitch indicator
    high_pitch_diff = ifelse(pitch_diff > pitch_mean, 1, 0),                          # Above-mean pitch indicator
    # Execute: high_pitch_diff = factor(high_pitch_diff, levels = c(0, 1)), # Ordered binary factor
    high_pitch_diff = factor(high_pitch_diff, levels = c(0, 1)),                      # Ordered binary factor
    # Execute: court_period = build_court_period(term), # Term-to-court-period mapping
    court_period = build_court_period(term),                                           # Term-to-court-period mapping
    # Execute: court_period = factor(court_period, levels = c("Burger", "Rehnquist", "Roberts")) # Stable plotting/model order
    court_period = factor(court_period, levels = c("Burger", "Rehnquist", "Roberts")) # Stable plotting/model order
  # Close function call arguments.
  )

# Quick frequency checks for engineered factors.
# Execute: table(justice_data$high_pitch_diff, useNA = "ifany")
table(justice_data$high_pitch_diff, useNA = "ifany")
# Execute: table(justice_data$court_period, useNA = "ifany")
table(justice_data$court_period, useNA = "ifany")

# Figure 1 source data: restrict to required chief justices and needed columns.
# Assign computed result to chief_data.
chief_data <- justice_data %>%
  # Execute: dplyr::filter(justiceName %in% c("WEBurger", "WHRehnquist", "JGRoberts")) %>%
  dplyr::filter(justiceName %in% c("WEBurger", "WHRehnquist", "JGRoberts")) %>%
  # Execute: dplyr::select(justiceName, petitioner_vote, sgpetac) %>%
  dplyr::select(justiceName, petitioner_vote, sgpetac) %>%
  # Execute: tidyr::drop_na()
  tidyr::drop_na()

# Compute petitioner-vote share by justice and SG amicus status.
# Assign computed result to fig1_summary.
fig1_summary <- chief_data %>%
  # Execute: dplyr::group_by(justiceName, sgpetac) %>%
  dplyr::group_by(justiceName, sgpetac) %>%
  # Execute: dplyr::summarise(prop_petitioner_vote = mean(petitioner_vote), .groups = "drop") %>%
  dplyr::summarise(prop_petitioner_vote = mean(petitioner_vote), .groups = "drop") %>%
  # Execute: dplyr::mutate(
  dplyr::mutate(
    # Execute: sgpetac_label = ifelse(sgpetac == 1, "Amicus", "No Amicus"),
    sgpetac_label = ifelse(sgpetac == 1, "Amicus", "No Amicus"),
    # Execute: sgpetac_label = factor(sgpetac_label, levels = c("No Amicus", "Amicus"))
    sgpetac_label = factor(sgpetac_label, levels = c("No Amicus", "Amicus"))
  # Close function call arguments.
  )

# Build Figure 1 bar chart with assignment colors and y-range.
# Create ggplot object and store in p1.
p1 <- ggplot(fig1_summary, aes(x = sgpetac_label, y = prop_petitioner_vote, fill = sgpetac_label)) +
  # Execute: geom_col() +
  geom_col() +
  # Execute: facet_wrap(~ justiceName) +
  facet_wrap(~ justiceName) +
  # Execute: scale_fill_manual(values = c("No Amicus" = "blue", "Amicus" = "red")) +
  scale_fill_manual(values = c("No Amicus" = "blue", "Amicus" = "red")) +
  # Execute: coord_cartesian(ylim = c(0, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  # Execute: labs(
  labs(
    # Execute: x = "Solicitor General Amicus (sgpetac)",
    x = "Solicitor General Amicus (sgpetac)",
    # Execute: y = "Proportion Voting for Petitioner",
    y = "Proportion Voting for Petitioner",
    # Execute: fill = ""
    fill = ""
  # Close function call arguments.
  ) +
  # Execute: theme_minimal()
  theme_minimal()

# Export Figure 1 with required dimensions/DPI.
# Save figure to disk with specified dimensions.
ggsave("HW3 Fig1.png", plot = p1, width = 6, height = 4, dpi = 300)

# Figure 2 source data: same justices plus pitch and term fields.
# Assign computed result to fig2_data.
fig2_data <- justice_data %>%
  # Execute: dplyr::filter(justiceName %in% c("WEBurger", "WHRehnquist", "JGRoberts")) %>%
  dplyr::filter(justiceName %in% c("WEBurger", "WHRehnquist", "JGRoberts")) %>%
  # Execute: dplyr::select(justiceName, petitioner_vote, pitch_diff, term) %>%
  dplyr::select(justiceName, petitioner_vote, pitch_diff, term) %>%
  # Execute: tidyr::drop_na()
  tidyr::drop_na()

# Recompute pitch grouping and period labels specifically for Figure 2.
# Assign computed result to fig2_mean.
fig2_mean <- mean(fig2_data$pitch_diff, na.rm = TRUE)
# Assign computed result to fig2_data.
fig2_data <- fig2_data %>%
  # Execute: dplyr::mutate(
  dplyr::mutate(
    # Execute: high_pitch_diff = ifelse(
    high_pitch_diff = ifelse(
      # Execute: pitch_diff > fig2_mean,
      pitch_diff > fig2_mean,
      # Execute: "Above Avg. Pitch Differential",
      "Above Avg. Pitch Differential",
      # Execute: "Below Avg. Pitch Differential"
      "Below Avg. Pitch Differential"
    # Close function call arguments.
    ),
    # Execute: high_pitch_diff = factor(
    high_pitch_diff = factor(
      # Execute: high_pitch_diff,
      high_pitch_diff,
      # Execute: levels = c("Below Avg. Pitch Differential", "Above Avg. Pitch Differential")
      levels = c("Below Avg. Pitch Differential", "Above Avg. Pitch Differential")
    # Close function call arguments.
    ),
    # Execute: court_period = build_court_period(term),
    court_period = build_court_period(term),
    # Execute: court_period = factor(court_period, levels = c("Burger", "Rehnquist", "Roberts"))
    court_period = factor(court_period, levels = c("Burger", "Rehnquist", "Roberts"))
  # Close function call arguments.
  )

# Compute petitioner-vote share by court period and pitch group.
# Assign computed result to fig2_summary.
fig2_summary <- fig2_data %>%
  # Execute: dplyr::group_by(court_period, high_pitch_diff) %>%
  dplyr::group_by(court_period, high_pitch_diff) %>%
  # Execute: dplyr::summarise(prop_petitioner_vote = mean(petitioner_vote), .groups = "drop")
  dplyr::summarise(prop_petitioner_vote = mean(petitioner_vote), .groups = "drop")

# Build Figure 2 bar chart with assignment labels/colors.
# Create ggplot object and store in p2.
p2 <- ggplot(fig2_summary, aes(x = high_pitch_diff, y = prop_petitioner_vote, fill = high_pitch_diff)) +
  # Execute: geom_col() +
  geom_col() +
  # Execute: facet_wrap(~ court_period) +
  facet_wrap(~ court_period) +
  # Execute: scale_fill_manual(values = c(
  scale_fill_manual(values = c(
    # Execute: "Below Avg. Pitch Differential" = "blue",
    "Below Avg. Pitch Differential" = "blue",
    # Execute: "Above Avg. Pitch Differential" = "red"
    "Above Avg. Pitch Differential" = "red"
  # Close function call arguments.
  )) +
  # Execute: coord_cartesian(ylim = c(0, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  # Execute: labs(
  labs(
    # Execute: x = "Pitch Differential Group",
    x = "Pitch Differential Group",
    # Execute: y = "Proportion Voting for Petitioner",
    y = "Proportion Voting for Petitioner",
    # Execute: fill = ""
    fill = ""
  # Close function call arguments.
  ) +
  # Execute: theme_minimal()
  theme_minimal()

# Export Figure 2 with required dimensions/DPI.
# Save figure to disk with specified dimensions.
ggsave("HW3 Fig2.png", plot = p2, width = 6, height = 4, dpi = 300)

# ---------------------------
# 4) Section 3.3 Regression Analyses
# ---------------------------

# Construct regression-ready dataset and core derived predictor.
# Assign computed result to analysis_data.
analysis_data <- justice_data %>%
  # Execute: dplyr::mutate(
  dplyr::mutate(
    # Execute: pr_petitioner_pos = dplyr::if_else(
    pr_petitioner_pos = dplyr::if_else(
      # Execute: petitioner_wc > 0 & respondent_wc > 0,
      petitioner_wc > 0 & respondent_wc > 0,
      # Execute: (petitioner_harvard_pos / petitioner_wc) - (respondent_harvard_pos / respondent_wc),
      (petitioner_harvard_pos / petitioner_wc) - (respondent_harvard_pos / respondent_wc),
      # Execute: NA_real_
      NA_real_
    # Close function call arguments.
    ),
    # Execute: court_period = factor(build_court_period(term), levels = c("Burger", "Rehnquist", "Roberts"))
    court_period = factor(build_court_period(term), levels = c("Burger", "Rehnquist", "Roberts"))
  # Close function call arguments.
  )

# Table 1 models: baseline, justice FE, then justice+term FE.
# Fit linear model and store result in m3_1.
m3_1 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)
# Fit linear model and store result in m3_2.
m3_2 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + factor(justiceName), data = analysis_data)
# Fit linear model and store result in m3_3.
m3_3 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + factor(justiceName) + factor(term), data = analysis_data)

# Export Table 1 in text format.
# Export regression table using stargazer.
stargazer(m3_1, m3_2, m3_3, type = "text", out = "HW3 Table1.txt")

# Fit pitch-by-court-period interaction model used for Figure 3.
# Fit linear model and store result in m3_period_base.
m3_period_base <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)
# Fit linear model and store result in m3_period_int.
m3_period_int <- lm(petitioner_vote ~ pitch_diff * court_period + pr_petitioner_pos, data = analysis_data)

# Prediction grid for smooth interaction lines in Figure 3.
# Assign computed result to m3_period_frame.
m3_period_frame <- model.frame(m3_period_int)
# Assign computed result to grid3.
grid3 <- expand.grid(
  # Execute: pitch_diff = seq(min(m3_period_frame$pitch_diff), max(m3_period_frame$pitch_diff), length.out = 120),
  pitch_diff = seq(min(m3_period_frame$pitch_diff), max(m3_period_frame$pitch_diff), length.out = 120),
  # Execute: court_period = levels(m3_period_frame$court_period),
  court_period = levels(m3_period_frame$court_period),
  # Execute: pr_petitioner_pos = mean(m3_period_frame$pr_petitioner_pos)
  pr_petitioner_pos = mean(m3_period_frame$pr_petitioner_pos)
# Close function call arguments.
)
# Assign computed result to grid3$pred.
grid3$pred <- predict(m3_period_int, newdata = grid3)

# Plot predicted petitioner-vote probability across pitch by period.
# Create ggplot object and store in p3.
p3 <- ggplot(grid3, aes(x = pitch_diff, y = pred, color = court_period)) +
  # Execute: geom_line(linewidth = 0.9) +
  geom_line(linewidth = 0.9) +
  # Execute: labs(
  labs(
    # Execute: x = "Pitch Differential",
    x = "Pitch Differential",
    # Execute: y = "Predicted Pr(Vote for Petitioner)",
    y = "Predicted Pr(Vote for Petitioner)",
    # Execute: color = "Court Period"
    color = "Court Period"
  # Close function call arguments.
  ) +
  # Execute: theme_minimal()
  theme_minimal()

# Export Figure 3.
# Save figure to disk with specified dimensions.
ggsave("HW3 Fig3.png", plot = p3, width = 6, height = 4, dpi = 300)

# Progressive model sequence required for Table 2.
# Fit linear model and store result in m_prog1.
m_prog1 <- lm(petitioner_vote ~ pitch_diff, data = analysis_data)
# Fit linear model and store result in m_prog2.
m_prog2 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)
# Fit linear model and store result in m_prog3.
m_prog3 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac, data = analysis_data)
# Fit linear model and store result in m_prog4.
m_prog4 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period, data = analysis_data)
# Fit linear model and store result in m_prog5.
m_prog5 <- lm(petitioner_vote ~ pitch_diff * court_period + pr_petitioner_pos + sgpetac, data = analysis_data)
# Fit linear model and store result in m_prog6.
m_prog6 <- lm(petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period, data = analysis_data)

# Export Table 2 with models 1 through 6.
# Export regression table using stargazer.
stargazer(
  # Execute: m_prog1, m_prog2, m_prog3, m_prog4, m_prog5, m_prog6,
  m_prog1, m_prog2, m_prog3, m_prog4, m_prog5, m_prog6,
  # Execute: type = "text",
  type = "text",
  # Execute: out = "HW3 Table2.txt"
  out = "HW3 Table2.txt"
# Close function call arguments.
)

# Prediction grid for Figure 4 (pitch x court period interaction).
# Assign computed result to prog5_frame.
prog5_frame <- model.frame(m_prog5)
# Assign computed result to grid4.
grid4 <- expand.grid(
  # Execute: pitch_diff = seq(min(prog5_frame$pitch_diff), max(prog5_frame$pitch_diff), length.out = 120),
  pitch_diff = seq(min(prog5_frame$pitch_diff), max(prog5_frame$pitch_diff), length.out = 120),
  # Execute: court_period = levels(prog5_frame$court_period),
  court_period = levels(prog5_frame$court_period),
  # Execute: pr_petitioner_pos = mean(prog5_frame$pr_petitioner_pos),
  pr_petitioner_pos = mean(prog5_frame$pr_petitioner_pos),
  # Execute: sgpetac = mean(prog5_frame$sgpetac)
  sgpetac = mean(prog5_frame$sgpetac)
# Close function call arguments.
)
# Assign computed result to grid4$pred.
grid4$pred <- predict(m_prog5, newdata = grid4)

# Plot Figure 4 from Model 5 predictions.
# Create ggplot object and store in p4.
p4 <- ggplot(grid4, aes(x = pitch_diff, y = pred, color = court_period)) +
  # Execute: geom_line(linewidth = 0.9) +
  geom_line(linewidth = 0.9) +
  # Execute: labs(
  labs(
    # Execute: x = "Pitch Differential",
    x = "Pitch Differential",
    # Execute: y = "Predicted Pr(Vote for Petitioner)",
    y = "Predicted Pr(Vote for Petitioner)",
    # Execute: color = "Court Period"
    color = "Court Period"
  # Close function call arguments.
  ) +
  # Execute: theme_minimal()
  theme_minimal()

# Export Figure 4.
# Save figure to disk with specified dimensions.
ggsave("HW3 Fig4.png", plot = p4, width = 6, height = 4, dpi = 300)

# Prediction grid for Figure 5 (pitch x pr_petitioner_pos interaction).
# Assign computed result to prog6_frame.
prog6_frame <- model.frame(m_prog6)
# Assign computed result to ref_period.
ref_period <- first_mode(prog6_frame$court_period)
# Assign computed result to pr_levels.
pr_levels <- c(-2, -1, 0, 1, 2)

# Assign computed result to grid5.
grid5 <- expand.grid(
  # Execute: pitch_diff = seq(min(prog6_frame$pitch_diff), max(prog6_frame$pitch_diff), length.out = 120),
  pitch_diff = seq(min(prog6_frame$pitch_diff), max(prog6_frame$pitch_diff), length.out = 120),
  # Execute: pr_petitioner_pos = pr_levels,
  pr_petitioner_pos = pr_levels,
  # Execute: sgpetac = mean(prog6_frame$sgpetac),
  sgpetac = mean(prog6_frame$sgpetac),
  # Execute: court_period = ref_period
  court_period = ref_period
# Close function call arguments.
)
# Assign computed result to grid5$pred.
grid5$pred <- predict(m_prog6, newdata = grid5)
# Assign computed result to grid5$pr_petitioner_pos.
grid5$pr_petitioner_pos <- factor(grid5$pr_petitioner_pos, levels = pr_levels)

# Plot Figure 5 using representative values of pr_petitioner_pos.
# Create ggplot object and store in p5.
p5 <- ggplot(grid5, aes(x = pitch_diff, y = pred, color = pr_petitioner_pos)) +
  # Execute: geom_line(linewidth = 0.9) +
  geom_line(linewidth = 0.9) +
  # Execute: labs(
  labs(
    # Execute: x = "Pitch Differential",
    x = "Pitch Differential",
    # Execute: y = "Predicted Pr(Vote for Petitioner)",
    y = "Predicted Pr(Vote for Petitioner)",
    # Execute: color = "pr_petitioner_pos",
    color = "pr_petitioner_pos",
    # Execute: subtitle = paste("Court period held at:", ref_period)
    subtitle = paste("Court period held at:", ref_period)
  # Close function call arguments.
  ) +
  # Execute: theme_minimal()
  theme_minimal()

# Export Figure 5.
# Save figure to disk with specified dimensions.
ggsave("HW3 Fig5.png", plot = p5, width = 6, height = 4, dpi = 300)

# ---------------------------
# 5) Section 3.4 Outlier Analysis and Validity
# ---------------------------

# Use progressive Model 6 as the final model for outlier diagnostics.
# Assign computed result to final_model.
final_model <- m_prog6
# Compute assignment-required influence diagnostics.
# Assign computed result to stud_resid.
stud_resid <- rstudent(final_model)
# Assign computed result to leverage.
leverage <- hatvalues(final_model)
# Assign computed result to cooks_d.
cooks_d <- cooks.distance(final_model)
# Assign computed result to dffits_val.
dffits_val <- dffits(final_model)

# Build one diagnostics table per model row.
# Assign computed result to outlier_df.
outlier_df <- data.frame(
  # Execute: obs_id = seq_along(stud_resid),
  obs_id = seq_along(stud_resid),
  # Execute: studentized_resid = stud_resid,
  studentized_resid = stud_resid,
  # Execute: leverage = leverage,
  leverage = leverage,
  # Execute: cooks_d = cooks_d,
  cooks_d = cooks_d,
  # Execute: dffits = dffits_val
  dffits = dffits_val
# Close function call arguments.
)

# Derive cutoffs from assignment formulas.
# Assign computed result to n_model.
n_model <- nrow(model.frame(final_model))
# Assign computed result to k_model.
k_model <- length(coef(final_model)) - 1
# Assign computed result to thr_resid.
thr_resid <- 2
# Assign computed result to thr_lev.
thr_lev <- (2 * k_model + 2) / n_model
# Assign computed result to thr_cook.
thr_cook <- 4 / n_model
# Assign computed result to thr_dffits.
thr_dffits <- 2 * sqrt(k_model / n_model)

# Flag rows crossing any threshold and rows crossing all thresholds.
# Assign computed result to outlier_df.
outlier_df <- outlier_df %>%
  # Execute: dplyr::mutate(
  dplyr::mutate(
    # Execute: is_outlier = abs(studentized_resid) > thr_resid |
    is_outlier = abs(studentized_resid) > thr_resid |
      # Execute: leverage > thr_lev |
      leverage > thr_lev |
      # Execute: cooks_d > thr_cook |
      cooks_d > thr_cook |
      # Execute: abs(dffits) > thr_dffits,
      abs(dffits) > thr_dffits,
    # Execute: is_egregious = abs(studentized_resid) > thr_resid &
    is_egregious = abs(studentized_resid) > thr_resid &
      # Execute: leverage > thr_lev &
      leverage > thr_lev &
      # Execute: cooks_d > thr_cook &
      cooks_d > thr_cook &
      # Execute: abs(dffits) > thr_dffits,
      abs(dffits) > thr_dffits,
    # Execute: abs_dffits = abs(dffits)
    abs_dffits = abs(dffits)
  # Close function call arguments.
  )

# Visual diagnostics plot: leverage vs |DFFITS| with threshold lines.
# Create ggplot object and store in p6.
p6 <- ggplot(outlier_df, aes(x = abs_dffits, y = leverage, color = is_outlier)) +
  # Execute: geom_point(alpha = 0.75) +
  geom_point(alpha = 0.75) +
  # Execute: geom_hline(yintercept = thr_lev, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = thr_lev, linetype = "dashed", color = "blue") +
  # Execute: geom_vline(xintercept = thr_dffits, linetype = "dashed", color = "red") +
  geom_vline(xintercept = thr_dffits, linetype = "dashed", color = "red") +
  # Execute: geom_text(
  geom_text(
    # Execute: data = subset(outlier_df, is_egregious),
    data = subset(outlier_df, is_egregious),
    # Execute: aes(label = obs_id),
    aes(label = obs_id),
    # Execute: vjust = -0.4,
    vjust = -0.4,
    # Execute: size = 2.8,
    size = 2.8,
    # Execute: check_overlap = TRUE
    check_overlap = TRUE
  # Close function call arguments.
  ) +
  # Execute: scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  # Execute: labs(
  labs(
    # Execute: x = "|DFFITS|",
    x = "|DFFITS|",
    # Execute: y = "Leverage",
    y = "Leverage",
    # Execute: color = "Outlier"
    color = "Outlier"
  # Close function call arguments.
  ) +
  # Execute: theme_minimal()
  theme_minimal()

# Export Figure 6.
# Save figure to disk with specified dimensions.
ggsave("HW3 Fig6.png", plot = p6, width = 6, height = 4, dpi = 300)

# Align analysis_data rows to the exact estimation sample used in final_model.
# Assign computed result to model_rows.
model_rows <- as.integer(rownames(model.frame(final_model)))
# Assign computed result to analysis_data_used.
analysis_data_used <- analysis_data[model_rows, ]
# Remove flagged outliers for robustness comparison model.
# Assign computed result to clean_data.
clean_data <- analysis_data_used[!outlier_df$is_outlier, ]

# Fit final specification on full sample and outlier-excluded sample.
# Fit linear model and store result in model_full.
model_full <- lm(
  # Execute: petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period,
  petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period,
  # Execute: data = analysis_data_used
  data = analysis_data_used
# Close function call arguments.
)
# Fit linear model and store result in model_clean.
model_clean <- lm(
  # Execute: petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period,
  petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period,
  # Execute: data = clean_data
  data = clean_data
# Close function call arguments.
)

# Export full-vs-clean comparison table (Table 3).
# Export regression table using stargazer.
stargazer(model_full, model_clean, type = "text", out = "HW3 Table3.txt")

# Save concise diagnostics summary values for narrative support.
# Assign computed result to outlier_summary.
outlier_summary <- c(
  # Execute: "Outlier Summary (Section 3.4)",
  "Outlier Summary (Section 3.4)",
  # Execute: paste("Model observations:", n_model),
  paste("Model observations:", n_model),
  # Execute: paste("k (predictors):", k_model),
  paste("k (predictors):", k_model),
  # Execute: paste("Threshold |studentized residual| >", thr_resid),
  paste("Threshold |studentized residual| >", thr_resid),
  # Execute: paste("Threshold leverage >", round(thr_lev, 6)),
  paste("Threshold leverage >", round(thr_lev, 6)),
  # Execute: paste("Threshold Cook's D >", round(thr_cook, 6)),
  paste("Threshold Cook's D >", round(thr_cook, 6)),
  # Execute: paste("Threshold |DFFITS| >", round(thr_dffits, 6)),
  paste("Threshold |DFFITS| >", round(thr_dffits, 6)),
  # Execute: paste("Flagged outliers (any threshold):", sum(outlier_df$is_outlier)),
  paste("Flagged outliers (any threshold):", sum(outlier_df$is_outlier)),
  # Execute: paste("Flagged egregious outliers (all thresholds):", sum(outlier_df$is_egregious))
  paste("Flagged egregious outliers (all thresholds):", sum(outlier_df$is_egregious))
# Close function call arguments.
)
# Write text output to file.
writeLines(outlier_summary, "HW3_OutlierSummary.txt")

# ---------------------------
# 6) Draft Answers for Q5-Q17
# ---------------------------

# Format numeric outputs consistently for text answers.
# Assign computed result to fmt_num.
fmt_num <- function(x, digits = 3) {
  # Execute: formatC(x, format = "f", digits = digits)
  formatC(x, format = "f", digits = digits)
# Close current code block.
}

# Q5
# Build Q5 narrative from computed distribution statistics.
# Assign computed result to q5_text.
q5_text <- paste0(
  # Execute: "petitioner_vote is binary and fairly balanced but slightly petitioner-leaning: mean = ",
  "petitioner_vote is binary and fairly balanced but slightly petitioner-leaning: mean = ",
  # Execute: fmt_num(mean(justice_data$petitioner_vote, na.rm = TRUE), 3),
  fmt_num(mean(justice_data$petitioner_vote, na.rm = TRUE), 3),
  # Execute: ", median = ", fmt_num(median(justice_data$petitioner_vote, na.rm = TRUE), 0), ". ",
  ", median = ", fmt_num(median(justice_data$petitioner_vote, na.rm = TRUE), 0), ". ",
  # Execute: "pitch_diff is centered near zero (mean = ", fmt_num(mean(justice_data$pitch_diff, na.rm = TRUE), 3),
  "pitch_diff is centered near zero (mean = ", fmt_num(mean(justice_data$pitch_diff, na.rm = TRUE), 3),
  # Execute: ", median = ", fmt_num(median(justice_data$pitch_diff, na.rm = TRUE), 3),
  ", median = ", fmt_num(median(justice_data$pitch_diff, na.rm = TRUE), 3),
  # Execute: ") but has an extreme lower-tail outlier (min = ", fmt_num(min(justice_data$pitch_diff, na.rm = TRUE), 3),
  ") but has an extreme lower-tail outlier (min = ", fmt_num(min(justice_data$pitch_diff, na.rm = TRUE), 3),
  # Execute: ", max = ", fmt_num(max(justice_data$pitch_diff, na.rm = TRUE), 3), "). ",
  ", max = ", fmt_num(max(justice_data$pitch_diff, na.rm = TRUE), 3), "). ",
  # Execute: "petitioner_harvard_pos is right-skewed (mean = ",
  "petitioner_harvard_pos is right-skewed (mean = ",
  # Execute: fmt_num(mean(justice_data$petitioner_harvard_pos, na.rm = TRUE), 3),
  fmt_num(mean(justice_data$petitioner_harvard_pos, na.rm = TRUE), 3),
  # Execute: ", median = ", fmt_num(median(justice_data$petitioner_harvard_pos, na.rm = TRUE), 0),
  ", median = ", fmt_num(median(justice_data$petitioner_harvard_pos, na.rm = TRUE), 0),
  # Execute: ", max = ", fmt_num(max(justice_data$petitioner_harvard_pos, na.rm = TRUE), 0), "). ",
  ", max = ", fmt_num(max(justice_data$petitioner_harvard_pos, na.rm = TRUE), 0), "). ",
  # Execute: "There are ", sum(is.na(justice_data$petitioner_harvard_pos)),
  "There are ", sum(is.na(justice_data$petitioner_harvard_pos)),
  # Execute: " missing values in petitioner_harvard_pos and related text variables, so complete-case models drop observat...
  " missing values in petitioner_harvard_pos and related text variables, so complete-case models drop observations."
# Close function call arguments.
)

# Q6/Q7
# Build Q6 narrative on variable construction and interpretation caveats.
# Assign computed result to q6_text.
q6_text <- paste(
  # Execute: "pitch_diff is measured as petitioner_pitch minus respondent_pitch and captures whether a justice spoke at h...
  "pitch_diff is measured as petitioner_pitch minus respondent_pitch and captures whether a justice spoke at higher or lower pitch to one side relative to the other.",
  # Execute: "Supplemental documentation indicates the pitch measure is standardized before differencing, which helps com...
  "Supplemental documentation indicates the pitch measure is standardized before differencing, which helps comparability across speakers (e.g., men vs. women with different baseline pitch ranges) and reduces sensitivity to absolute-frequency measurement noise.",
  # Execute: "petitioner_harvard_pos is constructed by counting words directed to the petitioner that match a predefined ...
  "petitioner_harvard_pos is constructed by counting words directed to the petitioner that match a predefined Harvard-IV positive-word dictionary.",
  # Execute: "A challenge is that dictionary methods ignore context: sarcasm, negation, legal jargon, and polysemy can ca...
  "A challenge is that dictionary methods ignore context: sarcasm, negation, legal jargon, and polysemy can cause words to be counted as 'positive' even when tone is neutral or negative."
# Close function call arguments.
)

# Build Q7 narrative on dictionary-based measurement error risks.
# Assign computed result to q7_text.
q7_text <- paste(
  # Execute: "Dictionary-based coding can introduce false positives (words marked positive in neutral/legal context), fal...
  "Dictionary-based coding can introduce false positives (words marked positive in neutral/legal context), false negatives (missed positive phrasing not in the dictionary), and context errors (negation, irony, sarcasm).",
  # Execute: "These measurement errors generally add noise and can attenuate coefficients toward zero, but if misclassifi...
  "These measurement errors generally add noise and can attenuate coefficients toward zero, but if misclassification is systematic by justice, issue area, or period, it can also bias effect estimates and distort substantive interpretation."
# Close function call arguments.
)

# Q8
# Split Figure 1 summaries into no-amicus vs amicus columns for delta reporting.
# Assign computed result to fig1_no.
fig1_no <- fig1_summary %>%
  # Execute: dplyr::filter(sgpetac_label == "No Amicus") %>%
  dplyr::filter(sgpetac_label == "No Amicus") %>%
  # Execute: dplyr::select(justiceName, prop_no = prop_petitioner_vote)
  dplyr::select(justiceName, prop_no = prop_petitioner_vote)
# Assign computed result to fig1_yes.
fig1_yes <- fig1_summary %>%
  # Execute: dplyr::filter(sgpetac_label == "Amicus") %>%
  dplyr::filter(sgpetac_label == "Amicus") %>%
  # Execute: dplyr::select(justiceName, prop_yes = prop_petitioner_vote)
  dplyr::select(justiceName, prop_yes = prop_petitioner_vote)
# Join and compute within-justice change in petitioner-vote share.
# Assign computed result to fig1_change.
fig1_change <- dplyr::left_join(fig1_no, fig1_yes, by = "justiceName") %>%
  # Execute: dplyr::mutate(delta = prop_yes - prop_no) %>%
  dplyr::mutate(delta = prop_yes - prop_no) %>%
  # Execute: dplyr::arrange(justiceName)
  dplyr::arrange(justiceName)

# Convert Figure 1 change table to a compact sentence fragment.
# Assign computed result to q8_detail.
q8_detail <- paste(
  # Execute: paste0(
  paste0(
    # Execute: fig1_change$justiceName, ": ",
    fig1_change$justiceName, ": ",
    # Execute: fmt_num(fig1_change$prop_no, 3), " (No Amicus) -> ",
    fmt_num(fig1_change$prop_no, 3), " (No Amicus) -> ",
    # Execute: fmt_num(fig1_change$prop_yes, 3), " (Amicus), delta = ",
    fmt_num(fig1_change$prop_yes, 3), " (Amicus), delta = ",
    # Execute: fmt_num(fig1_change$delta, 3)
    fmt_num(fig1_change$delta, 3)
  # Close function call arguments.
  ),
  # Execute: collapse = "; "
  collapse = "; "
# Close function call arguments.
)

# Build Q8 narrative from justice-level amicus differences.
# Assign computed result to q8_text.
q8_text <- paste(
  # Execute: "Across all three chief justices, petitioner-win rates are higher when the Solicitor General supports the pe...
  "Across all three chief justices, petitioner-win rates are higher when the Solicitor General supports the petitioner.",
  # Execute: q8_detail,
  q8_detail,
  # Execute: "A plausible interpretation is that SG support signals legal merit and institutional credibility, but the pa...
  "A plausible interpretation is that SG support signals legal merit and institutional credibility, but the pattern may also reflect case selection and omitted confounders."
# Close function call arguments.
)

# Q9
# Split Figure 2 summaries into below-vs-above pitch groups.
# Assign computed result to fig2_below.
fig2_below <- fig2_summary %>%
  # Execute: dplyr::filter(high_pitch_diff == "Below Avg. Pitch Differential") %>%
  dplyr::filter(high_pitch_diff == "Below Avg. Pitch Differential") %>%
  # Execute: dplyr::select(court_period, prop_below = prop_petitioner_vote)
  dplyr::select(court_period, prop_below = prop_petitioner_vote)
# Assign computed result to fig2_above.
fig2_above <- fig2_summary %>%
  # Execute: dplyr::filter(high_pitch_diff == "Above Avg. Pitch Differential") %>%
  dplyr::filter(high_pitch_diff == "Above Avg. Pitch Differential") %>%
  # Execute: dplyr::select(court_period, prop_above = prop_petitioner_vote)
  dplyr::select(court_period, prop_above = prop_petitioner_vote)
# Join and compute period-specific differences.
# Assign computed result to fig2_change.
fig2_change <- dplyr::left_join(fig2_below, fig2_above, by = "court_period") %>%
  # Execute: dplyr::mutate(delta_above_minus_below = prop_above - prop_below) %>%
  dplyr::mutate(delta_above_minus_below = prop_above - prop_below) %>%
  # Execute: dplyr::arrange(court_period)
  dplyr::arrange(court_period)

# Convert Figure 2 period deltas to narrative-ready text.
# Assign computed result to q9_detail.
q9_detail <- paste(
  # Execute: paste0(
  paste0(
    # Execute: fig2_change$court_period, ": Above = ", fmt_num(fig2_change$prop_above, 3),
    fig2_change$court_period, ": Above = ", fmt_num(fig2_change$prop_above, 3),
    # Execute: ", Below = ", fmt_num(fig2_change$prop_below, 3),
    ", Below = ", fmt_num(fig2_change$prop_below, 3),
    # Execute: ", delta(Above-Below) = ", fmt_num(fig2_change$delta_above_minus_below, 3)
    ", delta(Above-Below) = ", fmt_num(fig2_change$delta_above_minus_below, 3)
  # Close function call arguments.
  ),
  # Execute: collapse = "; "
  collapse = "; "
# Close function call arguments.
)

# Build Q9 narrative from period-level pitch-group contrasts.
# Assign computed result to q9_text.
q9_text <- paste(
  # Execute: "Higher pitch-difference observations are generally associated with lower petitioner-win rates in Rehnquist ...
  "Higher pitch-difference observations are generally associated with lower petitioner-win rates in Rehnquist and Roberts periods, while the Burger period shows little difference.",
  # Execute: q9_detail,
  q9_detail,
  # Execute: "This suggests the pitch-vote relationship is not constant over time and appears stronger in later court per...
  "This suggests the pitch-vote relationship is not constant over time and appears stronger in later court periods."
# Close function call arguments.
)

# Q10/Q11/Q12/Q13/Q14
# Pull model summaries and key statistics used in Q10-Q14 text.
# Assign computed result to m3_1_coef.
m3_1_coef <- coef(summary(m3_1))
# Assign computed result to m3_2_coef.
m3_2_coef <- coef(summary(m3_2))
# Assign computed result to m3_3_coef.
m3_3_coef <- coef(summary(m3_3))

# Assign computed result to pitch_b.
pitch_b <- m3_1_coef["pitch_diff", "Estimate"]
# Assign computed result to pitch_p.
pitch_p <- m3_1_coef["pitch_diff", "Pr(>|t|)"]
# Assign computed result to pr_b.
pr_b <- m3_1_coef["pr_petitioner_pos", "Estimate"]
# Assign computed result to pr_p.
pr_p <- m3_1_coef["pr_petitioner_pos", "Pr(>|t|)"]

# Assign computed result to adj_m3_1.
adj_m3_1 <- summary(m3_1)$adj.r.squared
# Assign computed result to adj_m3_2.
adj_m3_2 <- summary(m3_2)$adj.r.squared
# Assign computed result to adj_m3_3.
adj_m3_3 <- summary(m3_3)$adj.r.squared

# Assign computed result to justice_sig_n.
justice_sig_n <- sum(
  # Execute: grepl("^factor\\(justiceName\\)", rownames(m3_2_coef)) &
  grepl("^factor\\(justiceName\\)", rownames(m3_2_coef)) &
    # Execute: m3_2_coef[, "Pr(>|t|)"] < 0.05
    m3_2_coef[, "Pr(>|t|)"] < 0.05
# Close function call arguments.
)
# Assign computed result to term_sig_n.
term_sig_n <- sum(
  # Execute: grepl("^factor\\(term\\)", rownames(m3_3_coef)) &
  grepl("^factor\\(term\\)", rownames(m3_3_coef)) &
    # Execute: m3_3_coef[, "Pr(>|t|)"] < 0.05
    m3_3_coef[, "Pr(>|t|)"] < 0.05
# Close function call arguments.
)

# Build Q10 narrative on baseline predictors.
# Assign computed result to q10_text.
q10_text <- paste0(
  # Execute: "In the baseline model (m3_1), pitch_diff is negative and statistically significant (b = ",
  "In the baseline model (m3_1), pitch_diff is negative and statistically significant (b = ",
  # Execute: fmt_num(pitch_b, 3), ", p = ", signif(pitch_p, 3),
  fmt_num(pitch_b, 3), ", p = ", signif(pitch_p, 3),
  # Execute: "), implying higher petitioner-vs-respondent pitch is associated with a lower probability of a petitioner vo...
  "), implying higher petitioner-vs-respondent pitch is associated with a lower probability of a petitioner vote. ",
  # Execute: "pr_petitioner_pos is not statistically significant (b = ", fmt_num(pr_b, 3),
  "pr_petitioner_pos is not statistically significant (b = ", fmt_num(pr_b, 3),
  # Execute: ", p = ", signif(pr_p, 3), "), so lexical positivity differences do not show a clear linear association in t...
  ", p = ", signif(pr_p, 3), "), so lexical positivity differences do not show a clear linear association in this specification."
# Close function call arguments.
)

# Build Q11 narrative on the effect of justice fixed effects.
# Assign computed result to q11_text.
q11_text <- paste0(
  # Execute: "Controlling for justice fixed effects is important because justices have persistent baseline voting tendenc...
  "Controlling for justice fixed effects is important because justices have persistent baseline voting tendencies that can confound speech-effect estimates. ",
  # Execute: "After adding justice controls, adjusted R2 rises from ", fmt_num(adj_m3_1, 3),
  "After adding justice controls, adjusted R2 rises from ", fmt_num(adj_m3_1, 3),
  # Execute: " to ", fmt_num(adj_m3_2, 3), ", and ", justice_sig_n,
  " to ", fmt_num(adj_m3_2, 3), ", and ", justice_sig_n,
  # Execute: " justice indicators are significant at p < 0.05. ",
  " justice indicators are significant at p < 0.05. ",
  # Execute: "The pitch_diff estimate remains negative and similar in magnitude, indicating the relationship is not drive...
  "The pitch_diff estimate remains negative and similar in magnitude, indicating the relationship is not driven only by cross-justice composition."
# Close function call arguments.
)

# Build Q12 narrative on adding term indicators and adjusted R-squared change.
# Assign computed result to q12_text.
q12_text <- paste0(
  # Execute: "Adding term indicators further increases adjusted R2 from ", fmt_num(adj_m3_2, 3),
  "Adding term indicators further increases adjusted R2 from ", fmt_num(adj_m3_2, 3),
  # Execute: " to ", fmt_num(adj_m3_3, 3), ", with ", term_sig_n,
  " to ", fmt_num(adj_m3_3, 3), ", with ", term_sig_n,
  # Execute: " term indicators significant at p < 0.05. ",
  " term indicators significant at p < 0.05. ",
  # Execute: "This indicates nontrivial time-period variation in voting behavior and better overall fit once period-speci...
  "This indicates nontrivial time-period variation in voting behavior and better overall fit once period-specific shocks are absorbed."
# Close function call arguments.
)

# Assign computed result to coef_period.
coef_period <- coef(m3_period_int)
# Assign computed result to slope_burger.
slope_burger <- unname(coef_period["pitch_diff"])
# Assign computed result to slope_rehnquist.
slope_rehnquist <- slope_burger + unname(coef_period["pitch_diff:court_periodRehnquist"])
# Assign computed result to slope_roberts.
slope_roberts <- slope_burger + unname(coef_period["pitch_diff:court_periodRoberts"])

# Build Q13 narrative from period-specific interaction slopes.
# Assign computed result to q13_text.
q13_text <- paste0(
  # Execute: "In the pitch_diff * court_period interaction model, the negative slope is strongest in the Rehnquist period...
  "In the pitch_diff * court_period interaction model, the negative slope is strongest in the Rehnquist period and also negative in the Roberts period: ",
  # Execute: "Burger slope = ", fmt_num(slope_burger, 3),
  "Burger slope = ", fmt_num(slope_burger, 3),
  # Execute: ", Rehnquist slope = ", fmt_num(slope_rehnquist, 3),
  ", Rehnquist slope = ", fmt_num(slope_rehnquist, 3),
  # Execute: ", Roberts slope = ", fmt_num(slope_roberts, 3), ". ",
  ", Roberts slope = ", fmt_num(slope_roberts, 3), ". ",
  # Execute: "This supports a period-varying relationship where vocal-pitch effects are modest in Burger-era data and mor...
  "This supports a period-varying relationship where vocal-pitch effects are modest in Burger-era data and more negative afterward."
# Close function call arguments.
)

# Assign computed result to adj_prog.
adj_prog <- sapply(
  # Execute: list(m_prog1, m_prog2, m_prog3, m_prog4, m_prog5, m_prog6),
  list(m_prog1, m_prog2, m_prog3, m_prog4, m_prog5, m_prog6),
  # Execute: function(m) summary(m)$adj.r.squared
  function(m) summary(m)$adj.r.squared
# Close function call arguments.
)
# Build Q14 narrative from progressive-model fit comparisons.
# Assign computed result to q14_text.
q14_text <- paste0(
  # Execute: "Progressive models show small fit gains from Model 1 to 2, a clearer jump when sgpetac is added (Model 3), ...
  "Progressive models show small fit gains from Model 1 to 2, a clearer jump when sgpetac is added (Model 3), and minor incremental changes from additional period and interaction terms. ",
  # Execute: "Adjusted R2 values are: M1=", fmt_num(adj_prog[1], 3),
  "Adjusted R2 values are: M1=", fmt_num(adj_prog[1], 3),
  # Execute: ", M2=", fmt_num(adj_prog[2], 3),
  ", M2=", fmt_num(adj_prog[2], 3),
  # Execute: ", M3=", fmt_num(adj_prog[3], 3),
  ", M3=", fmt_num(adj_prog[3], 3),
  # Execute: ", M4=", fmt_num(adj_prog[4], 3),
  ", M4=", fmt_num(adj_prog[4], 3),
  # Execute: ", M5=", fmt_num(adj_prog[5], 3),
  ", M5=", fmt_num(adj_prog[5], 3),
  # Execute: ", M6=", fmt_num(adj_prog[6], 3), ". ",
  ", M6=", fmt_num(adj_prog[6], 3), ". ",
  # Execute: "Substantively, SG amicus support is the strongest added predictor in this set, while interaction terms mode...
  "Substantively, SG amicus support is the strongest added predictor in this set, while interaction terms modestly refine but do not radically change model fit."
# Close function call arguments.
)

# Q16/Q17
# Count how many diagnostic rules each observation violates.
# Assign computed result to outlier_df.
outlier_df <- outlier_df %>%
  # Execute: dplyr::mutate(
  dplyr::mutate(
    # Execute: n_flags = as.integer(abs(studentized_resid) > thr_resid) +
    n_flags = as.integer(abs(studentized_resid) > thr_resid) +
      # Execute: as.integer(leverage > thr_lev) +
      as.integer(leverage > thr_lev) +
      # Execute: as.integer(cooks_d > thr_cook) +
      as.integer(cooks_d > thr_cook) +
      # Execute: as.integer(abs(dffits) > thr_dffits)
      as.integer(abs(dffits) > thr_dffits)
  # Close function call arguments.
  )

# Attach case identifiers to outlier diagnostics for interpretability in Q16.
# Assign computed result to outlier_cases.
outlier_cases <- cbind(
  # Execute: outlier_df,
  outlier_df,
  # Execute: analysis_data_used[, c("docketId", "docket", "justiceName", "term")]
  analysis_data_used[, c("docketId", "docket", "justiceName", "term")]
# Close function call arguments.
)

# Rank outliers by number of threshold violations and influence magnitude.
# Assign computed result to top_outliers.
top_outliers <- outlier_cases %>%
  # Execute: dplyr::filter(is_outlier) %>%
  dplyr::filter(is_outlier) %>%
  # Execute: dplyr::arrange(desc(n_flags), desc(abs_dffits)) %>%
  dplyr::arrange(desc(n_flags), desc(abs_dffits)) %>%
  # Execute: dplyr::slice(1:5)
  dplyr::slice(1:5)

# Convert top outlier rows into concise text snippet.
# Assign computed result to q16_top.
q16_top <- paste(
  # Execute: paste0(
  paste0(
    # Execute: "obs ", top_outliers$obs_id, " (", top_outliers$docketId, ", ",
    "obs ", top_outliers$obs_id, " (", top_outliers$docketId, ", ",
    # Execute: top_outliers$justiceName, ", term ", top_outliers$term,
    top_outliers$justiceName, ", term ", top_outliers$term,
    # Execute: ", flags=", top_outliers$n_flags, ")"
    ", flags=", top_outliers$n_flags, ")"
  # Close function call arguments.
  ),
  # Execute: collapse = "; "
  collapse = "; "
# Close function call arguments.
)

# Build Q16 narrative summarizing outlier counts and notable cases.
# Assign computed result to q16_text.
q16_text <- paste0(
  # Execute: "Using assignment thresholds, ", sum(outlier_df$is_outlier),
  "Using assignment thresholds, ", sum(outlier_df$is_outlier),
  # Execute: " observations are flagged by at least one diagnostic and ",
  " observations are flagged by at least one diagnostic and ",
  # Execute: sum(outlier_df$is_egregious), " are egregious (all four criteria). ",
  sum(outlier_df$is_egregious), " are egregious (all four criteria). ",
  # Execute: "Top flagged observations include: ", q16_top, ". ",
  "Top flagged observations include: ", q16_top, ". ",
  # Execute: "Most high-impact cases are flagged by leverage, Cook's D, and DFFITS simultaneously, while none exceed the ...
  "Most high-impact cases are flagged by leverage, Cook's D, and DFFITS simultaneously, while none exceed the |studentized residual| > 2 rule."
# Close function call arguments.
)

# Compare coefficients and standard errors between full and clean models.
# Assign computed result to coef_full.
coef_full <- coef(summary(model_full))
# Assign computed result to coef_clean.
coef_clean <- coef(summary(model_clean))
# Assign computed result to common_terms.
common_terms <- intersect(rownames(coef_full), rownames(coef_clean))
# Assign computed result to comp.
comp <- data.frame(
  # Execute: term = common_terms,
  term = common_terms,
  # Execute: b_full = coef_full[common_terms, "Estimate"],
  b_full = coef_full[common_terms, "Estimate"],
  # Execute: se_full = coef_full[common_terms, "Std. Error"],
  se_full = coef_full[common_terms, "Std. Error"],
  # Execute: b_clean = coef_clean[common_terms, "Estimate"],
  b_clean = coef_clean[common_terms, "Estimate"],
  # Execute: se_clean = coef_clean[common_terms, "Std. Error"]
  se_clean = coef_clean[common_terms, "Std. Error"]
# Close function call arguments.
)

# Helper for standardized "full -> clean" coefficient text formatting.
# Assign computed result to get_comp.
get_comp <- function(term_name) {
  # Assign computed result to row.
  row <- comp[comp$term == term_name, , drop = FALSE]
  # Execute: paste0(
  paste0(
    # Execute: term_name, ": b ", fmt_num(row$b_full, 3), " -> ", fmt_num(row$b_clean, 3),
    term_name, ": b ", fmt_num(row$b_full, 3), " -> ", fmt_num(row$b_clean, 3),
    # Execute: "; SE ", fmt_num(row$se_full, 3), " -> ", fmt_num(row$se_clean, 3)
    "; SE ", fmt_num(row$se_full, 3), " -> ", fmt_num(row$se_clean, 3)
  # Close function call arguments.
  )
# Close current code block.
}

# Build Q17 narrative from side-by-side coefficient/SE changes.
# Assign computed result to q17_text.
q17_text <- paste(
  # Execute: "Removing flagged outliers changes some coefficients but preserves the core qualitative finding that pitch_d...
  "Removing flagged outliers changes some coefficients but preserves the core qualitative finding that pitch_diff remains negative and SG amicus support remains positive/significant.",
  # Execute: get_comp("pitch_diff"),
  get_comp("pitch_diff"),
  # Execute: get_comp("sgpetac"),
  get_comp("sgpetac"),
  # Execute: get_comp("pitch_diff:pr_petitioner_pos"),
  get_comp("pitch_diff:pr_petitioner_pos"),
  # Execute: "The clean-sample model has similar directional conclusions but somewhat different magnitudes and uncertaint...
  "The clean-sample model has similar directional conclusions but somewhat different magnitudes and uncertainty, so results look broadly robust with moderate sensitivity for interaction terms."
# Close function call arguments.
)

# Assemble all section draft answers into one output vector.
# Assign computed result to answers_q5_q17.
answers_q5_q17 <- c(
  # Execute: "Section 3.2-3.4 Draft Answers (Q5-Q17)",
  "Section 3.2-3.4 Draft Answers (Q5-Q17)",
  # Execute: "",
  "",
  # Execute: paste0("Q5: ", q5_text),
  paste0("Q5: ", q5_text),
  # Execute: "",
  "",
  # Execute: paste0("Q6: ", q6_text),
  paste0("Q6: ", q6_text),
  # Execute: "",
  "",
  # Execute: paste0("Q7: ", q7_text),
  paste0("Q7: ", q7_text),
  # Execute: "",
  "",
  # Execute: paste0("Q8: ", q8_text),
  paste0("Q8: ", q8_text),
  # Execute: "",
  "",
  # Execute: paste0("Q9: ", q9_text),
  paste0("Q9: ", q9_text),
  # Execute: "",
  "",
  # Execute: paste0("Q10: ", q10_text),
  paste0("Q10: ", q10_text),
  # Execute: "",
  "",
  # Execute: paste0("Q11: ", q11_text),
  paste0("Q11: ", q11_text),
  # Execute: "",
  "",
  # Execute: paste0("Q12: ", q12_text),
  paste0("Q12: ", q12_text),
  # Execute: "",
  "",
  # Execute: paste0("Q13: ", q13_text),
  paste0("Q13: ", q13_text),
  # Execute: "",
  "",
  # Execute: paste0("Q14: ", q14_text),
  paste0("Q14: ", q14_text),
  # Execute: "",
  "",
  # Execute: "Q15: IGNORE/DO NOT ANSWER.",
  "Q15: IGNORE/DO NOT ANSWER.",
  # Execute: "",
  "",
  # Execute: paste0("Q16: ", q16_text),
  paste0("Q16: ", q16_text),
  # Execute: "",
  "",
  # Execute: paste0("Q17: ", q17_text)
  paste0("Q17: ", q17_text)
# Close function call arguments.
)

# Save Q5-Q17 draft answers for use in the final PDF write-up.
# Write text output to file.
writeLines(answers_q5_q17, "HW3_DraftAnswers_Q5_Q17.txt")
