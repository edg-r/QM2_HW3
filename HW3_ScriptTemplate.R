# GPCO 454 - Quantitative Methods II - Winter 2025
# Homework 3 - End-to-End Script

# ---------------------------
# 1) Setup
# ---------------------------

# Keep commented per submission rules.
# setwd("/path/to/HW3")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stargazer)
})

build_court_period <- function(term_vec) {
  dplyr::case_when(
    term_vec >= 1969 & term_vec <= 1985 ~ "Burger",
    term_vec >= 1986 & term_vec <= 2004 ~ "Rehnquist",
    term_vec >= 2005 ~ "Roberts",
    TRUE ~ NA_character_
  )
}

first_mode <- function(x) {
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

# ---------------------------
# 2) Section 3.1 Preliminaries
# ---------------------------

justice_data <- read.table(
  "justice_results.tab",
  header = TRUE,
  sep = "\t",
  encoding = "ISO-8859-1"
)

str(justice_data)
summary(justice_data)

n_unique_dockets <- dplyr::n_distinct(justice_data$docket)
n_unique_caseid <- dplyr::n_distinct(justice_data$caseId)
n_unique_justices <- dplyr::n_distinct(justice_data$justiceName)

missing_by_var <- colSums(is.na(justice_data))
total_missing <- sum(is.na(justice_data))
missing_table <- data.frame(
  variable = names(missing_by_var),
  missing_n = as.integer(missing_by_var),
  missing_pct = round(as.integer(missing_by_var) / nrow(justice_data) * 100, 2)
)
missing_table_nonzero <- missing_table %>%
  dplyr::filter(missing_n > 0) %>%
  dplyr::arrange(desc(missing_n), variable)

write.table(
  missing_table_nonzero,
  file = "HW3_MissingValues_3_1.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

n_unique_docket_justice <- nrow(unique(justice_data[, c("docketId", "justiceName")]))
n_duplicate_docket_justice <- nrow(justice_data) - n_unique_docket_justice

q1_answer <- paste0(
  "This dataset contains ", nrow(justice_data), " justice-level observations across ",
  n_unique_dockets, " unique dockets (", n_unique_caseid, " unique case IDs) and ",
  n_unique_justices, " justices. ",
  "Each row includes vote outcomes (petitioner_vote), oral-argument speech measures (including pitch_diff), and case context such as term and amicus variables. ",
  "This structure supports testing whether oral-argument emotional cues are associated with Supreme Court voting behavior."
)

q2_answer <- paste0(
  "The unit of observation is one justice-docket vote record. ",
  "docketId + justiceName combinations are unique in this dataset (duplicates = ",
  n_duplicate_docket_justice, "), which is consistent with that unit."
)

q3_answer <- paste(
  "A vote in favor of the petitioner is coded as petitioner_vote = 1.",
  "This is substantively important because these votes determine case outcomes and legal precedent.",
  "The assignment uses this outcome to evaluate whether emotional dynamics in oral argument relate to judicial decisions."
)

q4_answer <- paste0(
  "There are ", total_missing, " missing values in total. ",
  nrow(missing_table_nonzero), " variables have missing data, and each has 232 missing observations (4.45% of rows). ",
  "Missingness matters because complete-case estimation can reduce sample size and potentially bias results if missingness is non-random."
)

answers_q1_q4 <- c(
  "Section 3.1 Draft Answers (Q1-Q4)",
  "",
  paste0("Q1: ", q1_answer),
  "",
  paste0("Q2: ", q2_answer),
  "",
  paste0("Q3: ", q3_answer),
  "",
  paste0("Q4: ", q4_answer),
  "",
  "Variables with missing values (Q4 support):",
  paste0(
    missing_table_nonzero$variable, ": ",
    missing_table_nonzero$missing_n, " (",
    missing_table_nonzero$missing_pct, "%)"
  )
)

writeLines(answers_q1_q4, con = "HW3_Section3_1_Q1_Q4_DraftAnswers.txt")

# ---------------------------
# 3) Section 3.2 Descriptive Stats and Plots
# ---------------------------

summary_3_2 <- summary(justice_data[, c("petitioner_vote", "pitch_diff", "petitioner_harvard_pos")])
print(summary_3_2)

pitch_mean <- mean(justice_data$pitch_diff, na.rm = TRUE)
justice_data <- justice_data %>%
  dplyr::mutate(
    high_pitch_diff = ifelse(pitch_diff > pitch_mean, 1, 0),
    high_pitch_diff = factor(high_pitch_diff, levels = c(0, 1)),
    court_period = build_court_period(term),
    court_period = factor(court_period, levels = c("Burger", "Rehnquist", "Roberts"))
  )

table(justice_data$high_pitch_diff, useNA = "ifany")
table(justice_data$court_period, useNA = "ifany")

chief_data <- justice_data %>%
  dplyr::filter(justiceName %in% c("WEBurger", "WHRehnquist", "JGRoberts")) %>%
  dplyr::select(justiceName, petitioner_vote, sgpetac) %>%
  tidyr::drop_na()

fig1_summary <- chief_data %>%
  dplyr::group_by(justiceName, sgpetac) %>%
  dplyr::summarise(prop_petitioner_vote = mean(petitioner_vote), .groups = "drop") %>%
  dplyr::mutate(
    sgpetac_label = ifelse(sgpetac == 1, "Amicus", "No Amicus"),
    sgpetac_label = factor(sgpetac_label, levels = c("No Amicus", "Amicus"))
  )

p1 <- ggplot(fig1_summary, aes(x = sgpetac_label, y = prop_petitioner_vote, fill = sgpetac_label)) +
  geom_col() +
  facet_wrap(~ justiceName) +
  scale_fill_manual(values = c("No Amicus" = "blue", "Amicus" = "red")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Solicitor General Amicus (sgpetac)",
    y = "Proportion Voting for Petitioner",
    fill = ""
  ) +
  theme_minimal()

ggsave("HW3 Fig1.png", plot = p1, width = 6, height = 4, dpi = 300)

fig2_data <- justice_data %>%
  dplyr::filter(justiceName %in% c("WEBurger", "WHRehnquist", "JGRoberts")) %>%
  dplyr::select(justiceName, petitioner_vote, pitch_diff, term) %>%
  tidyr::drop_na()

fig2_mean <- mean(fig2_data$pitch_diff, na.rm = TRUE)
fig2_data <- fig2_data %>%
  dplyr::mutate(
    high_pitch_diff = ifelse(
      pitch_diff > fig2_mean,
      "Above Avg. Pitch Differential",
      "Below Avg. Pitch Differential"
    ),
    high_pitch_diff = factor(
      high_pitch_diff,
      levels = c("Below Avg. Pitch Differential", "Above Avg. Pitch Differential")
    ),
    court_period = build_court_period(term),
    court_period = factor(court_period, levels = c("Burger", "Rehnquist", "Roberts"))
  )

fig2_summary <- fig2_data %>%
  dplyr::group_by(court_period, high_pitch_diff) %>%
  dplyr::summarise(prop_petitioner_vote = mean(petitioner_vote), .groups = "drop")

p2 <- ggplot(fig2_summary, aes(x = high_pitch_diff, y = prop_petitioner_vote, fill = high_pitch_diff)) +
  geom_col() +
  facet_wrap(~ court_period) +
  scale_fill_manual(values = c(
    "Below Avg. Pitch Differential" = "blue",
    "Above Avg. Pitch Differential" = "red"
  )) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Pitch Differential Group",
    y = "Proportion Voting for Petitioner",
    fill = ""
  ) +
  theme_minimal()

ggsave("HW3 Fig2.png", plot = p2, width = 6, height = 4, dpi = 300)

# ---------------------------
# 4) Section 3.3 Regression Analyses
# ---------------------------

analysis_data <- justice_data %>%
  dplyr::mutate(
    pr_petitioner_pos = dplyr::if_else(
      petitioner_wc > 0 & respondent_wc > 0,
      (petitioner_harvard_pos / petitioner_wc) - (respondent_harvard_pos / respondent_wc),
      NA_real_
    ),
    court_period = factor(build_court_period(term), levels = c("Burger", "Rehnquist", "Roberts"))
  )

m3_1 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)
m3_2 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + factor(justiceName), data = analysis_data)
m3_3 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + factor(justiceName) + factor(term), data = analysis_data)

stargazer(m3_1, m3_2, m3_3, type = "text", out = "HW3 Table1.txt")

m3_period_base <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)
m3_period_int <- lm(petitioner_vote ~ pitch_diff * court_period + pr_petitioner_pos, data = analysis_data)

m3_period_frame <- model.frame(m3_period_int)
grid3 <- expand.grid(
  pitch_diff = seq(min(m3_period_frame$pitch_diff), max(m3_period_frame$pitch_diff), length.out = 120),
  court_period = levels(m3_period_frame$court_period),
  pr_petitioner_pos = mean(m3_period_frame$pr_petitioner_pos)
)
grid3$pred <- predict(m3_period_int, newdata = grid3)

p3 <- ggplot(grid3, aes(x = pitch_diff, y = pred, color = court_period)) +
  geom_line(linewidth = 0.9) +
  labs(
    x = "Pitch Differential",
    y = "Predicted Pr(Vote for Petitioner)",
    color = "Court Period"
  ) +
  theme_minimal()

ggsave("HW3 Fig3.png", plot = p3, width = 6, height = 4, dpi = 300)

m_prog1 <- lm(petitioner_vote ~ pitch_diff, data = analysis_data)
m_prog2 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)
m_prog3 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac, data = analysis_data)
m_prog4 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period, data = analysis_data)
m_prog5 <- lm(petitioner_vote ~ pitch_diff * court_period + pr_petitioner_pos + sgpetac, data = analysis_data)
m_prog6 <- lm(petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period, data = analysis_data)

stargazer(
  m_prog1, m_prog2, m_prog3, m_prog4, m_prog5, m_prog6,
  type = "text",
  out = "HW3 Table2.txt"
)

prog5_frame <- model.frame(m_prog5)
grid4 <- expand.grid(
  pitch_diff = seq(min(prog5_frame$pitch_diff), max(prog5_frame$pitch_diff), length.out = 120),
  court_period = levels(prog5_frame$court_period),
  pr_petitioner_pos = mean(prog5_frame$pr_petitioner_pos),
  sgpetac = mean(prog5_frame$sgpetac)
)
grid4$pred <- predict(m_prog5, newdata = grid4)

p4 <- ggplot(grid4, aes(x = pitch_diff, y = pred, color = court_period)) +
  geom_line(linewidth = 0.9) +
  labs(
    x = "Pitch Differential",
    y = "Predicted Pr(Vote for Petitioner)",
    color = "Court Period"
  ) +
  theme_minimal()

ggsave("HW3 Fig4.png", plot = p4, width = 6, height = 4, dpi = 300)

prog6_frame <- model.frame(m_prog6)
ref_period <- first_mode(prog6_frame$court_period)
pr_levels <- c(-2, -1, 0, 1, 2)

grid5 <- expand.grid(
  pitch_diff = seq(min(prog6_frame$pitch_diff), max(prog6_frame$pitch_diff), length.out = 120),
  pr_petitioner_pos = pr_levels,
  sgpetac = mean(prog6_frame$sgpetac),
  court_period = ref_period
)
grid5$pred <- predict(m_prog6, newdata = grid5)
grid5$pr_petitioner_pos <- factor(grid5$pr_petitioner_pos, levels = pr_levels)

p5 <- ggplot(grid5, aes(x = pitch_diff, y = pred, color = pr_petitioner_pos)) +
  geom_line(linewidth = 0.9) +
  labs(
    x = "Pitch Differential",
    y = "Predicted Pr(Vote for Petitioner)",
    color = "pr_petitioner_pos",
    subtitle = paste("Court period held at:", ref_period)
  ) +
  theme_minimal()

ggsave("HW3 Fig5.png", plot = p5, width = 6, height = 4, dpi = 300)

# ---------------------------
# 5) Section 3.4 Outlier Analysis and Validity
# ---------------------------

final_model <- m_prog6
stud_resid <- rstudent(final_model)
leverage <- hatvalues(final_model)
cooks_d <- cooks.distance(final_model)
dffits_val <- dffits(final_model)

outlier_df <- data.frame(
  obs_id = seq_along(stud_resid),
  studentized_resid = stud_resid,
  leverage = leverage,
  cooks_d = cooks_d,
  dffits = dffits_val
)

n_model <- nrow(model.frame(final_model))
k_model <- length(coef(final_model)) - 1
thr_resid <- 2
thr_lev <- (2 * k_model + 2) / n_model
thr_cook <- 4 / n_model
thr_dffits <- 2 * sqrt(k_model / n_model)

outlier_df <- outlier_df %>%
  dplyr::mutate(
    is_outlier = abs(studentized_resid) > thr_resid |
      leverage > thr_lev |
      cooks_d > thr_cook |
      abs(dffits) > thr_dffits,
    is_egregious = abs(studentized_resid) > thr_resid &
      leverage > thr_lev &
      cooks_d > thr_cook &
      abs(dffits) > thr_dffits,
    abs_dffits = abs(dffits)
  )

p6 <- ggplot(outlier_df, aes(x = abs_dffits, y = leverage, color = is_outlier)) +
  geom_point(alpha = 0.75) +
  geom_hline(yintercept = thr_lev, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = thr_dffits, linetype = "dashed", color = "red") +
  geom_text(
    data = subset(outlier_df, is_egregious),
    aes(label = obs_id),
    vjust = -0.4,
    size = 2.8,
    check_overlap = TRUE
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  labs(
    x = "|DFFITS|",
    y = "Leverage",
    color = "Outlier"
  ) +
  theme_minimal()

ggsave("HW3 Fig6.png", plot = p6, width = 6, height = 4, dpi = 300)

model_rows <- as.integer(rownames(model.frame(final_model)))
analysis_data_used <- analysis_data[model_rows, ]
clean_data <- analysis_data_used[!outlier_df$is_outlier, ]

model_full <- lm(
  petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period,
  data = analysis_data_used
)
model_clean <- lm(
  petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period,
  data = clean_data
)

stargazer(model_full, model_clean, type = "text", out = "HW3 Table3.txt")

outlier_summary <- c(
  "Outlier Summary (Section 3.4)",
  paste("Model observations:", n_model),
  paste("k (predictors):", k_model),
  paste("Threshold |studentized residual| >", thr_resid),
  paste("Threshold leverage >", round(thr_lev, 6)),
  paste("Threshold Cook's D >", round(thr_cook, 6)),
  paste("Threshold |DFFITS| >", round(thr_dffits, 6)),
  paste("Flagged outliers (any threshold):", sum(outlier_df$is_outlier)),
  paste("Flagged egregious outliers (all thresholds):", sum(outlier_df$is_egregious))
)
writeLines(outlier_summary, "HW3_OutlierSummary.txt")

# ---------------------------
# 6) Draft Answers for Q5-Q17
# ---------------------------

fmt_num <- function(x, digits = 3) {
  formatC(x, format = "f", digits = digits)
}

# Q5
q5_text <- paste0(
  "petitioner_vote is binary and fairly balanced but slightly petitioner-leaning: mean = ",
  fmt_num(mean(justice_data$petitioner_vote, na.rm = TRUE), 3),
  ", median = ", fmt_num(median(justice_data$petitioner_vote, na.rm = TRUE), 0), ". ",
  "pitch_diff is centered near zero (mean = ", fmt_num(mean(justice_data$pitch_diff, na.rm = TRUE), 3),
  ", median = ", fmt_num(median(justice_data$pitch_diff, na.rm = TRUE), 3),
  ") but has an extreme lower-tail outlier (min = ", fmt_num(min(justice_data$pitch_diff, na.rm = TRUE), 3),
  ", max = ", fmt_num(max(justice_data$pitch_diff, na.rm = TRUE), 3), "). ",
  "petitioner_harvard_pos is right-skewed (mean = ",
  fmt_num(mean(justice_data$petitioner_harvard_pos, na.rm = TRUE), 3),
  ", median = ", fmt_num(median(justice_data$petitioner_harvard_pos, na.rm = TRUE), 0),
  ", max = ", fmt_num(max(justice_data$petitioner_harvard_pos, na.rm = TRUE), 0), "). ",
  "There are ", sum(is.na(justice_data$petitioner_harvard_pos)),
  " missing values in petitioner_harvard_pos and related text variables, so complete-case models drop observations."
)

# Q6/Q7
q6_text <- paste(
  "pitch_diff is measured as petitioner_pitch minus respondent_pitch and captures whether a justice spoke at higher or lower pitch to one side relative to the other.",
  "Supplemental documentation indicates the pitch measure is standardized before differencing, which helps comparability across speakers (e.g., men vs. women with different baseline pitch ranges) and reduces sensitivity to absolute-frequency measurement noise.",
  "petitioner_harvard_pos is constructed by counting words directed to the petitioner that match a predefined Harvard-IV positive-word dictionary.",
  "A challenge is that dictionary methods ignore context: sarcasm, negation, legal jargon, and polysemy can cause words to be counted as 'positive' even when tone is neutral or negative."
)

q7_text <- paste(
  "Dictionary-based coding can introduce false positives (words marked positive in neutral/legal context), false negatives (missed positive phrasing not in the dictionary), and context errors (negation, irony, sarcasm).",
  "These measurement errors generally add noise and can attenuate coefficients toward zero, but if misclassification is systematic by justice, issue area, or period, it can also bias effect estimates and distort substantive interpretation."
)

# Q8
fig1_no <- fig1_summary %>%
  dplyr::filter(sgpetac_label == "No Amicus") %>%
  dplyr::select(justiceName, prop_no = prop_petitioner_vote)
fig1_yes <- fig1_summary %>%
  dplyr::filter(sgpetac_label == "Amicus") %>%
  dplyr::select(justiceName, prop_yes = prop_petitioner_vote)
fig1_change <- dplyr::left_join(fig1_no, fig1_yes, by = "justiceName") %>%
  dplyr::mutate(delta = prop_yes - prop_no) %>%
  dplyr::arrange(justiceName)

q8_detail <- paste(
  paste0(
    fig1_change$justiceName, ": ",
    fmt_num(fig1_change$prop_no, 3), " (No Amicus) -> ",
    fmt_num(fig1_change$prop_yes, 3), " (Amicus), delta = ",
    fmt_num(fig1_change$delta, 3)
  ),
  collapse = "; "
)

q8_text <- paste(
  "Across all three chief justices, petitioner-win rates are higher when the Solicitor General supports the petitioner.",
  q8_detail,
  "A plausible interpretation is that SG support signals legal merit and institutional credibility, but the pattern may also reflect case selection and omitted confounders."
)

# Q9
fig2_below <- fig2_summary %>%
  dplyr::filter(high_pitch_diff == "Below Avg. Pitch Differential") %>%
  dplyr::select(court_period, prop_below = prop_petitioner_vote)
fig2_above <- fig2_summary %>%
  dplyr::filter(high_pitch_diff == "Above Avg. Pitch Differential") %>%
  dplyr::select(court_period, prop_above = prop_petitioner_vote)
fig2_change <- dplyr::left_join(fig2_below, fig2_above, by = "court_period") %>%
  dplyr::mutate(delta_above_minus_below = prop_above - prop_below) %>%
  dplyr::arrange(court_period)

q9_detail <- paste(
  paste0(
    fig2_change$court_period, ": Above = ", fmt_num(fig2_change$prop_above, 3),
    ", Below = ", fmt_num(fig2_change$prop_below, 3),
    ", delta(Above-Below) = ", fmt_num(fig2_change$delta_above_minus_below, 3)
  ),
  collapse = "; "
)

q9_text <- paste(
  "Higher pitch-difference observations are generally associated with lower petitioner-win rates in Rehnquist and Roberts periods, while the Burger period shows little difference.",
  q9_detail,
  "This suggests the pitch-vote relationship is not constant over time and appears stronger in later court periods."
)

# Q10/Q11/Q12/Q13/Q14
m3_1_coef <- coef(summary(m3_1))
m3_2_coef <- coef(summary(m3_2))
m3_3_coef <- coef(summary(m3_3))

pitch_b <- m3_1_coef["pitch_diff", "Estimate"]
pitch_p <- m3_1_coef["pitch_diff", "Pr(>|t|)"]
pr_b <- m3_1_coef["pr_petitioner_pos", "Estimate"]
pr_p <- m3_1_coef["pr_petitioner_pos", "Pr(>|t|)"]

adj_m3_1 <- summary(m3_1)$adj.r.squared
adj_m3_2 <- summary(m3_2)$adj.r.squared
adj_m3_3 <- summary(m3_3)$adj.r.squared

justice_sig_n <- sum(
  grepl("^factor\\(justiceName\\)", rownames(m3_2_coef)) &
    m3_2_coef[, "Pr(>|t|)"] < 0.05
)
term_sig_n <- sum(
  grepl("^factor\\(term\\)", rownames(m3_3_coef)) &
    m3_3_coef[, "Pr(>|t|)"] < 0.05
)

q10_text <- paste0(
  "In the baseline model (m3_1), pitch_diff is negative and statistically significant (b = ",
  fmt_num(pitch_b, 3), ", p = ", signif(pitch_p, 3),
  "), implying higher petitioner-vs-respondent pitch is associated with a lower probability of a petitioner vote. ",
  "pr_petitioner_pos is not statistically significant (b = ", fmt_num(pr_b, 3),
  ", p = ", signif(pr_p, 3), "), so lexical positivity differences do not show a clear linear association in this specification."
)

q11_text <- paste0(
  "Controlling for justice fixed effects is important because justices have persistent baseline voting tendencies that can confound speech-effect estimates. ",
  "After adding justice controls, adjusted R2 rises from ", fmt_num(adj_m3_1, 3),
  " to ", fmt_num(adj_m3_2, 3), ", and ", justice_sig_n,
  " justice indicators are significant at p < 0.05. ",
  "The pitch_diff estimate remains negative and similar in magnitude, indicating the relationship is not driven only by cross-justice composition."
)

q12_text <- paste0(
  "Adding term indicators further increases adjusted R2 from ", fmt_num(adj_m3_2, 3),
  " to ", fmt_num(adj_m3_3, 3), ", with ", term_sig_n,
  " term indicators significant at p < 0.05. ",
  "This indicates nontrivial time-period variation in voting behavior and better overall fit once period-specific shocks are absorbed."
)

coef_period <- coef(m3_period_int)
slope_burger <- unname(coef_period["pitch_diff"])
slope_rehnquist <- slope_burger + unname(coef_period["pitch_diff:court_periodRehnquist"])
slope_roberts <- slope_burger + unname(coef_period["pitch_diff:court_periodRoberts"])

q13_text <- paste0(
  "In the pitch_diff * court_period interaction model, the negative slope is strongest in the Rehnquist period and also negative in the Roberts period: ",
  "Burger slope = ", fmt_num(slope_burger, 3),
  ", Rehnquist slope = ", fmt_num(slope_rehnquist, 3),
  ", Roberts slope = ", fmt_num(slope_roberts, 3), ". ",
  "This supports a period-varying relationship where vocal-pitch effects are modest in Burger-era data and more negative afterward."
)

adj_prog <- sapply(
  list(m_prog1, m_prog2, m_prog3, m_prog4, m_prog5, m_prog6),
  function(m) summary(m)$adj.r.squared
)
q14_text <- paste0(
  "Progressive models show small fit gains from Model 1 to 2, a clearer jump when sgpetac is added (Model 3), and minor incremental changes from additional period and interaction terms. ",
  "Adjusted R2 values are: M1=", fmt_num(adj_prog[1], 3),
  ", M2=", fmt_num(adj_prog[2], 3),
  ", M3=", fmt_num(adj_prog[3], 3),
  ", M4=", fmt_num(adj_prog[4], 3),
  ", M5=", fmt_num(adj_prog[5], 3),
  ", M6=", fmt_num(adj_prog[6], 3), ". ",
  "Substantively, SG amicus support is the strongest added predictor in this set, while interaction terms modestly refine but do not radically change model fit."
)

# Q16/Q17
outlier_df <- outlier_df %>%
  dplyr::mutate(
    n_flags = as.integer(abs(studentized_resid) > thr_resid) +
      as.integer(leverage > thr_lev) +
      as.integer(cooks_d > thr_cook) +
      as.integer(abs(dffits) > thr_dffits)
  )

outlier_cases <- cbind(
  outlier_df,
  analysis_data_used[, c("docketId", "docket", "justiceName", "term")]
)

top_outliers <- outlier_cases %>%
  dplyr::filter(is_outlier) %>%
  dplyr::arrange(desc(n_flags), desc(abs_dffits)) %>%
  dplyr::slice(1:5)

q16_top <- paste(
  paste0(
    "obs ", top_outliers$obs_id, " (", top_outliers$docketId, ", ",
    top_outliers$justiceName, ", term ", top_outliers$term,
    ", flags=", top_outliers$n_flags, ")"
  ),
  collapse = "; "
)

q16_text <- paste0(
  "Using assignment thresholds, ", sum(outlier_df$is_outlier),
  " observations are flagged by at least one diagnostic and ",
  sum(outlier_df$is_egregious), " are egregious (all four criteria). ",
  "Top flagged observations include: ", q16_top, ". ",
  "Most high-impact cases are flagged by leverage, Cook's D, and DFFITS simultaneously, while none exceed the |studentized residual| > 2 rule."
)

coef_full <- coef(summary(model_full))
coef_clean <- coef(summary(model_clean))
common_terms <- intersect(rownames(coef_full), rownames(coef_clean))
comp <- data.frame(
  term = common_terms,
  b_full = coef_full[common_terms, "Estimate"],
  se_full = coef_full[common_terms, "Std. Error"],
  b_clean = coef_clean[common_terms, "Estimate"],
  se_clean = coef_clean[common_terms, "Std. Error"]
)

get_comp <- function(term_name) {
  row <- comp[comp$term == term_name, , drop = FALSE]
  paste0(
    term_name, ": b ", fmt_num(row$b_full, 3), " -> ", fmt_num(row$b_clean, 3),
    "; SE ", fmt_num(row$se_full, 3), " -> ", fmt_num(row$se_clean, 3)
  )
}

q17_text <- paste(
  "Removing flagged outliers changes some coefficients but preserves the core qualitative finding that pitch_diff remains negative and SG amicus support remains positive/significant.",
  get_comp("pitch_diff"),
  get_comp("sgpetac"),
  get_comp("pitch_diff:pr_petitioner_pos"),
  "The clean-sample model has similar directional conclusions but somewhat different magnitudes and uncertainty, so results look broadly robust with moderate sensitivity for interaction terms."
)

answers_q5_q17 <- c(
  "Section 3.2-3.4 Draft Answers (Q5-Q17)",
  "",
  paste0("Q5: ", q5_text),
  "",
  paste0("Q6: ", q6_text),
  "",
  paste0("Q7: ", q7_text),
  "",
  paste0("Q8: ", q8_text),
  "",
  paste0("Q9: ", q9_text),
  "",
  paste0("Q10: ", q10_text),
  "",
  paste0("Q11: ", q11_text),
  "",
  paste0("Q12: ", q12_text),
  "",
  paste0("Q13: ", q13_text),
  "",
  paste0("Q14: ", q14_text),
  "",
  "Q15: IGNORE/DO NOT ANSWER.",
  "",
  paste0("Q16: ", q16_text),
  "",
  paste0("Q17: ", q17_text)
)

writeLines(answers_q5_q17, "HW3_DraftAnswers_Q5_Q17.txt")
