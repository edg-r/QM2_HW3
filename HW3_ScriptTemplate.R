# GPCO 454 - Quantitative Methods II - Winter 2025
# Homework 3 - Script Template (Pseudocode Draft)

# NOTE:
# This file is intentionally filled with comments + pseudocode so you can review logic.
# Replace pseudocode blocks with executable code once approved.

# ---------------------------
# Section 3.1 - Preliminaries
# ---------------------------

# 1) Set working directory and load packages
# setwd("/path/to/HW3")  # Keep commented before submission per instructions.
# install.packages(c("dplyr", "ggplot2", "ggeffects", "stargazer", "ggrepel"))
# library(dplyr)
# library(ggplot2)
# library(ggeffects)
# library(stargazer)
# library(ggrepel)

# 2) Load the dataset (tab-delimited with required encoding)
# justice_data <- read.table(
#   "justice_results.tab",
#   header = TRUE,
#   sep = "\t",
#   encoding = "ISO-8859-1"
# )

# 3) Explore dataset structure (for Q1/Q2 context)
# str(justice_data)            # rows, columns, and data types
# names(justice_data)          # variable names
# dim(justice_data)            # observation and variable counts

# 4) Summarize variables
# summary(justice_data)        # global summary statistics

# 5) Count unique Supreme Court cases using docket
# n_unique_dockets <- length(unique(justice_data$docket))
# n_unique_dockets

# 6) Check missing values by column and overall
# missing_by_var <- colSums(is.na(justice_data))
# total_missing <- sum(is.na(justice_data))
# missing_by_var[missing_by_var > 0]  # print only variables with missing values
# total_missing


# ---------------------------
# Section 3.2 - Getting to Know the Data and Descriptive Statistics
# ---------------------------

# 1) Examine key variables
# summary(justice_data[, c("petitioner_vote", "pitch_diff", "petitioner_harvard_pos")])

# 2) Create new variables
# 2a) Binary high_pitch_diff using mean of pitch_diff
# pitch_mean <- mean(justice_data$pitch_diff, na.rm = TRUE)
# justice_data$high_pitch_diff <- ifelse(justice_data$pitch_diff > pitch_mean, 1, 0)
# justice_data$high_pitch_diff <- factor(justice_data$high_pitch_diff, levels = c(0, 1))

# 2b) court_period by term (Burger 1969-1985, Rehnquist 1986-2004, Roberts 2005+)
# justice_data$court_period <- ifelse(
#   justice_data$term >= 1969 & justice_data$term <= 1985, "Burger",
#   ifelse(justice_data$term >= 1986 & justice_data$term <= 2004, "Rehnquist", "Roberts")
# )
# justice_data$court_period <- factor(
#   justice_data$court_period,
#   levels = c("Burger", "Rehnquist", "Roberts")
# )

# 2c) Verify categorical variables
# table(justice_data$high_pitch_diff, useNA = "ifany")
# table(justice_data$court_period, useNA = "ifany")

# 3) Visualize amicus support and voting patterns (HW3 Fig1.png)
# 3a) Keep only Chief Justices in the prompt
# chief_data <- justice_data %>%
#   dplyr::filter(justiceName %in% c("WEBurger", "WHRehnquist", "JGRoberts")) %>%
#   dplyr::select(justiceName, petitioner_vote, sgpetac) %>%
#   tidyr::drop_na()

# 3b) Compute proportion of petitioner votes by justice and amicus status
# fig1_summary <- chief_data %>%
#   dplyr::group_by(justiceName, sgpetac) %>%
#   dplyr::summarise(prop_petitioner_vote = mean(petitioner_vote), .groups = "drop")

# 3c) Recode sgpetac for readable legend labels
# fig1_summary$sgpetac_label <- ifelse(fig1_summary$sgpetac == 1, "Amicus", "No Amicus")

# 3d) Plot grouped/faceted bars
# p1 <- ggplot(fig1_summary, aes(x = sgpetac_label, y = prop_petitioner_vote, fill = sgpetac_label)) +
#   geom_col() +
#   facet_wrap(~ justiceName) +
#   scale_fill_manual(values = c("No Amicus" = "blue", "Amicus" = "red")) +
#   coord_cartesian(ylim = c(0, 1)) +
#   labs(x = "Solicitor General Amicus (sgpetac)", y = "Proportion Voting for Petitioner") +
#   theme_minimal()

# 3e) Save figure
# ggsave("HW3 Fig1.png", plot = p1, width = 6, height = 4, dpi = 300)

# 4) Visualize pitch differential by court period (HW3 Fig2.png)
# 4a) Filter to same Chief Justices and needed columns
# fig2_data <- justice_data %>%
#   dplyr::filter(justiceName %in% c("WEBurger", "WHRehnquist", "JGRoberts")) %>%
#   dplyr::select(justiceName, petitioner_vote, pitch_diff, term) %>%
#   tidyr::drop_na()

# 4b) Build labeled high_pitch_diff using >= mean (per prompt for this figure)
# fig2_mean <- mean(fig2_data$pitch_diff, na.rm = TRUE)
# fig2_data$high_pitch_diff <- ifelse(
#   fig2_data$pitch_diff >= fig2_mean,
#   "Above Avg. Pitch Differential",
#   "Below Avg. Pitch Differential"
# )
# fig2_data$high_pitch_diff <- factor(
#   fig2_data$high_pitch_diff,
#   levels = c("Below Avg. Pitch Differential", "Above Avg. Pitch Differential")
# )

# 4c) Build court_period
# fig2_data$court_period <- ifelse(
#   fig2_data$term >= 1969 & fig2_data$term <= 1985, "Burger",
#   ifelse(fig2_data$term >= 1986 & fig2_data$term <= 2004, "Rehnquist", "Roberts")
# )
# fig2_data$court_period <- factor(fig2_data$court_period, levels = c("Burger", "Rehnquist", "Roberts"))

# 4d) Compute proportions by period x pitch group
# fig2_summary <- fig2_data %>%
#   dplyr::group_by(court_period, high_pitch_diff) %>%
#   dplyr::summarise(prop_petitioner_vote = mean(petitioner_vote), .groups = "drop")

# 4e) Plot faceted bars
# p2 <- ggplot(fig2_summary, aes(x = high_pitch_diff, y = prop_petitioner_vote, fill = high_pitch_diff)) +
#   geom_col() +
#   facet_wrap(~ court_period) +
#   scale_fill_manual(values = c("Below Avg. Pitch Differential" = "blue", "Above Avg. Pitch Differential" = "red")) +
#   coord_cartesian(ylim = c(0, 1)) +
#   labs(x = "Pitch Differential Group", y = "Proportion Voting for Petitioner") +
#   theme_minimal()

# 4f) Save figure
# ggsave("HW3 Fig2.png", plot = p2, width = 6, height = 4, dpi = 300)


# ---------------------------
# Section 3.3 - Regression Analyses
# ---------------------------

# 1) Create pr_petitioner_pos:
# proportion positive words toward petitioner minus proportion toward respondent
# analysis_data <- justice_data
# analysis_data$pr_petitioner_pos <- with(
#   analysis_data,
#   (petitioner_harvard_pos / petitioner_wc) - (respondent_harvard_pos / respondent_wc)
# )
# Optional safety: replace division-by-zero outcomes with NA before modeling.

# 2) Baseline regression (m3_1)
# m3_1 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)

# 3) Add justice fixed effects (m3_2)
# analysis_data$justiceName <- factor(analysis_data$justiceName)
# m3_2 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + justiceName, data = analysis_data)

# 4) Add term fixed effects (m3_3)
# analysis_data$term <- factor(analysis_data$term)
# m3_3 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + justiceName + term, data = analysis_data)

# Save Table 1
# stargazer(m3_1, m3_2, m3_3, type = "text", out = "HW3 Table1.txt")

# 5) Court-period interaction with pitch_diff (save HW3 Fig3.png)
# 5a) Recreate court_period variable for modeling
# analysis_data$court_period <- ifelse(
#   as.numeric(as.character(analysis_data$term)) >= 1969 & as.numeric(as.character(analysis_data$term)) <= 1985, "Burger",
#   ifelse(as.numeric(as.character(analysis_data$term)) >= 1986 & as.numeric(as.character(analysis_data$term)) <= 2004, "Rehnquist", "Roberts")
# )
# analysis_data$court_period <- factor(analysis_data$court_period, levels = c("Burger", "Rehnquist", "Roberts"))

# 5b) Baseline and interaction models
# m3_period_base <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)
# m3_period_int  <- lm(petitioner_vote ~ pitch_diff * court_period + pr_petitioner_pos, data = analysis_data)

# 5c) Interaction plot (predicted petitioner vote by pitch_diff and court_period)
# p3_pred <- ggeffects::ggpredict(m3_period_int, terms = c("pitch_diff [all]", "court_period"))
# p3 <- plot(p3_pred) + labs(x = "Pitch Differential", y = "Predicted Pr(Vote for Petitioner)")
# ggsave("HW3 Fig3.png", plot = p3, width = 6, height = 4, dpi = 300)

# 6) Progressive model building (Models 1-6)
# m_prog1 <- lm(petitioner_vote ~ pitch_diff, data = analysis_data)
# m_prog2 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos, data = analysis_data)
# m_prog3 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac, data = analysis_data)
# m_prog4 <- lm(petitioner_vote ~ pitch_diff + pr_petitioner_pos + sgpetac + court_period, data = analysis_data)
# m_prog5 <- lm(petitioner_vote ~ pitch_diff * court_period + pr_petitioner_pos + sgpetac, data = analysis_data)
# m_prog6 <- lm(petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period, data = analysis_data)

# Save Table 2
# stargazer(m_prog1, m_prog2, m_prog3, m_prog4, m_prog5, m_prog6, type = "text", out = "HW3 Table2.txt")

# 7) Interaction visualizations (HW3 Fig4.png and HW3 Fig5.png)
# 7a) Pitch_diff x court_period from Model 5 (or the dedicated interaction model)
# p4_pred <- ggeffects::ggpredict(m_prog5, terms = c("pitch_diff [all]", "court_period"))
# p4 <- plot(p4_pred) + labs(x = "Pitch Differential", y = "Predicted Pr(Vote for Petitioner)")
# ggsave("HW3 Fig4.png", plot = p4, width = 6, height = 4, dpi = 300)

# 7b) Pitch_diff x pr_petitioner_pos from Model 6
# p5_pred <- ggeffects::ggpredict(m_prog6, terms = c("pitch_diff [all]", "pr_petitioner_pos [-2,-1,0,1,2]"))
# p5 <- plot(p5_pred) + labs(x = "Pitch Differential", y = "Predicted Pr(Vote for Petitioner)")
# ggsave("HW3 Fig5.png", plot = p5, width = 6, height = 4, dpi = 300)


# ---------------------------
# Section 4 - Outlier Analysis and Threats to Validity
# ---------------------------

# 1) Outlier diagnostics on final model (Model 6 from Section 3.3)
# final_model <- m_prog6

# 1a) Calculate diagnostics
# stud_resid <- rstudent(final_model)
# leverage <- hatvalues(final_model)
# cooks_d <- cooks.distance(final_model)
# dffits_val <- dffits(final_model)

# 1b) Build diagnostics dataframe
# outlier_df <- data.frame(
#   obs_id = seq_along(stud_resid),
#   studentized_resid = stud_resid,
#   leverage = leverage,
#   cooks_d = cooks_d,
#   dffits = dffits_val
# )

# 1c) Define thresholds using assignment formulas
# n <- nrow(model.frame(final_model))
# k <- length(coef(final_model)) - 1
# thr_resid <- 2
# thr_lev <- (2 * k + 2) / n
# thr_cook <- 4 / n
# thr_dffits <- 2 * sqrt(k / n)

# 1d) Flag outliers (any threshold exceeded)
# outlier_df$is_outlier <- with(
#   outlier_df,
#   abs(studentized_resid) > thr_resid |
#     leverage > thr_lev |
#     cooks_d > thr_cook |
#     abs(dffits) > thr_dffits
# )

# 1e) Flag egregious outliers (all thresholds exceeded)
# outlier_df$is_egregious <- with(
#   outlier_df,
#   abs(studentized_resid) > thr_resid &
#     leverage > thr_lev &
#     cooks_d > thr_cook &
#     abs(dffits) > thr_dffits
# )

# 2) Visualize potential outliers (HW3 Fig6.png)
# p6 <- ggplot(outlier_df, aes(x = abs(dffits), y = leverage, color = is_outlier)) +
#   geom_point(alpha = 0.7) +
#   geom_hline(yintercept = thr_lev, linetype = "dashed", color = "blue") +
#   geom_vline(xintercept = thr_dffits, linetype = "dashed", color = "red") +
#   scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
#   labs(x = "|DFFITS|", y = "Leverage", color = "Outlier") +
#   theme_minimal()

# Label egregious points by observation number
# p6 <- p6 + ggrepel::geom_text_repel(
#   data = subset(outlier_df, is_egregious),
#   aes(label = obs_id),
#   size = 3
# )

# ggsave("HW3 Fig6.png", plot = p6, width = 6, height = 4, dpi = 300)

# 3) Compare regressions with and without outliers
# 3a) Create clean dataset excluding flagged outliers
# clean_data <- analysis_data[!outlier_df$is_outlier, ]

# 3b) Re-estimate final specification on full and clean data
# model_full <- lm(petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period, data = analysis_data)
# model_clean <- lm(petitioner_vote ~ pitch_diff * pr_petitioner_pos + sgpetac + court_period, data = clean_data)

# 3c) Save comparison table
# stargazer(model_full, model_clean, type = "text", out = "HW3 Table3.txt")

# End of pseudocode template.
