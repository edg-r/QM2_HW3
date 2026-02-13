# GPCO 454 - Quantitative Methods II - Winter 2025
# Homework 3 - Solutions

# ---------------------------
# Section 3.1 - Preliminaries
# ---------------------------

# 1. Set your working directory and load the necessary packages in your R script.

#setwd("~/Desktop/QM2 R Files/HW3") # <- this should be changed to your directory and commented out!
# install.packages("ggeffects")

# 2. Load the dataset into R

# 3. Explore the dataset
# Display the structure of the dataset

# 4. Summarize the key variables

# 5. Count unique observations in the 'docket' variable

# 6. Check for missing values


# ---------------------------
# Section 3.2 - Getting to Know the Data and Descriptive Statistics
# ---------------------------

# 1. Examine Key Variables: Generate summary statistics

# 2. Create New Variables

# Create the binary variable high_pitch_diff

# Create the categorical variable court_period based on chief justice tenure

# Convert court_period to a factor for categorical analysis

# Verify the new variables

# 3. Visualize Amicus Support and Voting Patterns

# Filter for the three Chief Justices

# Calculate proportion of votes in favor of petitioner

# Create bar plot

# Save the plot


# 4. Visualize Pitch Differential by Court Period

# Filter for relevant variables and remove missing values

# Create high_pitch_diff variable

# Create court_period variable

# Calculate proportion of votes by pitch differential and court period

# Create bar plot

# Save the plot


# ---------------------------
# Section 3.3 - Regression Analyses
# ---------------------------

# 1. Create a New Variable: Proportion of Positive Words

# 2. Estimate First Regression Model (Baseline Model)

# 3. Addressing Justice-Specific Effects

# Convert justiceName to a factor for inclusion in the regression

# Run the regression with justice fixed effects

# 4. Adding Term-Specific Indicators

# Create a regression table with all models

# 5. Court Period Interaction with Pitch Differential

# Create court period variable based on chief justice tenure

# Run models for overall and court-period interactions

# Visualize interaction effects

# Save the plot

# 6. Progressive Model Building

# Run multiple regressions with increasing complexity

# Create regression table

# 7. Interaction Effect Visualization
# Model: Pitch Differential x Court Period

# Plot the interaction effect

# Save the plot

# Interaction: Pitch Differential x Positive Words

# Plot the interaction effect

# Save the plot

# ---------------------------
# Section 4 - Outlier Analysis and Threats to Validity
# ---------------------------

# 1. Outlier Diagnostics on Final Regression Model

# Re-run the final regression model from Section 3

# Calculate outlier diagnostics

# Create a dataframe to store outlier diagnostics

# Define Critical Thresholds for Outliers

# Identify Outliers Based on Thresholds

# Identify Egregious Outliers (if all thresholds are exceeded)

# 2. Visualize Potential Outliers

# Save the plot

# 3. Regression Models with and without Outliers

# Filter out identified outliers

# Model 1: Full Dataset

# Model 2: Without Outliers

# Regression Table Comparison
