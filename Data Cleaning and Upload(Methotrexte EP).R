
# ============================================================
# Project: Success of Medical Management of Tubal Ectopic
#          Pregnancy with Methotrexate
# Author:  Osama Ahmed Ibrahim
# Purpose: Data cleaning, normality testing, descriptive
#          statistics, logistic regression modeling (univariable
#          & multivariable), model diagnostics, ROC/AUC
#          comparison, multicollinearity checks, and nomogram
# ============================================================


# ── 1. LOAD PACKAGES ─────────────────────────────────────────
# Data import and wrangling
library(readxl)
library(tidyverse)

# Tables and regression summaries
library(gtsummary)

# Plot composition
library(patchwork)

# Statistical tests (Shapiro-Wilk, etc.)
library(rstatix)

# VIF (variance inflation factor) for multicollinearity
library(car)

# Correlation matrix visualization
library(corrplot)

# Pseudo-R² statistics (McFadden, Nagelkerke, Cox-Snell)
library(DescTools)

# Hosmer-Lemeshow goodness-of-fit test
library(ResourceSelection)

# ROC curve construction and DeLong's AUC comparison test
library(pROC)

# Nomogram construction via lrm() (regression modeling strategies)
library(rms)


# ── 2. IMPORT DATA ───────────────────────────────────────────
ectopic_pregnancy_data <- read_excel(path = "Project 5 data.xlsx")


# ── 3. CLEAN COLUMN NAMES ────────────────────────────────────
# Standardize column name casing: title case for most variables,
# preserve BMI and ID as-is, force BHCG to uppercase
ectopic_pregnancy_data <- ectopic_pregnancy_data %>%
  rename_with(str_to_title, c(-BMI, -ID)) %>%
  rename_with(str_to_upper, Bhcg) %>%
  rename(
    "EP Mass Diameter(cm)" = `Ep Mass Diameter(Cm)`,
    "Number of Doses"      = `No. Of Doses`
  )


# ── 4. CONVERT CATEGORICAL VARIABLES TO FACTORS ──────────────
# Free Fluid, Outcome, and Number of Doses are categorical;
# str_to_title() ensures consistent "Yes/No/Success/Failure"
# capitalization before factoring
ectopic_pregnancy_data <- ectopic_pregnancy_data %>%
  mutate(across(
    c(`Free Fluid`, Outcome, `Number of Doses`),
    ~ factor(str_to_title(.x))
  ))

# Explicitly order Number of Doses levels for proper display
# in tables and plots (One < Two < Multiple)
ectopic_pregnancy_data <- ectopic_pregnancy_data %>%
  mutate(
    `Number of Doses` = factor(
      `Number of Doses`,
      levels = c("One", "Two", "Multiple")
    )
  )



