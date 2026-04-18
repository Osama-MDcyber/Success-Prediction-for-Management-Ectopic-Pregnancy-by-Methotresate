
source(file = "Data Cleaning and Upload(Methotrexte EP).R")


# ── 5. NORMALITY TESTING FUNCTION ────────────────────────────
# Custom function combining visual (histogram, density, Q-Q plot)
# and formal (Shapiro-Wilk) normality assessment.
# Non-normal variables will be summarized as median (IQR)
# rather than mean (SD) in Table 1.
Normality_Testing <- function(data, x) {
  p1 <- data %>%
    ggplot(aes(x = {{ x }})) +
    geom_histogram() +
    ggtitle("Histogram")
  
  p2 <- data %>%
    ggplot(aes(x = {{ x }})) +
    geom_density() +
    ggtitle("Density Plot")
  
  p3 <- data %>%
    ggplot(aes(sample = {{ x }})) +
    geom_qq() +
    geom_qq_line() +
    ggtitle("Q-Q Plot")
  
  # Stack the three diagnostic plots vertically using patchwork
  print(p1 / p2 / p3)
  
  # Return Shapiro-Wilk test result (p < 0.05 → non-normal)
  data %>% shapiro_test({{ x }})
}


# ── 6. APPLY NORMALITY TESTING ───────────────────────────────

# Age: normal distribution (Shapiro-Wilk p > 0.05)
ectopic_pregnancy_data %>% Normality_Testing(Age)

# BMI: non-normal → report as median (IQR)
ectopic_pregnancy_data %>% Normality_Testing(BMI)

# Gravity: non-normal → report as median (IQR)
ectopic_pregnancy_data %>% Normality_Testing(Gravity)

# Parity: non-normal → report as median (IQR)
ectopic_pregnancy_data %>% Normality_Testing(Parity)

# Abortion: non-normal → report as median (IQR)
ectopic_pregnancy_data %>% Normality_Testing(Abortion)

# Gestational Age: temporarily rename to avoid backtick issues
# in the tidy evaluation context of the function
ectopic_pregnancy_data %>%
  rename("gest_age_weeks" = `Gestational Age (Weeks)`) %>%
  Normality_Testing(gest_age_weeks)

# b-HCG: non-normal → report as median (IQR)
ectopic_pregnancy_data %>% Normality_Testing(BHCG)

# Ectopic mass diameter: temporarily rename for same reason above
ectopic_pregnancy_data %>%
  rename("ep_mass_diameter" = `EP Mass Diameter(cm)`) %>%
  Normality_Testing(ep_mass_diameter)


# ── 7. TABLE 1 — BASELINE CHARACTERISTICS ───────────────────
# gtsummary auto-selects median (IQR) for non-normal continuous
# variables and n (%) for factors.
# Gravity, Parity, Abortion are integer-coded but clinically
# continuous → force "continuous" type to avoid being treated as
# categorical.
ectopic_pregnancy_data %>%
  tbl_summary(
    include = -ID,
    type = list(
      Gravity  ~ "continuous",
      Parity   ~ "continuous",
      Abortion ~ "continuous"
    ),
    label = list(
      BHCG                   ~ "b-HCG (mIU/mL)",
      `EP Mass Diameter(cm)` ~ "Ectopic mass diameter (cm)"
    )
  ) %>%
  modify_footnote_header(
    "Abbreviations: BMI = body mass index; b-HCG = beta human chorionic gonadotropin.",
    columns = "label"
  ) %>%
  bold_labels() %>%
  as_flex_table()


# ── 8. UNIVARIABLE LOGISTIC REGRESSION ───────────────────────
# Screen each predictor individually against the binary Outcome.
# Exponentiated coefficients = crude odds ratios (OR) with 95% CI.
# hide_n = TRUE removes the redundant N column from the table.
univariable_reg_table <- ectopic_pregnancy_data %>%
  select(
    Age, BMI, Gravity, Parity, Abortion,
    `Gestational Age (Weeks)`, BHCG,
    `Free Fluid`, `EP Mass Diameter(cm)`, Outcome
  ) %>%
  tbl_uvregression(
    label = list(
      BHCG                   ~ "b-HCG (mIU/mL)",
      `EP Mass Diameter(cm)` ~ "Ectopic mass diameter (cm)"
    ),
    method       = "glm",
    y            = Outcome,
    method.args  = list(family = "binomial"),
    exponentiate = TRUE,
    hide_n       = TRUE
  ) %>%
  bold_labels()


# ── 9. MULTIVARIABLE LOGISTIC REGRESSION MODELS ──────────────
# Model 1 (full): includes gestational age as a candidate predictor
# Model 2 (reduced): drops gestational age after LRT comparison
# Both models are nested; Model 2 ⊂ Model 1
multivariable_model1 <- glm(
  Outcome ~ BMI + `Gestational Age (Weeks)` + BHCG + `Free Fluid`,
  family = "binomial",
  data   = ectopic_pregnancy_data
)

multivariable_model2 <- glm(
  Outcome ~ BMI + BHCG + `Free Fluid`,
  family = "binomial",
  data   = ectopic_pregnancy_data
)

# Format regression tables with exponentiated coefficients (ORs)
# bold_p() highlights statistically significant predictors (p < 0.05)
table_multivaraible_1 <- tbl_regression(multivariable_model1, exponentiate = TRUE) %>%
  bold_p(t = 0.05) %>%
  bold_labels()

table_multivaraible_2 <- tbl_regression(multivariable_model2, exponentiate = TRUE) %>%
  bold_p(t = 0.05) %>%
  bold_labels()


# ── 10. NESTED MODEL COMPARISON (LRT) ────────────────────────
# Likelihood ratio test (Chi-squared) compares Model 1 vs. Model 2.
# Non-significant result (p > 0.05) confirms gestational age does
# not significantly improve model fit → Model 2 is preferred
# on grounds of parsimony.
anova(multivariable_model1, multivariable_model2, test = "Chisq")


# ── 11. MODEL CALIBRATION — HOSMER-LEMESHOW TEST ─────────────
# Assesses whether observed outcome frequencies match predicted
# probabilities across 10 equal-sized groups.
# Non-significant p-value (> 0.05) indicates good calibration.
hoslem.test(multivariable_model2$y, fitted(multivariable_model2))


# ── 12. MODEL FIT — PSEUDO-R² STATISTICS ─────────────────────
# McFadden > 0.4 indicates excellent explanatory power.
# Nagelkerke and Cox-Snell provide complementary fit measures.
PseudoR2(multivariable_model2, which = c("McFadden", "Nagelkerke", "CoxSnell"))


# ── 13. TABLE 2 — MERGED REGRESSION TABLE ────────────────────
# Combines unadjusted (univariable) and adjusted (both multivariable
# models) ORs side-by-side for a single publication-ready table.
tbl_merge(
  tbls        = list(univariable_reg_table, table_multivaraible_1, table_multivaraible_2),
  tab_spanner = c("**Unadjusted**", "**Model 1**", "**Model 2**")
) %>%
  modify_footnote_header(
    footnote = "OR = odds ratio; CI = confidence interval. Exponentiated logistic regression coefficients.",
    columns  = starts_with("estimate")
  ) %>%
  modify_footnote_spanning_header(
    footnote = "Model 1: BMI + gestational age + b-HCG + free fluid",
    columns  = "estimate_2"
  ) %>%
  modify_footnote_spanning_header(
    footnote = "Model 2: BMI + b-HCG + free fluid",
    columns  = "estimate_3"
  ) %>%
  modify_footnote_body(
    footnote = "Binary variable: Yes = free fluid present on ultrasound",
    columns  = "label",
    rows     = variable == "Free Fluid" & row_type == "label"
  ) %>%
  as_flex_table()


# ── 14. ROC CURVES & AUC COMPARISON ─────────────────────────
# Build ROC objects from observed outcomes and fitted probabilities.
# ci = TRUE computes 95% CI for AUC via DeLong's method.
roc1 <- roc(multivariable_model1$y, fitted(multivariable_model1), ci = TRUE)
roc2 <- roc(multivariable_model2$y, fitted(multivariable_model2), ci = TRUE)

# Plot both curves on the same axes for visual comparison.
# legacy.axes = TRUE plots 1-Specificity (FPR) on the x-axis,
# which is the conventional clinical orientation.
plot(roc1, col = "steelblue", lwd = 2, legacy.axes = TRUE,
     main = "ROC Curve Comparison")
# add = TRUE overlays roc2 on the existing plot rather than
# opening a new graphics device
plot(roc2, col = "tomato", lwd = 2, add = TRUE)

# Legend displays AUC with 95% CI for each model
legend("bottomright",
       legend = c(
         paste0("Model 1 (AUC = ", round(auc(roc1), 3),
                ", 95% CI: ", round(ci.auc(roc1)[1], 3),
                "–", round(ci.auc(roc1)[3], 3), ")"),
         paste0("Model 2 (AUC = ", round(auc(roc2), 3),
                ", 95% CI: ", round(ci.auc(roc2)[1], 3),
                "–", round(ci.auc(roc2)[3], 3), ")")
       ),
       col = c("steelblue", "tomato"), lwd = 2)

# DeLong's test formally compares the two AUCs.
# Accounts for the correlation between models tested on the
# same patients. Non-significant result → no meaningful
# difference in discrimination between Model 1 and Model 2.
roc.test(roc1, roc2, method = "delong")


# ── 15. MULTICOLLINEARITY DIAGNOSTICS ────────────────────────
# Pearson correlation matrix for numeric predictors.
# Values > 0.7 would suggest problematic collinearity.
numeric_predictors_correlation <- cor(
  ectopic_pregnancy_data[, c("BMI", "Gestational Age (Weeks)", "BHCG")]
)

# Visual correlation heatmap with coefficient values overlaid
corrplot(numeric_predictors_correlation,
         method       = "color",
         type         = "full",
         col          = COL1(sequential = "Blues", n = 200),
         addCoef.col  = "White",
         tl.col       = "Black")

# Variance Inflation Factor (VIF): values < 5 are acceptable;
# values > 10 indicate severe multicollinearity
vif(multivariable_model1)


# ── 16. NOMOGRAM — INDIVIDUALIZED PROBABILITY PREDICTION ─────
# rms::lrm() requires a datadist object so the package knows
# the range and distribution of each predictor for plotting.
# Free Fluid is renamed to remove the space, as rms does not
# handle backtick-quoted names reliably in nomogram().
cleaned_name_data <- ectopic_pregnancy_data %>%
  rename("Free_Fluid" = `Free Fluid`)

dd <- datadist(cleaned_name_data)
options(datadist = "dd")

# Refit Model 2 using lrm() instead of glm().
# x = TRUE and y = TRUE store the design matrix and response
# vector, which are required internally by nomogram().
lrm_model <- lrm(
  Outcome ~ BMI + BHCG + Free_Fluid,
  data = cleaned_name_data,
  x    = TRUE,
  y    = TRUE
)

# Build the nomogram.
# fun = plogis converts the linear predictor (log-odds) to
# predicted probability for the bottom axis.
# fun.at specifies exactly which probability values appear as
# tick marks — chosen to avoid label overlap near p = 0.5
# where the logistic curve is steepest.
nom <- nomogram(
  lrm_model,
  fun      = plogis,
  fun.at   = c(0.1, 0.3, 0.5, 0.7, 0.9),
  funlabel = "Predicted Probability"
)

# xfrac controls the fraction of horizontal space allocated to
# variable name labels on the left side of the plot
plot(nom, xfrac = 0.4)


