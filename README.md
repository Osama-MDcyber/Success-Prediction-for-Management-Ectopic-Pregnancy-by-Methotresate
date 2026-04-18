# Success of Medical Management of Tubal Ectopic Pregnancy with Methotrexate

R analysis and report for a cross-sectional study evaluating clinical and biochemical predictors
of methotrexate treatment success in 121 patients with tubal ectopic pregnancy.

## Contents

- **Data Preprocessing:** Cleaning and standardization of variable names and types, 
  normality assessment for continuous variables using the Shapiro-Wilk test combined 
  with visual diagnostics (histogram, density plot, Q-Q plot).

- **Baseline Characteristics:** Summary of patient demographics and clinical variables 
  including BMI, gestational age, b-HCG, free fluid, and ectopic mass diameter, 
  reported as median (IQR) or n (%).

- **Univariable Logistic Regression:** Screening of each candidate predictor against 
  treatment outcome, reported as unadjusted odds ratios (OR) with 95% confidence intervals.

- **Multivariable Logistic Regression:** Two competing models fitted — Model 1 (BMI + 
  gestational age + b-HCG + free fluid) and Model 2 (BMI + b-HCG + free fluid) — with 
  nested model comparison via likelihood ratio test (χ² analysis of deviance).

- **Model Diagnostics:** Calibration assessed with the Hosmer-Lemeshow goodness-of-fit 
  test (g = 10); model fit quantified using McFadden's, Nagelkerke's, and Cox-Snell 
  pseudo-R² statistics; multicollinearity evaluated via Pearson correlation matrix and 
  variance inflation factor (VIF).

- **Discrimination:** ROC curves for both models with AUC and 95% CI, formally compared 
  using DeLong's test for correlated ROC curves.

- **Clinical Nomogram:** Individual patient probability estimation tool constructed from 
  the final model using the `rms` package, mapping BMI, b-HCG, and free fluid status 
  to predicted probability of treatment success.

- **Publication-Ready Outputs:** Merged regression table (unadjusted + Model 1 + Model 2) 
  with column footnotes, overlaid ROC curve plot with AUC legend, and nomogram figure 
  with customized probability axis scaling.
