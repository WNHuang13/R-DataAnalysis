library(ggmirt)
library(tidyverse)
library(mirt)
library(subscore)
library(lavaan)
library(ggplot2)
library(RColorBrewer)
library(reliable)

# Import dataset 
content_analysis_outcomes <- read_sav(" ")

# Select relevant items
pure <- content_analysis_outcomes[, c(5:11)]


############################################################
# IRT Models (1PL and 2PL) with 7 factors
############################################################

# Fit 1PL
fit1PL <- mirt(pure, 1, itemtype = "Rasch", verbose = FALSE)
M2(fit1PL)

# Fit 2PL
fit2PL <- mirt(pure, 1, itemtype = "2PL", verbose = FALSE)
summary(fit2PL)
M2(fit2PL)
itemfit(fit2PL)

# Extract parameters (2PL)
params2PL <- coef(fit2PL, IRTpars = TRUE, simplify = TRUE)
round(params2PL$items, 3) 

# Trace plots with labels
tracePlot(fit2PL, title = "", facet = FALSE, legend = TRUE) +
  scale_color_manual(
    values = c("#e41a1c", "#a65628", "#4daf4a", "#ff7f00", "#377eb8", "#984ea3", "#e7298a"),
    labels = c("Verification", "Justification", "Explication", "Source", "Example", "Metacognition", "Emotion")
  )


############################################################
# CFA Model (7 factors)
############################################################

model.cfa <- "
  feedback =~ Verification + Justifications + Explications + Sources + Example + Metacognition + Motivation
"
ESTModel.cfa <- cfa(model.cfa, data = pure, meanstructure = TRUE,
                    information = "observed", estimator = "DWLS")
summary(ESTModel.cfa, fit.measure = TRUE, standardized = TRUE, rsq = TRUE, modindices = TRUE)


############################################################
# Reduced Models (5 factors, removing Example & Emotion)
############################################################

# 1PL
fit_51 <- mirt(content_analysis_outcomes[, c(5,6,7,8,10)], 1, itemtype = "Rasch", verbose = FALSE)
M2(fit_51)

# 2PL
fit_5 <- mirt(content_analysis_outcomes[, c(5,6,7,8,10)], 1, itemtype = "2PL", verbose = FALSE, SE = TRUE)
summary_fit <- summary(fit_5)
M2(fit_5)
itemfit(fit_5)

# Extract 2PL parameters
params_5 <- coef(fit_5, IRTpars = TRUE, simplify = TRUE)
round(params_5$items, 3)

# Trace plots for 5 factors
tracePlot(fit_5, title = "", facet = FALSE, legend = TRUE) +
  scale_color_manual(
    values = c("#e41a1c", "#a65628", "#4daf4a", "#ff7f00", "#984ea3"),
    labels = c("Verification", "Justification", "Explication", "Source", "Metacognition")
  )


############################################################
# CFA Model (5 factors)
############################################################

model.cfa.n <- "
  feedback =~ Verification + Justifications + Explications + Sources + Metacognition
"
ESTModel.cfa.n <- cfa(model.cfa.n, data = pure, meanstructure = TRUE,
                      information = "observed", estimator = "DWLS")
summary(ESTModel.cfa.n, fit.measure = TRUE, standardized = TRUE, rsq = TRUE, modindices = TRUE)


############################################################
# Confidence Intervals for Factor Loadings
############################################################

factor_loadings <- summary_fit$rotF[, "F1"]
se_loadings <- coef(fit_5, simplify = TRUE)$items[, "a1.SE"]

z_score <- 1.96
lower_ci <- factor_loadings - z_score * se_loadings
upper_ci <- factor_loadings + z_score * se_loadings

ci_results <- data.frame(
  Item = 1:length(factor_loadings),
  Factor_Loading = factor_loadings,
  SE = se_loadings,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
)

print(ci_results)

############################################################
# Person Ability Estimates (Theta scores)
############################################################

theta2 <- as.vector(fscores(fit_5))

IRT_outcome <- content_analysis_outcomes %>%
  mutate(person = as.vector(fscores(fit_5)))

# Export person scores to Excel
write_xlsx(IRT_outcome, path = " ")


############################################################
# Model Diagnostics
############################################################

# Yen's Q3
Yen.Q3(pure[, c(1,2,3,4,6)], IRT.model = "2pl")

# Coefficient H
coefficient_H(ESTModel.cfa.n)
