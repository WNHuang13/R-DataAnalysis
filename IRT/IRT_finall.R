library(ggmirt)
library(tidyverse)
library(mirt)
library(subscore)
library(lavaan)
library(ggplot2)
library(RColorBrewer)

content_analysis_outcomes <- read_sav(
  "  ")

pure <- content_analysis_outcomes[,c(5:11)]


#seven factors for 1PLM
fit1PL <- mirt(content_analysis_outcomes[,c(5:11)], 1, itemtype = "Rasch", verbose = F)
M2(fit1PL)

#seven factors for 2PLM
fit2PL <- mirt(content_analysis_outcomes[,c(5:11)], 1, itemtype = "2PL", verbose = F)
fit2PL

summary(fit2PL)
M2(fit2PL)
itemfit(fit2PL)

params2PL <- coef(fit2PL, IRTpars = TRUE, simplify = TRUE)
round(params2PL$items, 3) 

tracePlot(fit2PL, title="",facet = F, legend = T) + scale_color_brewer(palette = "Set1") +
  scale_color_hue(labels=c("Verification","Justification","Explication","Source",
                           "Example","Metacognition","Emotion"))

p <- tracePlot(fit2PL, title="", facet = FALSE, legend = TRUE)

p + 
  scale_color_manual(
    values = c(
      "#e41a1c", 
      "#a65628",
      "#4daf4a", 
      "#ff7f00", 
      "#377eb8",
      "#984ea3",
      "#e7298a"
    ),
    labels = c("Verification", "Justification", "Explication", "Source", 
               "Example", "Metacognition", "Emotion")
  )




#CFA for seven factors
model.cfa <- "feedback =~ Verification+Justifications+Explications+Sources+Example+Metacognition+Motivation"
ESTModel.cfa <- cfa (model.cfa, data = pure, meanstructure=TRUE, information="observed",estimator = "DWLS")
summary(ESTModel.cfa, fit.measure=TRUE, standardized=TRUE, rsq=TRUE,modindices=TRUE)


#five factors model for 1PLM
fit_51 <- mirt(content_analysis_outcomes[,c(5,6,7,8,10)], 1, itemtype = "Rasch", verbose = F)
M2(fit_51)

#final model containing 5 factors for 2PLM
fit_5 <- mirt(content_analysis_outcomes[,c(5,6,7,8,10)], 1, itemtype = "2PL", verbose = F, SE = TRUE)
fit_5

summary_fit <-summary(fit_5)
M2(fit_5)
itemfit(fit_5)

params_5 <- coef(fit_5, IRTpars = TRUE, simplify = TRUE)
round(params_5$items, 3) 

tracePlot(fit_5, title="",facet = F, legend = T) + scale_color_brewer(palette = "Set1") +
  scale_color_hue(labels=c("Verification","Justification","Explication","Source","Metacognition"))


p2 <- tracePlot(fit_5, title="", facet = FALSE, legend = TRUE)

p2 + 
  scale_color_manual(
    values = c(
      "#e41a1c", 
      "#a65628",
      "#4daf4a", 
      "#ff7f00", 
      "#984ea3"  
    ),
    labels = c("Verification","Justification","Explication","Source","Metacognition")
  )



#CI for factor loadings in IRT
factor_loadings <- summary_fit$rotF[,"F1"]
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


# theta
theta2 <- as.vector(fscores(fit_5))

IRT_outcome <- content_analysis_outcomes %>% 
  mutate(person=as.vector(fscores(fit_5)))

itemInfoPlot(fit_5, facet = T) 

write_xlsx(IRT_outcome,path="   ")

#CFA for five factors
model.cfa.n <- "feedback =~ Verification+Justifications+Explications+Sources+Metacognition"
ESTModel.cfa.n <- cfa (model.cfa.n, data = pure, meanstructure=TRUE, information="observed",estimator = "DWLS")
summary(ESTModel.cfa.n, fit.measure=TRUE, standardized=TRUE, rsq=TRUE,modindices=TRUE)

#Q3 to prove independence
Yen.Q3(pure[,c(1,2,3,4,6)],IRT.model="2pl")

library(reliable)
coefficient_H(ESTModel.cfa.n)
