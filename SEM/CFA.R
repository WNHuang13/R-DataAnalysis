library(lavaan)
library(measureQ)
library(semTools)
library(mccimm)
library(MASS)
library(semPlot)
library(reliable)

Model.A <- '
             MAPP =~ mapp1 + mapp2 + mapp3
             PAPP =~ papp1 + papp2 + papp3
             MAVD =~ mavd1 + mavd2 + mavd3
             PAVD =~ pavd1 + pavd2 + pavd3
'

ESTModel.A <- cfa (Model.A, data = full_data, meanstructure=TRUE, information="observed")
summary(ESTModel.A, fit.measure=TRUE, standardized=TRUE,modindices=TRUE)

coefficient_H(ESTModel.A)

