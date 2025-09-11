library(lavaan)
library(semPlot)
library(lavaanPlot)

options(max.print = 300)


# Model with final exam as the rightmost outcome variable

model.1 <- '
MAPP =~ mapp1 + mapp2 + mapp3
PAPP =~ papp1 + papp2 + papp3
MAVD =~ mavd1 + mavd2 + mavd3
PAVD =~ pavd1 + pavd2 + pavd3

Midterm_T ~ MAPP + PAPP + MAVD + PAVD
mediantheta ~ MAPP + PAPP + MAVD + PAVD+ Midterm_T 
medianLength ~ MAPP + PAPP + MAVD + PAVD + Midterm_T 
Exam ~ MAPP + PAPP + MAVD + PAVD + Midterm_T + mediantheta + medianLength

mediantheta ~~ medianLength
'

fit1<- sem(model.1, data = full_data)
summary (fit1, standardized= TRUE, fit.measures=TRUE, rsquare=TRUE,modindices=TRUE)


#Two syntaxes for generating different figures

semPaths(fit1,what="paths", whatLabels="par",style="lisrel",layout="tree",
         rotation=2)
lavaanPlot(model = fit1, node_options = list(shape = "box", 
                                             fontname = "Helvetica"), edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))



#Indirect effect

model.I<- '
MAPP =~ mapp1 + mapp2 + mapp3
PAPP =~ papp1 + papp2 + papp3
MAVD =~ mavd1 + mavd2 + mavd3
PAVD =~ pavd1 + pavd2 + pavd3

Midterm_T ~ MAPP + a11*PAPP + a41*MAVD + PAVD
mediantheta ~ MAPP + c11*PAPP + c41*MAVD + PAVD+  b11*Midterm_T 
medianLength ~ MAPP + c12*PAPP + c42*MAVD + PAVD+  b12*Midterm_T 
Exam ~ MAPP + PAPP + MAVD + PAVD + Midterm_T + mediantheta + medianLength

mediantheta ~~ medianLength

#PAPP-THETA
PPMT:= a11*b11
PPT:=PPMT+c11

#PAPP-LENGTH
PPML:= a11*b12
PPL:=PPML+c12

#MAVD-theta
MVMT:= a41*b11
MVT:=MVMT+c41

#MAVD-LENGTH
MVML:= a41*b12
MVL:=MVML+c42
'

fit.I<-sem(model.I, full_data,se="bootstrap", bootstrap = 1000)
summary(fit.I, fit.measure=TRUE, standardized=TRUE, rsq=TRUE)
