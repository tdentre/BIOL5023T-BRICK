#Modelling

#John Kary - model book

#Linear model (regression) has a response and predictor or (dependant and independant variable)(assumes that follows liner)
#measurement error and the predictor can influence the response.
#General linear model relaxes criteria and lets predictors be continous or categorical. generally wouldn't fit into linear
#regression model, it would be ANOVA. ANOVA is a linear model using categorical predictors, ANCOVA has both categorical and
#continous predictors. Assumptions of test: normality (distribution), heteroscedasticity (variance), and independance. 
#They refer to the residuals, variation in the response due to random error, error must be normally distributed.
#Want to have the varience in small and large values to be similar. 
#Generalized linear model relaxes the normaility even more, so no longer need to assume normaility.
#Bionomial or bernuly distribution is 0 or 1. can be framed using the log() ONLY 2 outcomes. Could be proportion, expressed
#as an odds ratio (#sussesses/#failures)
#Poisson is for non-negative, discrete; for count data. Use the log of the counts to relax constraints of normaility.
#glmm is generalized linear mixed model, can also be thought of as repeated measures.
#gam (generalized additive model) you do not assume linear relationships between the response and predictor. gamm is mixed
#model version of gam.
#B0 is intercept, B1 is slope
library(tidyverse)
