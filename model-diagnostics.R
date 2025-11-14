############### Model diagnostics for regression model ########################

###################### Regression analysis #####################
install.packages("car")
install.packages("QuantPsyc")

library(boot);library(car);library(QuantPsyc)
library(Rcmdr)

#simple regression (ALCOOL2 is outcome variable, AUTOparent is predictor)
autohis <- data1[, c("ALCOOL2", "AUTOparent", "HISpacon")]

lmalcool2 <- lm(ALCOOL2 ~ AUTOparent, data=autohis, na.action=na.exclude)

summary(lmalcool2)
sqrt(0.003832) #root of multiple R-squared, it means pearson R (correlation)

  # multiple R-squared means % of explication (model's goodness-of-fit), 
    #if multiple r is similar with 1, strong correlation between two variables
    # as well as model's goodness-of-fit is good
  # adjust R-squared means cross-validation
    #if adjust R is similar with multiple R, good cross-validation 
  # F means whether my model is better than mean model
    #if p value <0.05, my model is better than mean
  # Intercept estimate(12.713) mean y0 
  # AUTOparent estimate(0.0225) mean the value that ALCOOL2 change by AUTOparent
    #if AUTOparent(predictor) change to 1, ALCOOL2 increase 0.0225

#multiple regression (ALCOOL2 is outcome, AUTOparent and HISpacon are predictors)
multilmalcool2 <- lm(ALCOOL2 ~ AUTOparent+HISpacon, data=autohis, na.action=na.exclude)
summary(multilmalcool2)




############## Diagnostic test with regression model #################

#standartized b
install.packages("QuantPsyc")
lm.beta(multilmalcool2)

#Confidential Interval 95%
confint(multilmalcool2)

# F ratio for comparison between A model and B model
anova(lmalcool2, multilmalcool2)

#Outlier test
autohis$rstandard <- rstandard(multilmalcool2)
autohis$rstudent <- rstudent(multilmalcool2)
autohis$cookd <- cooks.distance(multilmalcool2)
autohis$dfbeta <- dfbeta(multilmalcool2)
autohis$dffits <- dffits(multilmalcool2)

install.packages("car")
    #Anova()
    #linear.hypothesis()
    #cooks.distance()
    #outlieTest()
    #durbin.watson()
    #levene.test()

# check for outlier with standard residual
autohis$outlier_rstandard <-autohis$rstandard >2 | autohis$rstandard < -2 
sum(autohis$outlier_rstandard) # number of outliers
autohis[autohis$outlier_rstandard,] # values of outliers

# check for correlation between near residuals
durbinWatsonTest(lmalcool2) # 2 is the best

# check for VIF
mean(vif(multilmalcool2)) # 1 is the best

# check for residual by eyes
plot(multilmalcool2) # values should be randomly scattered







############ Diagnostic test with logistic regression model #################
summary(glm)
summary(glm2)
# Residual deviance should be less than Null deviance : It means my model is better to predict the outcome varibles


#logistic regression - chi-squared and chi-squared 's probability
modelchi <- glm$null.deviance - glm$deviance
chidf <- glm$df.null - glm$df.residual
chisq.prob <- 1 - pchisq(modelchi, chidf)

modelchi
chidf
chisq.prob
# x2(1)=10.9, p < 0.001
# Model's goodness-of-fit is improved with x2(1)=10.9, p < 0.001. 


#logistic regression R - hosmer-lamshow
R2.hl <- modelchi/glm$null.deviance

#logistic regression R - Cox-snell
R.cs <- 1 - exp((glm$deviance - glm$null.deviance)/314)
##314 is the sample size

#logistic regression R - neikegle
R.n <- R.cs /(1-(exp(-glm$null.deviance/314)))

#logistic regression - odds ratio
exp(glm$coefficients)
exp(confint(glm))
## If odds ratio is more than 1, greater predictive variable, greater outcome variables


# Check the multicollinearity
mean(vif(glm2))
## If mean of VIF is similar with 1, there is little correlation between predictive variables


# Check the linearity between predictive variables and logit of outcome variable
ALCOOL.etudemere$logETUDE <- log(ALCOOL.etudemere$ETUDEmere)*ALCOOL.etudemere$ETUDEmere
ALCOOL.etudemere$logUSAGE <- log(ALCOOL.etudemere$AUTORITEmere)*ALCOOL.etudemere$AUTORITEmere
## Making interaction terms by multiplying log of predictive variables and their value


logglm <- glm(ALCOOL6b ~ ETUDEmere + AUTORITEmere + logETUDE + logUSAGE, data=ALCOOL.etudemere, family = binomial(), na.action=na.exclude)


summary(logglm)
## IF log variables' z>0.05, linearity condition is met.






