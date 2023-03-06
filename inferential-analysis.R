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



#### Regression with multiple variables and moderating effect

# Alcoolmereb * ETUDEmere is a moderator
model1 <- glm(AUDITb ~ ALCOOLmereb + ALCOOLmereb*ETUDEmere, data=familydata, family = binomial(), na.action=na.exclude)






# ACM
install.packages("explor")
library(explor)
explor(alcoolF)


# making dummy variables
contrasts(sample$music) <-contr.treatment(4, base=1)
  # making music's dummy variable, 4 is number of category, base=1 is the baseline

# regrssion with dummy variables
dummylm <- lm(day1 ~ music, data = sample)
summary(dummylm)

install.packages("car")
install.packages("mlogit")

library(mlogit)
library(car)
library(dplyr)
library(stats)

View(data2)
View(alcoolautorite)

#logistic regression
data2$ALCOOL6b <- as.factor(data2$ALCOOL6b)
  #for logistic regrssion, y should be factor

glm <- glm(ALCOOL6b ~ AUTOparentb, 
           data=data2, family = binomial(), na.action=na.exclude)
  #ALCOOL6b is outcome, AUTOparentb is predict variable
glm
glm2 <- glm(ALCOOL6b ~ AUTOparentb + HISpacon, data=data2, family = binomial(), na.action=na.exclude)

summary(glm)
  # Residual deviance should be less than Null deviance

#logistic regression - chi-squared and chi-squared 's probability
modelchi <- glm$null.deviance - glm$deviance
chidf <- glm$df.null - glm$df.residual
chisq.prob <- 1 - pchisq(modelchi, chidf)

modelchi
chidf
chisq.prob
  # x2(1)=0.22, p = 0.63
  # Model is not good because p > 0.05 

#logistic regression R - hosmer-lamshow
R2.hl <- modelchi/glm$null.deviance

#logistic regression R - Cox-snell
R.cs <- 1 - exp((glm$deviance - glm$null.deviance)/270)
  #270 is the sample size

#logistic regression R - neikegle
R.n <- R.cs /(1-(exp(-glm$null.deviance/270)))

#logistic regression - odds ratio
exp(glm$coefficients)
exp(confint(glm))






#####################logistic regression###################################
ALCOOL.etudemere <- familydata[, c("ALCOOL6b", "AUTORITEmere", "ALCOOLmereb","ETUDEmere")] 
ALCOOL.etudemere$ALCOOL6b <- as.factor(ALCOOL.etudemere$ALCOOL6b) #for logistic regrssion, y should be factor


# ALCOOL6b is outcome, ETUDEmere and AUTORITEmere are predict variables
glm

glm2 <- glm(ALCOOL6b ~ ETUDEmere + AUTORITEmere, data=ALCOOL.etudemere, family = binomial(), na.action=na.exclude)
glm2


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





############logistic regression with relevel factor levels (reference) ###############
alcool_parent$AUDITb <- relevel(alcool_parent$AUDITb,"low-risk")
levels(alcool_parent$AUDITb)


ALCOOL.mother_glm <- glm(AUDITb ~ ALCOOLmereb+AUTORITEmereb+ETUDEmere+PROFmere2+SEXE, 
                         data=alcool_parent, family = binomial(), na.action=na.exclude)

ALCOOL.father_glm <- glm(AUDITb ~ ALCOOLpereb+AUTORITEpereb+ETUDEpere+PROFpere2+SEXE, 
                         data=alcool_parent, family = binomial(), na.action=na.exclude)


summary(ALCOOL.mother_glm)
summary(ALCOOL.father_glm)



TABAC.mother_glm <- glm(TABACencore ~ TABACmereb+AUTORITEmereb+ETUDEmere+PROFmere2+SEXE, 
                        data=tabac_parent, family = binomial(), na.action=na.exclude)

TABAC.father_glm <- glm(TABACencore ~ TABACpereb+AUTORITEpereb+ETUDEpere+PROFpere2+SEXE, 
                        data=tabac_parent, family = binomial(), na.action=na.exclude)


summary(TABAC.mother_glm)
summary(TABAC.father_glm)


# Odds ratio (crude OR vs adjusted OR)
library(epiDisplay)
logistic.display(ALCOOL.mother_glm)
logistic.display(ALCOOL.father_glm)

logistic.display(TABAC.mother_glm)
logistic.display(TABAC.father_glm)







########### logistic regression with backward approach #############

##logistic regression

ALCOOL.mother_glm <- glm(AUDITb ~ ALCOOLmereb+AUTORITEmereb+ETUDEmere+PROFmere2, 
                         data=alcool_parent, family = binomial(), na.action=na.exclude)

ALCOOL.father_glm <- glm(AUDITb ~ ALCOOLpereb+AUTORITEpereb+ETUDEpere+PROFpere2, 
                         data=alcool_parent, family = binomial(), na.action=na.exclude)


summary(ALCOOL.mother_glm)
summary(ALCOOL.father_glm)


# backward regression
reduced.model = step(ALCOOL.mother_glm, direction = "backward") # redefining the regression model without non-significant variables
summary(reduced.model)



# Odds ratio formula
ORtable=function(x,digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result =round(result,digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5","p")
  result}

ORtable(reduced.model) # It is the result of adjusted OR



# Odds ratio (crude OR vs adjusted OR)
library(epiDisplay)
logistic.display(reduced.model)




###################### Factorial analysis #############################

install.packages("FactoMineR")
install.packages("Factoshiny")
install.packages("FactoInvestigate")
library(FactoMineR) ; library(Factoshiny) ; library(resshiny)

FAdata <- familydata2[,c("AUTORITEmereb","AUTORITEpereb","AUDITb","TABACencore","CANNABISencore")]

FAdata$AUTORITEmereb <- as.factor(FAdata$AUTORITEmereb)
FAdata$AUTORITEpereb <- as.factor(FAdata$AUTORITEpereb)
FAdata$AUDITb <- as.factor(FAdata$AUDITb)
FAdata$TABACencore <- as.factor(FAdata$TABACencore)
FAdata$CANNABISencore <- as.factor(FAdata$CANNABISencore)

FA <- MCA(FAdata, ncp = 4, graph = FALSE) # ncp means the number of axes

FAresult<-Factoshiny(FA)
resshiny = MCAshiny(FA)
