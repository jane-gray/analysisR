data1 <- data
View(data1)


# insert variables with calculation
data1$ALCOOLpro <- data1$ALCOOL4+data1$ALCOOL5


# Data selection
alcoolautorite1 <- data1 [, c("ALCOOLprob","AUTOparent")]


# Data selection - subset
alcoolF <- subset(alcoolautorite, SEXE == 2)


# remove NA
alcoolautorite <- na.omit(alcoolautorite)


# data cleansing - ifelse
familydata$ALCOOLmereb <- ifelse(familydata$ALCOOLmere < 1, 1, 2)
familydata$ALCOOLmereb <- ifelse(familydata$ALCOOLmereb == 1, "Non-user","User")
familydata$SEXE <- ifelse(familydata$SEXE == 1, "Male", "Female")


# data cleansing 3 - ifelse with multiple conditions
familydata$AUDITb <- ifelse((familydata$AUDIT < 3 & familydata$SEXE == "Female") | (familydata$AUDIT < 4 & familydata$SEXE == "Male"), "low-risk","high-risk")


# data cleansing 4 - NA value change
familydata$ALCOOLencore[is.na(familydata$ALCOOLencore)] <-99


# data cleansing 5 - remove variables with names
familydata <- subset(familydata, select=-c(ALCOOLmere2))


# data frame for grouping
female <- subset(familydata, SEXE=="Female")
male <- subset(familydata, SEXE=="Male")

library(dplyr)
familydataFF <- filter(familydata1, SEXE==2, AUTORITEmere>14)



# Outlier check and remove
male2$TABACage <- ifelse(male2$TABACage > 30, NA, male$TABACage)


# NA / NULL
sum(is.na(familydata1$ALCOOL5))
na.omit()





################## Descriptive analysis ###################
# Descriptive statistics
summary(familydata$AUTORITEmere)

library(psych)
describe(familydata$AUTORITEpere)

library(pastecs)
stat.desc(familydata$AUDIT, basic=FALSE, norm=TRUE)



################## Freq and Cross table analysis ################
table(male$ALCOOL6b)


library(crosstable)
crosstable.prop <- crosstable(data1, c(ALCOOLprob), by=SEXE, total = "both", showNA = "no",
                          percent_digits=0, percent_pattern = "{n} ({p_col}/{p_row})", test=TRUE)%>%
    as_flextable(keep_id=TRUE)


library(epiDisplay)
tab1(familydata$AUDITb, sort.group="decreasing", cum.percent = TRUE)


library(questionr)
questionr::freq(familydata1$ALCOOL1, cum=TRUE, sort="dec",total=TRUE)


library(gmodels)
CrossTable(familydata1$ALCOOL1, familydata1$SEXE)





######################## Chart and Graph #############################

# histogram packages
library(ggplot2)
library(tidyverse)

#Histrogram
hist(alcoolautoritemere$AUTORITEmere, breaks=20, col="white")

#Histrogram with normal curve
x <- alcoolautoritemere$AUTORITEmere
h <- hist(x, breaks=20, col="white", xlab="autority of mother",
          main = "Histrogram with normal curve")
xfit <- seq(min(x),max(x),length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit,yfit,col="blue", lwd=2)

#Scatterplot
attach(alcoolautoritemere)
plot(AUTORITEmere, ALCOOLpro, main="Scatterplot",
     xlab="autority of mother", ylab="alcohol use",pch=19)

abline(lm(ALCOOLpro~AUTORITEmere), col="red") #regression line(y~x)
lines(lowess(AUTORITEmere,ALCOOLpro), col="blue") #lowess line(x,y)


#Scatterplot packages
install.packages("ggplot")
install.packages("viridis")
install.packages("viridisLite")


##Scatterplot with color and shape by ggplot2
ggplot(alcoolautorite, aes(AUTORITEmere, ALCOOLpro, color=SEXE, shape=SEXE))+geom_point(size=6)+theme(legend.position="top")


#Scatterplot with scaled color by ggplot2
ggplot(alcoolautoritemere, aes(AUTORITEmere, ALCOOLpro))+geom_point(aes(color=ALCOOLpro))+scale_color_viridis(option="D")


#Scatterplot with size by ggplot2
AA <-xyTable(alcoolautorite$ALCOOLpro, alcoolautorite$AUTORITEmere)
coeff_bigger<-0.5
plot(AA$x, AA$y, cex=AA$number*coeff_bigger,pch=16, col=rgb(0,0,1,0.5),
     xlab = "alcohol use", ylab="autority of mother", xlim=c(0,10), ylim=c(0,40))




################# Normality and homogeneity test ##########################
## 1. Normality test 
# scatter plot with random number assignment and regression line

library(ggplot2)
scatter <- ggplot(familydata, aes(AUDIT, AUTORITEmere))
scatter2 <- scatter + geom_point(position="jitter")+labs(x="AUDIT", y="Mother's Control")
scatter3 <- scatter2 + geom_smooth(method = lm)

scatter3

# histogram with normal distribution line

library(ggplot2)
histogram <- ggplot(familydata, aes(AUTORITEmere))
histogram2 <- histogram + geom_histogram(aes(y=..density..),binwidth = 1, colour = "black", fill="white")
histogram3 <- histogram2 + stat_function(fun=dnorm, args=list(mean = mean(familydata$AUTORITEmere), sd = sd(familydata$AUTORITEmere)))

histogram3                                         
histogram2


# Shapiro-wilk normality test (if p<0.05, it is not a normal distribution)
shapiro.test(familydata$AUTORITEpere)


# Kolmogorov-Smirnov test (if p<0.05, if is not a normal distribution)
ks.test(familydata$AUTORITEpere, y = "pnorm",mean = mean(familydata$AUTORITEpere), sd=sd(familydata$AUTORITEpere))


# Skewness and Kurtosis
library(pastecs)
stat.desc(familydata$AUTORITEmere, basic=FALSE, norm=TRUE)


# Q-Q plot
library(ggplot2)
ggplot(familydata, aes(sample=AUTORITEpere)) + stat_qq()
qqnorm(familydata$AUDIT, main='Normal')
qqline(familydata$AUDIT)


#Levene Test for homogeneity of variances (if p<0.05, it has not a homogeneity of variance)
leveneTest(alcoolautorite$ALCOOLprob, alcoolautorite$SEXE)




############ Correlation analysis #########################
#Pearson and spearman correlation coefficient
library(Hmisc)
alcoolautoritemere1 <- as.matrix(alcoolautoritemere) #make sure data as matrix for using rcorr function
rcorr(alcoolautoritemere1, type=c("pearson","spearman"))

CorrelationDATA<-familydata[,c("AUDIT","AUTORITEmere")]
CorrelationDATA <- as.matrix(CorrelationDATA)
rcorr(CorrelationDATA, type=c("pearson","spearman"))

parentAUTO_co_r
parentAUTO_co_r$P # exact p-value of corrleation
parentAUTO_co_r$r # exact r of correlation

#Pearson and spearman test with one or two-sides test
#   less means negative one-side test, greater means positive one-side test
#    cor.test can be implemented for only two variables
cor.test(alcoolautorite$ALCOOLpro, alcoolautorite$AUTORITEmere, alternative="less", method="spearman") 

spearmanALCOOL<-cor.test(familydata$AUDIT, familydata$AUTORITEmere, alternative="less", method="spearman") 


#Kendall's test 
cor(alcoolautoritemere2, method="kendall")
cor(alcoolautoritemere2, method="pearson")
cor(alcoolautoritemere2, method="spearman")

cor(CorrelationDATA, method="spearman")

#Coefficient of determination
alcoolautoritemere2 <- alcoolautorite[,c("ALCOOLpro","AUTORITEmere")]
cor(alcoolautoritemere2)           # Coefficient
cor(alcoolautoritemere2)^2         # Coefficient of determination
cor(alcoolautoritemere2)^2*100     # % Coefficient of determination 


#Biserial correlation coefficient
library(polycor)
polyserial(alcoolautoritemereb$ALCOOLpro, alcoolautoritemereb$AUTORITEmereb, ML=TRUE, std.err=TRUE)


#Partial correlation coefficient (control the third variable)
install.packages("ggm")
library(Hmisc)
library(ggm)

pc <- pcor(c("ALCOOLpro","AUTORITEmere","ALCOOL2"), var(alcool_age_auto))
#ALCOOLpro and AUTORITemere are correlation value, ALCOOL2 is control value
# 3rd, 4th, 5th control values can be added.

pc^2 # R2 value

pcor.test(pc,1,270) #p-value test (data, number of third variable, number of samples)


#Comparison between correlation without or with controlling the third variable
nonpc <- cor(alcool_age_auto) #without controlling 
nonpc^2*100                   #without controlling (% coefficient of determinant)

pc <- pcor(c("ALCOOLpro","AUTORITEmere","ALCOOL2"), var(alcool_age_auto)) #with controlling
pc^2*100


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


################# Z-test for comparison the mean between two groups ###############
install.packages("BSDA")
library(BSDA)
z.test(x=female$AUDIT, y=male$AUDIT, mu=0, sigma.x=2.5, sigma.y=2.5)


names(familydata)



################ ANOVA for comparison the mean between three+ groups ##############
# One-way ANOVA
sample <- aov(AUDIT ~ AVISalcool, familydata2)
summary(sample)


# One-way ANOVA and post-hoc analysis
install.packages("DescTools")
library(DescTools)

PostHocTest(sample, method='lsd')
PostHocTest(m, method='bonferroni')


# Two-way ANOVA
sample <- aov(AUDIT ~ *ALCOOLpere, familydata)
summary(sample)


############ repeated two-way ANOVA #########################  

familydata2$AUTORITEmereb <- ifelse(familydata2$AUTORITEmere < 12, 1, 2)
familydata2$AUTORITEpereb <- ifelse(familydata2$AUTORITEpere < 8, 1, 2)

install.packages("ez")
install.packages("multcomp")
install.packages("nlme")
install.packages("pastecs")
install.packages("reshape")
install.packages("WRS", repos="http://R-Forge.R-project.org")

library(ez) ; library(multcomp) ; library(nlme) ; library(pastecs); library(reshape) ; library(WRS)

ANOVAdata <- familydata2 [, c("SEXE","AUDITb","AUTORITEmereb","AUTORITEpereb","AUDIT","AUTORITEmere","AUTORITEpere")]

ANOVAdata$SEXE <- as.factor(ANOVAdata$SEXE)
ANOVAdata$AUDITb <- as.factor(ANOVAdata$AUDITb)
ANOVAdata$AUTORITEmereb <- as.factor(ANOVAdata$AUTORITEmereb)
ANOVAdata$AUTORITEpereb <- as.factor(ANOVAdata$AUTORITEpereb)

View(ANOVAdata)


##################### Box plot #########################################################
p <- ggplot(ANOVAdata, aes(x=SEXE, y=AUDIT, fill=SEXE)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")





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



#====================================================================================================#




# data

library(readxl)
familydata<-read_excel("C:/R/rdata/familyfactor/familydata.xlsx")

setwd("C:/R/rdata/familyfactor")
familydata1<-read_excel("familydata.xlsx")
getwd()


View(familydata)
names(familydata)


# data cleansing 1 - creating and editing variables

familydata$SUBmere <- familydata$ALCOOLmere+familydata$TABACmere+familydata$CANNABISmere
familydata$SUBpere <- familydata$ALCOOLpere+familydata$TABACpere+familydata$CANNABISpere

familydata$AUTORITEmere <- familydata$AUTORITEmere - 7
familydata$AUTORITEpere <- familydata$AUTORITEpere - 7


summary(familydata$AUTORITEmere)
describe(familydata$ALCOOLmereb)

# data cleansing 2 - ifelse

familydata$ALCOOLmereb <- ifelse(familydata$ALCOOLmere < 1, 1, 2)
familydata$ALCOOLpereb <- ifelse(familydata$ALCOOLpere < 1, 1, 2)
familydata$TABACmereb <- ifelse(familydata$TABACmere < 1, 1, 2)
familydata$TABACpereb <- ifelse(familydata$TABACpere < 1, 1, 2)
familydata$CANNABISmereb <- ifelse(familydata$CANNABISmere < 1, 1, 2)
familydata$CANNABISpereb <- ifelse(familydata$CANNABISpere < 1, 1, 2)

familydata$ALCOOLmereb <- ifelse(familydata$ALCOOLmereb == 1, "Non-user","User")
familydata$ALCOOLpereb <- ifelse(familydata$ALCOOLpereb == 1, "Non-user","User")
familydata$TABACmereb <- ifelse(familydata$TABACmereb == 1, "Non-user","User")
familydata$TABACpereb <- ifelse(familydata$TABACpereb == 1, "Non-user","User")
familydata$CANNABISmereb <- ifelse(familydata$CANNABISmereb == 1, "Non-user","User")
familydata$CANNABISpereb <- ifelse(familydata$CANNABISpereb == 1, "Non-user","User")

familydata$SEXE <- ifelse(familydata$SEXE == 1, "Male", "Female")




# data cleansing 3 - ifelse with multiple conditions

familydata$AUDITb <- ifelse((familydata$AUDIT < 3 & familydata$SEXE == "Female") | (familydata$AUDIT < 4 & familydata$SEXE == "Male"), "low-risk","high-risk")


# data cleansing 4 - NA value change

familydata$ALCOOLencore[is.na(familydata$ALCOOLencore)] <-2
familydata$TABACencore[is.na(familydata$TABACencore)] <- 2
familydata$CANNABISencore[is.na(familydata$CANNABISencore)] <- 2

familydata$ALCOOLencore <- ifelse(familydata$ALCOOLencore == 1, "User", "Non-user")
familydata$TABACencore <- ifelse(familydata$TABACencore == 1, "User", "Non-user")
familydata$CANNABISencore <- ifelse(familydata$CANNABISencore == 1, "User", "Non-user")




# data cleansing 5 - remove variables with names

familydata <- subset(familydata, select=-c(ALCOOLmere2))




# data frame for grouping

female <- subset(familydata, SEXE=="Female")
male <- subset(familydata, SEXE=="Male")

library(dplyr)
familydataFF <- filter(familydata1, SEXE==2, AUTORITEmere>14)




# Outlier check and remove

male2 <- male
male2$TABACage <- ifelse(male2$TABACage > 30, NA, male$TABACage)

familydata2<-familydata
familydata2$TABACage <- ifelse(familydata2$TABACage > 30, NA, familydata2$TABACage)
familydata2$AUTORITEmereb <- ifelse(familydata2$AUTORITEmere < 12, 1, 2)
familydata2$AUTORITEpereb <- ifelse(familydata2$AUTORITEpere < 12, 1, 2)

alcool_parent$AUTORITEmereb <- familydata2$AUTORITEmereb
alcool_parent$AUTORITEpereb <- familydata2$AUTORITEpereb
tabac_parent$AUTORITEmereb <- familydata2$AUTORITEmereb
tabac_parent$AUTORITEpereb <- familydata2$AUTORITEpereb



names(familydata)
names(familydata2)


# Descriptive statistics

summary(familydata$AUTORITEmere)
summary(familydata$AUTORITEpere)

library(psych)
describe(familydata$ALCOOLpereb)


library(pastecs)
stat.desc(familydata$AUDIT, basic=FALSE, norm=TRUE)








################## Freq and Cross table analysis ################
#Frequency test
table(familydata$AUTORITEmere)


library(epiDisplay)
tab1(familydata$AUDITb, sort.group="decreasing", cum.percent = TRUE)


library(questionr)
questionr::freq(familydata1$ALCOOL1, cum=TRUE, sort="dec",total=TRUE)


library(gmodels)
CrossTable(familydata1$ALCOOL1, familydata1$SEXE)
CrossTable(familydata1$TABACencore, familydata1$SEXE)
CrossTable(familydata1$CANNABISencore, familydata1$SEXE)


# NA / NULL
sum(is.na(familydata1$ALCOOL5))
na.omit()





################################ << Parametic Test>> #########################
## 1. Normality test 
# scatter plot with random number assignment and regression line

library(ggplot2)
scatter <- ggplot(familydata, aes(AUDIT, AUTORITEmere))
scatter2 <- scatter + geom_point(position="jitter")+labs(x="AUDIT", y="Mother's Control")
scatter3 <- scatter2 + geom_smooth(method = lm)

scatter3

# histogram with normal distribution line

library(ggplot2)
histogram <- ggplot(familydata, aes(AUTORITEpere))
histogram2 <- histogram + geom_histogram(aes(y=..density..),binwidth = 1, colour = "black", fill="white")
histogram3 <- histogram2 + stat_function(fun=dnorm, args=list(mean = mean(familydata$AUTORITEpere), sd = sd(familydata$AUTORITEpere)))

histogram3                                         
histogram2

# Shapiro-wilk normality test (if p<0.05, it is significantly different with normal distribution)
shapiro.test(familydata$AUDIT)

# Kolmogorov-Smirnov test (if p<0.05, if is not a normal distribution)
ks.test(familydata$AUDIT, y = "pnorm",mean = mean(familydata$AUDIT), sd=sd(familydata$AUDIT))


# Skewness and Kurtosis
library(pastecs)
stat.desc(familydata$AUTORITEmere, basic=FALSE, norm=TRUE)


# Q-Q plot
library(ggplot2)
ggplot(familydata, aes(sample=AUTORITEpere)) + stat_qq()
qqnorm(familydata$AUTORITEpere, main='Normal')
qqline(familydata$AUTORITEpere)



## Homogenuity test
# Levene's test (if p<0.05, it has not a homogeneity of variance)

install.packages("car")
library(car)
leveneTest(alcool_parent$AUDITb, alcool_parent$SEXE,alcool_parent$ALCOOLmereb,
           alcool_parent$ALCOOLpereb, alcool_parent$AUTORITEmereb, alcool_parent$AUTORITEpereb,
           alcool_parent$ETUDEmere, alcool_parent$ETUDEpere,center=mean)

names(alcool_parent)


## Independency test for regression model
library(car)
durbinWatsonTest(multilmalcool2) # 2 is the best, less than 1 or more than 3 is a problem


# check for VIF
mean(vif(multilmalcool2)) # 1 is the best


# check for residual by eyes
plot(multilmalcool2) # values should be randomly scattered





###################### Regression analysis #####################
install.packages("car")
install.packages("QuantPsyc")

library(boot);library(car);library(QuantPsyc)
library(Rcmdr)






#simple regression (AUDIT is outcome variable, AUTOparent is predictor)
lmALCOOL.autoritem <- lm(AUDIT ~ AUTORITEmere, data=familydata, na.action=na.exclude)
lmALCOOL.autoritef <- lm(AUDIT ~ AUTORITEpere, data=familydata, na.action=na.exclude)


summary(lmALCOOL.autoritem)
summary(lmALCOOL.autoritef)


sqrt(0.01972) #root of multiple R-squared, it means pearson R (correlation)

# multiple R-squared means % of explication (model's goodness-of-fit), 
#if multiple r is similar with 1, strong correlation between two variables
# as well as model's goodness-of-fit is good
# adjust R-squared means cross-validation
#if adjust R is similar with multiple R, good cross-validation 
# F means whether my model is better than the mean model
#if F-statistics' p value <0.05, my model is better than the mean model
# Intercept estimate(4.946) mean y0 
# AUTORITEmere estimate(-0.07) mean the value that AUDIT change by AUTORITEmere
#if AUTORITEmere(predictor) change to 1, AUDIT decrease 0.07




#multiple regression (AUDIT is outcome, AUTORITEmere and ALCOOLmere are predictors)
ALCOOL.autousageM <- familydata[, c("AUDIT", "AUTORITEmere", "ALCOOLmere")]
ALCOOL.autousageF <- familydata[, c("AUDIT", "AUTORITEpere", "ALCOOLpere")]


multilmalcool2 <- lm(AUDIT ~ AUTORITEmere+ALCOOLmere, data=ALCOOL.autousageM, na.action=na.exclude)
multilfalcool2 <- lm(AUDIT ~ AUTORITEpere+ALCOOLpere, data=ALCOOL.autousageF, na.action=na.exclude)
multilmalcool1 <- lm(AUDIT ~ AUTORITEmere, data=ALCOOL.autousageM, na.action=na.exclude)

summary(multilmalcool2)
summary(multilfalcool2)
summary(multilmalcool1)

#standartized b
install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(multilmalcool2)# It means, ALCOOLmere has a greater influence on AUDIT than those of AUTORITEmere.
lm.beta(multilfalcool2)


#Confidential Interval 95%
confint(multilmalcool2)
confint(multilfalcool2)


# F ratio for comparison between A model and B model (ANOVA)
## But, the model should be a hierarchical model which is added more variables than the first model.
anova(multilmalcool1,multilfalcool2) #model 2 is better than model 1, because F(2,311) = 10.66, p < 0.01



#Outlier test
ALCOOL.autousageM$rstandard <- rstandard(multilmalcool2)
ALCOOL.autousageM$rstudent <- rstudent(multilmalcool2)
ALCOOL.autousageM$cookd <- cooks.distance(multilmalcool2)
ALCOOL.autousageM$dfbeta <- dfbeta(multilmalcool2)
ALCOOL.autousageM$dffits <- dffits(multilmalcool2)

# rstandard : check whether rstandard is bigger or smaller than 2
ALCOOL.autousageM$outlier.rstandard <- ALCOOL.autousageM$rstandard > 2 | ALCOOL.autousageM$rstandard < -2 

sum(ALCOOL.autousageM$outlier.rstandard) # how many outlier of rstandard is included in my sample ?

ALCOOL.autousageM[ALCOOL.autousageM$outlier.rstandard, c("AUDIT","AUTORITEmere","ALCOOLmere","rstandard")] 
# What case is an outlier?
# Check whether there are cases with extra ordinary residuals

ALCOOL.autousageM[ALCOOL.autousageM$outlier.rstandard, c("AUDIT","AUTORITEmere","ALCOOLmere","cookd")] 
# Check whether there are cases having more than 1 cook's distance

cooks.distance(ALCOOL.mother_glm)
cookd <- cooks.distance(ALCOOL.mother_glm) > 1
sum(cookd)
# Check whether there are cases having more than 1 cook's distance




# check for correlation between near residuals
library(car)
durbinWatsonTest(multilmalcool2) # 2 is the best, less than 1 or more than 3 is a problem


# check for VIF
mean(vif(multilmalcool2)) # 1 is the best


# check for residual by eyes
plot(multilmalcool2) # values should be randomly scattered



install.packages("car")
#Anova()
#linear.hypothesis()
#cooks.distance()
#outlieTest()
#durbin.watson()
#levene.test()




# ACM
install.packages("explor")
library(explor)
explor()


View(familydata)



# making dummy variables
ALCOOL.autousageM_Dummy <- ALCOOL.autousageM

View(ALCOOL.autousageM_Dummy)
ALCOOL.autousageM_Dummy$ALCOOLmere <- as.factor(ALCOOL.autousageM_Dummy$ALCOOLmere)

contrasts(ALCOOL.autousageM_Dummy$ALCOOLmere) <-contr.treatment(5, base=1)
# making music's dummy variable, 4 is number of category, base=1 is the baseline



View(ALCOOL.autousageM_Dummy)

# regrssion with dummy variables
dummylmM <- lm(AUDIT ~ ALCOOLmere, data = ALCOOL.autousageM_Dummy)
summary(dummylmM)


library(mlogit)
library(car)
library(dplyr)
library(stats)





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









#### example. Logistic regression for [Parental characters b + AUDITb] ####

alcool_parent <- familydata2[, c("AUDITb","ALCOOLmereb","ALCOOLpereb",
                                 "AUTORITEmereb","AUTORITEpereb","ETUDEmere","ETUDEpere",
                                 "PROFpere2","PROFmere2")]

alcool_parent$AUDITb <- as.factor(alcool_parent$AUDITb)
alcool_parent$ALCOOLmereb <- as.factor(alcool_parent$ALCOOLmereb)
alcool_parent$ALCOOLpereb <- as.factor(alcool_parent$ALCOOLpereb)
alcool_parent$AUTORITEmereb <- as.factor(alcool_parent$AUTORITEmereb)
alcool_parent$AUTORITEpereb <- as.factor(alcool_parent$AUTORITEpereb)
alcool_parent$ETUDEmere <- as.factor(alcool_parent$ETUDEmere)
alcool_parent$ETUDEpere <- as.factor(alcool_parent$ETUDEpere)
alcool_parent$PROFpere2 <- as.factor(alcool_parent$PROFpere2)
alcool_parent$PROFmere2 <- as.factor(alcool_parent$PROFmere2)


View(sub_subparent)


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




################ exercise of multi logistic regression #############

#1. creating sample dataset
ex <- data.frame(familydata$ETUDEmere, familydata$ETUDEpere, familydata$AUDITb, familydata$ALCOOL6, familydata$SEXE)


#2. Changing data as factor
is.factor(ex$familydata.AUDITb)

ex$familydata.AUDITb <- as.factor(ex$familydata.AUDITb)
ex$familydata.SEXE <- as.factor(ex$familydata.SEXE) 
ex$familydata.ETUDEmere<-as.factor(ex$familydata.ETUDEmere)
ex$familydata.ETUDEpere<-as.factor(ex$familydata.ETUDEpere)
ex$familydata.ALCOOL6<-as.factor(ex$familydata.ALCOOL6)


#3. Setting baseline group (female(1) is a baseline)
ex$familydata.SEXE <- relevel(ex$familydata.SEXE, ref=1)


#4. Changing data structure

install.packages("Ecdat")
library(Ecdat)

install.packages("mlogit")
library(mlogit)

ex1 <- mlogit.data(ex, choice = "familydata.ALCOOL6", shape="wide")


#5. Conducting mlogit with or without predictive variables
# it is my model
ex1ml <- mlogit(familydata.ALCOOL6 ~ 1 | familydata.ETUDEmere + familydata.ETUDEpere+familydata.SEXE, data = ex1, reflevel = 1)

# it is baseline model (without predictive variables)
ex1ml_base <- mlogit(familydata.ALCOOL6 ~ 1, data = ex1, reflevel = 1)


#6. Comparing two models
summary(ex1ml)
summary(ex1ml_base)

#7. Obtaining coefficients and confint
data.frame(exp(ex1ml$coefficients))
exp(confint(ex1ml))



######################### Internal reliability ##############

# Cronbach's alpha (0 = bad, 1 = good, 0.6 or higher = good)


install.packages("ltm")
library(ltm)

sample <- data.frame(familydata$ALCOOL4, familydata$ALCOOL5, familydata$ALCOOL6)
sample2 <- na.omit(sample)
cronbach.alpha(sample2)





################# Z-test for comparison the mean between two groups ###############
install.packages("BSDA")
library(BSDA)

describe(familydata$AUDIT) # sigma is the SD of population

z.test(x=female$AUDIT, y=male$AUDIT, mu=0, sigma.x=2.5, sigma.y=2.5)





################ ANOVA for comparison the mean between three+ groups ##############
# One-way ANOVA
sample <- aov(AUDIT ~ AVISalcool, familydata2)
summary(sample)


# One-way ANOVA and post-hoc analysis
install.packages("DescTools")
library(DescTools)

PostHocTest(sample, method='lsd')
PostHocTest(m, method='bonferroni')


# Two-way ANOVA
sample <- aov(AUDIT ~ *ALCOOLpere, familydata)
summary(sample)



############ repeated two-way ANOVA #########################  

install.packages("ez")
install.packages("multcomp")
install.packages("nlme")
install.packages("pastecs")
install.packages("reshape")
install.packages("WRS", repos="http://R-Forge.R-project.org")

library(ez) ; library(multcomp) ; library(nlme) ; library(pastecs); library(reshape) ; library(WRS)

ANOVAdata <- familydata2 [, c("SEXE","AUDITb","AUTORITEmereb","AUTORITEpereb","AUDIT","AUTORITEmere","AUTORITEpere")]

ANOVAdata$SEXE <- as.factor(ANOVAdata$SEXE)
ANOVAdata$AUDITb <- as.factor(ANOVAdata$AUDITb)
ANOVAdata$AUTORITEmereb <- as.factor(ANOVAdata$AUTORITEmereb)
ANOVAdata$AUTORITEpereb <- as.factor(ANOVAdata$AUTORITEpereb)

View(ANOVAdata)


##################### Box plot #########################################################
p <- ggplot(ANOVAdata, aes(x=SEXE, y=AUDIT, fill=SEXE)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")





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






############################ Correlation analysis #########################
#Pearson and spearman correlation coefficient

library(Hmisc)
parentALCOOL.age_co <- familydata2[,c("ALCOOLmere","ALCOOLpere","ALCOOLage")]
parentALCOOL.age_co <- as.matrix(parentALCOOL.age_co)
rcorr(parentALCOOL.age_co, type=c("spearman")) 

parentTABAC.age_co <- familydata_alpha[,c("TABACmere","TABACpere","TABACage")]
parentTABAC.age_co <- as.matrix(parentTABAC.age_co)
rcorr(parentTABAC.age_co, type=c("spearman")) 

parentAUTO_co_r
parentAUTO_co_r$P # exact p-value of corrleation
parentAUTO_co_r$r # exact r of correlation


#Pearson and spearman test with one or two-sides test
#   less means negative one-side test, greater means positive one-side test
#    cor.test can be implemented for only two variables and for biserial correlation
spearmanALCOOL<-cor.test(familydata$AUDIT, familydata$AUTORITEmere, alternative="less", method="spearman") 


#Kendall's test 
cor(alcoolautoritemere2, method="kendall")
cor(alcoolautoritemere2, method="pearson")
cor(alcoolautoritemere2, method="spearman")

cor(CorrelationDATA, method="spearman")
cor(CorrelationDATA, method="pearson")


#Coefficient of determination
cor(CorrelationDATA)           # Coefficient
cor(CorrelationDATA)^2         # Coefficient of determination
cor(CorrelationDATA)^2*100     # % Coefficient of determination 








