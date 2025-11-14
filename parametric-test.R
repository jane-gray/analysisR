################################ Assumption & Parametric Tests #########################
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

# Shapiro-wilk normality test (if p<0.05, it is significantly different with normal distribution, using Non-parametric test)
shapiro.test(familydata$AUDIT)

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


# check for residual by plot
plot(multilmalcool2) # values should be randomly scattered





######################### Internal reliability ##############

# Cronbach's alpha (0 = bad, 1 = good, 0.6 or higher = good)


install.packages("ltm")
library(ltm)

sample <- data.frame(familydata$ALCOOL4, familydata$ALCOOL5, familydata$ALCOOL6)
sample2 <- na.omit(sample)
cronbach.alpha(sample2)

