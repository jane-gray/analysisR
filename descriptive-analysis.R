# Basic R packages for data analysis
library(tidyverse)
library(questionr)
library(forcats)
library(haven)
library(R2HTML)




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








##### Summary statistics, frequency, percentage, mean #####
table(data$value)   #freaquency
prop.table()*100    #percentage
round(x,digits=1)   #round
mean(data$value, na.rm=TRUE) #mean





##### Cross tab analysis #####

table(Base$var1, Base$var2) # frequency
prop.table(Base_HSM2$nom de variable)*100 # percentage
cprop() #percentage of column
rprop() #percentage of row

summary(data$variable) # summary statistics and cross tab (ver.2)
summary(data$variable[data$PAYS == "Coree"]) # summary statistics with conditional data (using only data where PAYS = 'Coree')
sd(data$variable[data$PAYS == "Coree"], na.rm = TRUE) # standard deviation with conditional data (using only data where PAYS = 'Coree')
mean(data$variable[data$PAYS == "Coree"], na.rm = TRUE) # mean with conditionnal data (using only data where PAYS = 'Coree')
freq(data$variable, total=TRUE, sort="dec") #cross tab analysis (ver. 3)
CrossTable(data$variable1, data$variable2) #cross tab analysis (ver. 4 - using gmodels package)




###### chi-squared test, Fisher's exact test (with categorical variables) #######
# chi-squared test (Cell expected frequency ≥ 5)
cont_table <- table(data$AUDIT, data$age, data$PAYS)
cont_table
prop.table(cont_table, margin=2) ## column-wise percentage
chisq.test(cont_table) #chi-squared test
fisher.test(cont_table) #Fisher's exact test (At least one cell with expected frequency < 5)


####### t-test (with continous variables, mean comparison)
t.test(data$variable_numeric, data$variable_numeric2)



##### Chart #####

barplot(data$variable) #barchart
hist(data$variable) #histogram
dotchart(data$variable) #dot chart
pie(data$variable) #pie chart


# barchart example
barplot(beside=T,sexe_discrimi,main="Expérience des disciminations", 
col=c("lightblue","mistyrose"), ylim=c(0,14000))
+legend(1,12000,c("homme","femme"),cex=1,fill=c("lightblue","mistyrose"))


# scatter plot example
scatter <- ggplot(base,aes(age,heures.tv,colour=sexe))
scatter+geom_point()+labs(x="age",y="TV watching hours")

# scatter plot with linear regression line example
scatter+geom_point()+labs(x="age",y="TV watching hours")

# scatter plot with curved regression line example
scatter+geom_point()+labs(x="age",y="TV watching hours")+geom_smooth(method="lm",colour="Red")







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




