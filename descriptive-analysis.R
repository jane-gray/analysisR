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








##### frequency, percentage, mean #####
table(data$value)   #freaquency
prop.table()*100    #percentage
round(x,digits=1)   #round
mean(data$value, na.rm=TRUE) #mean





##### Cross tab analysis #####

table(Base$var1, Base$var2) # frequency
prop.table(Base_HSM2$nom de variable)*100 # percentage
cprop() #percentage of column
rprop() #percentage of row

summary(data$variable) #cross tab analysis (version 2)
freq(data$variable, total=TRUE, sort="dec") #cross tab analysis (version3)
CrossTable(data$variable1, data$variable2) #cross tab analysis (version4 - using gmodels package)



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

