# Basic R packages for data analysis
library(tidyverse)
library(questionr)
library(forcats)
library(haven)
library(R2HTML)




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

