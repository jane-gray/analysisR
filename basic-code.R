###### Basic Execution Key #####
# ;             (finish command)
# control+enter (Code Execution) 
# shift+enter   (enter)
# control+l     (console cleaning)



##### Operating Keys #####
# 2x2x2 = 2^3 = 2**3      (cubic)
# %/% = 13%/%4 = 3        (rest)
# %% = 13%%4 = 1          (rest)
# 3 == 4 , false          (is it same?)
# 3 != 4 , true           (is it not same?)
# !(3==4), true           ('3 is the same as 4' is not true)
# 3>=2, true              (3 is bigger than 2 or same as 2) 
# &                       (A and B) 
# |                       (A or B) 
# &&                      (A and B only for the first data of vector) 
# ||                      (A and B only for the first data of vector)
# X = 5:7                 (set 5,6,7 into the value X)



##### package control #####
# install.packages("package name")         (package intall)
# library(package name)                    (package loading)
# update.packages("package name")          (package uptate)
# remove.packages("package name")          (remove package)
# search(), searchpaths()                  (search package lists)
# help(packages="package name")            (package instruction)




##### Data loarding #####

# txt Data loading
read.table()

# example
survey_rblank=read.table(file="d:/survey_blank.txt", header=TRUE, sep=" ", na.strings="None")  
# load survey_blank file in d drive with head of data, separated by blank, na value is None)

# csv Data loading
read.csv()

# example
read.csv(file="d:/BS2015.csv",head=TRUE)

# xl data loading
read_excel()

#example
read_excel(path = "/Users/jiwonjeong/Documents/R/Rpractice.xlsx",sheet=1,col_names=TRUE)
# load excel data sheet 1 with pathway including head of data)




##### Data saving #####
setwd("d:/data/") #set working directory
fileNames = list.files(pattern="xlsx") #save the list of xlsx fils in working directory

# Package for xlsx data saving : writexl
write_xlsx(dataname,path="savingname.xlsx")
write.csv(dataname, "savingname.csv")






##### Data type #####
# mode(x)                 (What is data type of x?) 
# is.numeric(x)           (Is data type of x numeric?)
# is.caracter(x)
# is.factor(x)
# as.numeric(x)           (Change data tpye of x as numeric)
# as.caracter(x)
# as.factor(x)




##### Vector (vector is one lined datset) #####
# c()                              (make vector combining data)
# seq()                            (make vector with sequance data)
# seq(from=1, to=5, by=0.5)        (make vector including data from 1 to 5 by 0.5 sequence)
# sequence(5)                      (make vector from 1 to 5)
# rep()                            (make vector by repeating data)
# rep("a", times=5)                (make vector by repeating a for 5 times)
# rep(c("a","b"), times=5)         (make vector by repeating a,b for 3 times)
# rep(c("a","b"), each=3)          (make vector by repeating a,b for 3 times each)
# rep(c("a","b"), times=c(10,5))   (make vector by repeating a for 10 times, b for 5 times)  

# length()                         (length of vector)
# names() = c("kim","lee")         (give names to each data)
# names() = NULL                   (delete names)
# x[]                              (indexing certain data in vector x) 
# x[2:4]                           (indexing data from 2 to 4 in vector x)
# x[-c(1,2,3)]                     (indexing all data except data from 1 to 3 in vector x) 



##### Factor #####
# factor()                         (make factor)
# factor(vector name, levels, labels, ordered=TRUE)



##### Matrix #####
# rbind()                          (make row bined matrix)
# cbind()                          (make column bined matrix)
# matrix(data, nrow=2, ncol=3, byrow=TRUE) (make 2 row x 3 column matrix)



##### Array #####
# array(data,dim=c(2,2) )          (make 2 row, 2 column, 1 height 3D array)
# array(data, dim=c(2,2,2))        (make 2 row, 2 column, 2 height 3D array) 



##### list #####
# list(vector,factor,matrix,array) (make a list inclduing vector, factor, matrix, array)
# names(list) = c("name1","name2") (give names to list) 




##### Data frame #####

#create data frame (version 1)
df <- data.frame(name=c('A', 'B', 'C', 'D', 'E'),
                 score=c(99, 90, 86, 88, 95),
                 subscore=c(33, 28, 31, 39, 34),
                 risk=c(30, 28, 24, 24, 28))

# creat data frame (version 2)
name=c('A', 'B', 'C', 'D', 'E')
score=c(99, 90, 86, 88, 95)
subscore=c(33, 28, 31, 39, 34)
risk=c(30, 28, 24, 24, 28))
df <- data.frame(name, score, subscore,risk, stringsAsFactors=FALSE)

#data frame desciption
str(df)       #data string
nrow(df)      #number of row
ncol(df)      #number of colum
dim(df)       #dimension of data
names(df)     #names of data

#Select certain parts of existing data frame 
df2 <- df[2,3] #select and put 2 row and 3 colum data from existing data frame df into new data frame df2

#Select certain parts of existing data using data name
df3 <- df[,"score"] #select and put all data of "score" into df3, "score" is data name

#Select certain parts of existing data using condition
df4 <- df[df$score>=80,] #select only the data with scores greater than 80 from df and put them into df4.

# example
lecturerOnly <- lecturerData[job=="Lecturer",]
lecturerOnly <- subset(lecturerData, job=="Lecturer")
alcoholPersonality <- lecturerData[alcohol>10, c("friends","alcohol")]
alcoholPersonality <- subset(lecturerData, alcohol>10, select = c("friends","alcohol"))



##### Data modification #####

# Creat new value
df$rank = df[,c("1","2","3","4","5")])     # Make new value 'rank' by adding 1,2,3,4,5 in df

# Modify value 
df$rank[1] = 0     # change first value of rank in df as 0

# Renaming
df <- rename(df, score = Var1, risk = Var2)


# Delete value
df$rank = NULL    # remove rank in df
df$rank <- df$rank[,-4] #remove 4th column of rank in df
rm(df) #remove df



###### if function #####
if(x > 10){print("x is large")}        #first condition
else if(x>5){print("x is medium")}     #second condition
  else{print("x is small")}            #third condition

diamonds$group = ifelse(diamonds$price < 100, "less than 100", ifelse(diamonds$price < 500, "100~500", "more than 10000"))



##### for function ##### 
for(i in 1:5){print("hello!")}        #put 1 to 5 into i and repeat "hello!" until the condition is satisfied
for(i in 1:5){cat("hello",i,"\n")}    #put 1 to 5 into i and repeat "hello+i" and space until the condition is satisfied




#### while function #####
# basic while code
i <- 1
n <- 10
while (i <= n) {                     # Repeating condition : prints (i) repeatedly, adding +1 until i is less than or equal to n(10)
  print(i)
  i = i + 1
}


# Using break & next while code
i <- 1
n <- 10
while (i <= n) {
  if(i == 9){
    break                          # stop and exit if repeating output value is 9
  }
  if(i == 8){
    i = i + 1                      # keep going if repeating output value is 8
    next
  }
  print(i)
  i = i + 1                        # Repeating condition : prints (i) repeatedly, adding +1
}



# nested while loop
i=1
while (i <= 4) {                   
  j = 1                             # First repeating condition : prints (i+j) repeatedly, adding +1 until i is less than or equal to (4)
  while(j < 2) {                    # Second repeating condition : put (j) repeatedly, adding j+1 until j is less than (2)
    print(i + j)
    j = j + 1
  }
  i = i + 1                         
}



###### Basic R packages for data analysis #####
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






