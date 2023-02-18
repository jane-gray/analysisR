#Test Branch

# 실행키

# ; = finish command
# control+enter = valid, shift+enter = enter
# console 화면 비우기 = control+l (console에서)

# 연산자
# 2x2x2 = 2^3 = 2**3 거듭제곱
# %/% = 13%/%4 = 3 몫
# %% = 13%%4 = 1 (rest) 나머지
# 3 == 4 , false (is it same?) 같다
# 3 != 4 , true  (is it not same?) 같지 않다
# !(3==4), true ('3 is the same as 4' is not true) 괄호 안의 내용을 부정
# 3>=2, true (3 is bigger than 2 or same as 2) 크거나 같다
# & = A and B 조건 A와 B를 모두 만족
# | = A or B 조건 A 또는 B를 만족
# && = A and B (only for the first data of vector) 벡터의 첫번째 데이터에만
# || = A and B (only for the first data of vector)
# X = 5:7 X에 5,6,7 값을 대입

# 데이터 유형
# mode () 함수는 데이터 유형 알려줌
# is 함수/ is.numeric(x)는 x의 데이터 유형을 알려줌
# as.numeric(x)는 x의 데이터유형을 수치형으로 바꾸는거

# 벡터 만들기
# C()함수는 여러 데이터를 가지는 벡터 생성
# seq()함수는 주기성을 가지는 수치형 벡터 생성
# seq(from=1, to=5, by=0.5)
# sequence(5) 함수는 1부터 5까지 수치형 벡터 생성
# rep() 함수는 데이터를 반복하여 벡터를 생성
# rep("a", times=5)
# rep(c("a","b"), times=5)
# rep(c("a","b"), each=3)
# rep(c("a","b"), times=c(10,5))

# 벡터 속성
# length() 데이터 개수
# names() = c("kim","lee") 벡터에 이름 부여
# names() = NULL 벡터 이름 삭제
# 벡터명[] 벡터 인덱싱
# 벡터명[2:4] 2~4번째까지 데이터 추출
# 벡터명[-c(1,2,3)] 1,2,3번째 데이터 빼고 추출

# factor 생성
# factor()
# factor(vector name, levels, labels, ordered=TRUE)

# matrix 생성
# rbind()
# cbind()
# matrix(data, nrow=2, ncol=3, byrow=TRUE) 행2, 열3인 행기준 정렬된 행렬

# array 생성
# array(data,dim=c(2,2) ) 행2, 열2인 2차원 어레이
# array(data, dim=c(2,2,2)) 행2, 열2, 높이2인 3차원 어레이

##### list 생성
# list(벡터,팩터,행렬,배열)
# names(list) = c("이름","이름") 리스트 머리에 이름부여하기

##### 데이터 프레임 생성
# data.frame(data, data, data, stringsAsFactors=FALSE)
# str() 데이터 속성 알려주는 함수

##### 데이터 프레임의 특정부분만 선택
# 새데이터프레임 <- 기존데이터프레임[행,열]

##### 조건지정해서 특정부분만 선택
# 행열 추출하기 = 데이터명[,"name"]
# 행열 조건줘서 추출하기: diamonds[diamonds$price>=18000,]

# lecturerOnly <- lecturerData[job=="Lecturer",]
# lecturerOnly <- subset(lecturerData, job=="Lecturer")
# alcoholPersonality <- lecturerData[alcohol>10, c("friends","alcohol")]
# alcoholPersonality <- subset(lecturerData, alcohol>10, select = c("friends","alcohol"))

# 패키지 설치 install.packages("패키지이름")
# 패키지 로딩 library(패키지명)
# 패키지 업데이터 update.packages("패키지이름")
# 패키지 삭제 remove.packages("패키지이름")
# 패키지 목록보기 search(), searchpaths()
# 패키지 도움말 help(packages="패키지이름")

# 데이터불러오기 read.table()
# survey_rblank=read.table(file="d:/survey_blank.txt", header=TRUE, sep=" ", na.strings="None")  
# d드라이브의 survey_blank파일을 읽는데, 헤드는 그대로 쓰고, 구분자는 공백이고, None은 결측치다

# 데이터불러오기 read.csv()
# read.csv(file="d:/인구주택총조사2015.csv",head=TRUE)

# 엑셀불러오는 페지키 = readxl
# readxl 함수, read_exel()
# read_excel(path = "/Users/jiwonjeong/Documents/R/Rpractice.xlsx",sheet=1,col_names=TRUE)

# 새변수만들기 = diamonds$xyz.sum = rowSums(diamonds[,c("x","y","z")])
# 새변수만들기 = diamonds$xyz.mean = rowMeans(diamonds[,c("x","y,"z")])

# 데이터수정하기 = diamonds[diamonds$price >= 18000, "price"] = 18000
# 데이터삭제하기 = diamonds$table = NULL
# 데이터삭제하기 = diamonds(-c(100,200,300),)
# 데이터삭제하기 = subset(diamonds, select=-c(cut,color))
# 데이터삭제하기 = data <- data[,-52] 52번째 열을 삭제해라
# 데이터삭제하기 = rm()
# 데이터 정렬하기 = diamonds[order(diamonds$price,decreasing = TRUE),]

# if구문
# if(x > 10){print("x is large")}else if(x>5){print("x is medium")}else{print("x is small")}
# diamonds$group = ifelse(diamonds$price < 100, "100 미만", ifelse(diamonds$price < 500, "100~500사이", "10000이상"))

# for구문 
#for(i in 1:5){print("hello!")} i에 1~5까지 넣어서 반복해라
#for(i in 1:5){cat("hello",i,"\n")} hello뒤에 숫자까지 넣어서 반복해라

#빈도 백분율 평균 구하기
# 빈도 = table(데이터명$변수명)
# 백분율 = prop.table(빈도)
# 반올림 = round(x,digits=1)
# 빈도백분율 구하는 패키지 = prettyR
# 결측값 제외한 평균 = mean(데이터명$변수명, na.rm=TRUE)

# R 파일 경로 : /Users/jiwonjeong/Documents/R/PR/
# 작업공간 설정하기(이곳에 엑셀 파일이 있다고 가정함) : setwd("d:/data/")
# 작업공간에 있는 엑셀 파일의 목록을 저장 : fileNames = list.files(pattern="xlsx")
# setwd("/Users/jiwonjeong/Documents/R/PR/")

# for문을 이용하여 여러 개의 엑셀 파일을 읽어오고,
# 엑셀 파일의 이름과 동일하게 각각의 R데이터를 생성한다.

#for(i in 1:length(fileNames)){
#data = read_excel(path=fileNames[i], sheet=1, col_names=TRUE)
#assign(x=fileNames[i], value=data)}

# 데이터 엑셀로 저장하기 패키지 : writexl
# 함수 : write_xlsx(데이터변수이름,path="저장파일명.xlsx")
# 데이터 엑셀로 저장하기2
# 함수 : write.csv(데이터프레임이름, "저장파일명.csv)


# table croisée

# table(Base$var1, Base$var2) <- fréquence
# prop.table(Base_HSM2$nom de variable)*100 <- pourcantage
# round(데이터, digits = 1) <- 1 digits
# Pour afficher les % colonnes, on peut utiliser cprop()
# Pour afficher les % lignes, on peut utiliser lprop()
# Pour qfficher les % totaux, on peut utiliser prop()
# tri <- table(base$age, base$sexe)

# 테이블 변수 삭제하기
# sexe_discri[,-1]
# sexe_discrimination[-1,]

# barchart
# barplot(beside=T,sexe_discrimi,main="Expérience des disciminations", 
# col=c("lightblue","mistyrose"), ylim=c(0,14000))
# +legend(1,12000,c("homme","femme"),cex=1,fill=c("lightblue","mistyrose"))

# r분석 기본 패키지
#install.packages("tidyverse")
#install.packages("R2HTML", dep=TRUE)
#library(tidyverse)
#library(questionr)
#library(forcats)
#library(haven)
#library(R2HTML)

# R file directory : /Users/jiwonjeong/Documents/R/EVSAN
# 작업공간 설정: setwd("d:/data/")
# setwd("/Users/jiwonjeong/Documents/R/EVSAN")

# 데이터 불러오기
# read_excel("/Users/jiwonjeong/Documents/R/EVSAN/Base.xls")
# data <- read_excel("/Users/jiwonjeong/Documents/R/EVSAN/Base.xls")


# Tri à plat : summary(data$variable) or freq(data$variable, total=TRUE, sort="dec")
# digits : number of digits to keep for the percentages
# cum: if TRUE, display cumulative percentages
# total : if TRUE, add a final row with totals
# sort : inc, dec
# valid : if TRUE, valid percentage
# na.last : if TRUE, NA values are be last table row

# Tri à plat, tri croisé questionr : freq(data$variable)
# 여러 콜론묶어서 테이블 만들기 : table(data$variable1, data$variable2)
# 테이블 다변량통계 패키지 gmodels : CrossTable(data$variable1, data$variable2)


# 새로운 콜론 만들기
# data$newcolon <- round(data$Nbann,0)

# barplot(data$Cas) 바차트
# hist(data$Cas) 히스토그램
# dotchart(data$Cas) 점차트
# pie(data$Cas) 파이차트

# survey_rblank=read.table(file="d:/survey_blank.txt", header=TRUE, sep=" ", na.strings="None")  

# recoder les données : questionr, forcats, irec()
# example : base에 sexe데이터를 바탕으로 Femme을 F, Homme을 H으로 하는 새로운 sexe_rec 변수생성
# irec(base)
## Recodage de base$sexe en base$sexe_rec
#base$sexe_rec <- fct_recode(base$sexe,
#  "H" = "Homme",
#  "F" = "Femme")


# table croisée
# table(Base$var1, Base$var2) <- 빈도구하기
# prop.table(데이터명$변수명)*100 <- 백분율 구하기
# round(데이터, digits = 1) <- 1자리 수까지 반올림
# Pour afficher les % colonnes, on peut utiliser cprop()
# Pour afficher les % lignes, on peut utiliser lprop()
# Pour qfficher les % totaux, on peut utiliser prop()
# tri <- table(base$age, base$sexe)

# khi-2 test
# 카이제곱 값은 χ2 = Σ (관측값 - 기댓값)2 / 기댓값 
# chisq.test(tri)
# X-squared = Khi-2
# df = le nombre de degres de liberte
# pvalue = la valeur de p que l'on peut retenir
# Il faut effectuer le test du Khi-2 sur les effectifs et non sur les pourcentqges


# 예시(데이터추출, 백분율, 빈도수)
# prof_femme <- subset(base, age>39 & sexe=="Femme", select = c("id","age","sexe","occup"))
# fre.prof2 <- prop.table(sort(table(prof_femme$occup),decreasing = TRUE))*100
# fre.profession2 <- sort(table(prof_femme$occup),decreasing = TRUE)

# 예시 (데이터프레임 생성 및 변수명 변경)
# prof_40 <- data.frame(fre.profession, fre.profession)

# prof_40 <- rename(prof_40, Profession_men = Var1, Profession_women = Var1.1)
# scatter 산점도 만들기
# scatter <- ggplot(base,aes(age,heures.tv,colour=sexe))
# scatter+geom_point()+labs(x="age",y="TV watching hours")
# 산점도에 회귀선 추가
# scatter+geom_point()+labs(x="age",y="TV watching hours")+geom_smooth(method="lm",colour="Red")
# 곡선회귀선을 추가하고 싶으면 +geom_smooth()

# packages for questionnaires
install.packages("questionr")
library(questionr)

# dataset of handicap
data(hdv2003)
base <- hdv2003
View(base)

#행개수, 열개수, 행열개수
nrow(base)
ncol(base)
dim(base)
str(base)
names(base)

#위에서 10개, 아래서 10개 데이터 보여줘
head(base,10)
tail(base,10)




