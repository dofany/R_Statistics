#### 실습1 ####

View(mtcars)

### 주제 : 자동차의 실린더 수와 변속기의 관계가 있는지 알고싶다.
# 실린더 수와 변속기 종류들을 파악
table(mtcars$cyl, mtcars$am)

# 테이블의 가독성을 높이기 위해 전처리
mtcars$tm <- ifelse(mtcars$am == 0, "auto","manual")
result <- table(mtcars$cyl, mtcars$tm)

# 그래프
barplot(result)

# auto의 눈금이 벗어났기 때문에 최대값을 알 수 없다.(눈금 조정)
barplot(result, ylim=c(0,20), legend=rownames(result))

# 범례 형식을 조정
mylegend <- paste(rownames(result),"cyl")
barplot(result, ylim=c(0,20), legend=mylegend) # 실린더의 갯수

# 그래프를 수직으로 나누기
barplot(result, ylim=c(0,20), legend=mylegend, beside = T)

# 그래프를 수평으로 나누기
barplot(result, ylim=c(0,20), legend=mylegend, beside = T,horiz=T)

# 색상
barplot(result, ylim=c(0,20), legend=mylegend, beside = T,horiz=T, col=c('tan1','coral2','firebrick2'))

result

# 결과 합
addmargins(result)

# 카이제곱 검정
chisq.test(result)
fisher.test(result)

#### 실습2 ####

mydata <- read.csv("../data/anova_two_way.csv",fileEncoding = "CP949",encoding = "UTF-8")
View(mydata)

### 주제 : ad_layer(시,군,구)와 multichild(다가구 자녀지원 조례)가 관계가 있는가? (없다)
mydata <- table(mydata$ad_layer, mydata$multichild)
mydata

chisq.test(mydata)

fisher.test(mydata)

#### 실습3 ####
### 주제 : 성별과 종교는 서로 연관성이 있는가?

library(foreign)
library(ggplot2)
library(dplyr)
library(readxl)

raw_welfare <- read.spss("../data/Koweps_hpc10_2015_beta1.sav",to.data.frame = T)
welfare <- rename(raw_welfare, sex=h10_g3, birth=h10_g4, marriage=h10_g10,
                  religion=h10_g11, income=p1002_8aq1, code_job=h10_eco9,
                  code_region=h10_reg7)

wefare <- welfare[, c("sex", "birth", "marriage", "religion", "income", "code_job", "code_region")]
str(welfare)

tab <- table(welfare$sex, welfare$religion)
tab
chisq.test(tab)


#### 실습4 : Cocran-Armitage Trend Test ####
library(moonBook)
View(acs)

### 주제 : 흡연여부와 고혈압의 유무가 서로 관련이 있는가?
table(acs$HBP, acs$smoking)

# Smoking의 순서 변경
str(acs)
acs$smoking[is.na(acs$smoking)]<-"smoker"
acs$smoking <- factor(acs$smoking,levels = c("Never","Ex-smoker","smoker"))
result <- table(acs$HBP, acs$smoking)
result

chisq.test(result)

#prop.trend.test()
# x : 사건이 발생한 횟수
# n : 합계

# 고혈압이 발생한 사람의 숫자(x에 해당)
result[2, ]

# 흡연(smoking)을 시도한 횟수 
colSums(result)

prop.trend.test(result[2,],colSums(result))

# 모자이크 그래프
mosaicplot(result)

# 색상
mosaicplot(result,col=c("tan1","firebrick2","coral2"))
colors()
demo("colors") # 콘솔창에 엔터키를 누르면 색상이 바뀜

# 행과 열의 위치 변경

mosaicplot(t(result),col=c("tan1","firebrick2","coral2"))

# 레이블 장착
mosaicplot(t(result),col=c("tan1","firebrick2","coral2"),
           xlab="smoking",ylab="Hypertension")

# table 함수
mytable(smoking ~ age, data=acs)
