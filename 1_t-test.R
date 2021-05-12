#### Power Analysis ####
# 적정한 표본의 갯수를 산출
# cohen's d (effective size) : 두 집단의 평균 차이를 두 집단의 표준편차의 합으로 나눠준다.
# # 절대값(1의평균 - 2의평균) / 루트(제곱근)((1.표준편차^2 + 2.표준편차^2) /2)

ky <- read.csv("../data/KY.csv",fileEncoding = "CP949", encoding="UTF-8")
View(ky)

table(ky$group)

mean.1 <- mean(ky$score[which(ky$group==1)])
mean.2 <- mean(ky$score[(ky$group==2)])
cat(mean.1, mean.2)

sd.1 <- sd(ky$score[which(ky$group == 1)])
sd.2 <- sd(ky$score[ky$group == 2])
cat(sd.1,sd.2)

effective_size <- abs(mean.1 - mean.2) / sqrt((sd.1^2 + sd.2^2) /2)
effective_size

install.packages("pwr")
library(pwr)
pwr.t.test(d=effective_size, alternative="two.sided", type="two.sample", power=.8, sig.level=.05) 
# d : effective size의 값 , alternative : 양측,단측 검사, type : 1,2 개 표본 또는 쌍 표본 power : 검정력 sig.level : 유의 수준


#### 사례 : 두 집단 간의 평균 비교 ####
install.packages("moonBook")
library(moonBook)
options("scipen"=999)

# 경기도에 소재한 대학병원에서 2년동안 내원한 급성 관상동맥증후군 환자 데이터
acs

## 가설 설정
# 주제 : 두 집단(남성, 여성)의 나이 차이를 알고 싶다.
# 귀무가설 : 남성과 여성의 평균 나이에 대해 차이가 없다.
# 대립가설 : 남성과 여성의 평균 나이에 대해 차이가 없다.

head(acs)
# 남성과 여성의 평균나이
mean.man <- mean(acs$age[acs$sex=="Male"])
mean.woman <- mean(acs$age[acs$sex=="Female"])
cat(mean.man, mean.woman)

### 정규분포 테스트
moonBook::densityplot(age~sex, data=acs) # 결과변수 ~ 원인변수, 데이터

## 가설 설정
# 주제 : 두 집단의 정규분포 여부를 알고 싶다.
# 귀무 가설 : 두 집단이 정규 분포이다.
# 대립 가설 : 두 집단이 정규 분포가 아니다.

shapiro.test(acs$age[acs$sex=="Male"]) # p-value : 대립 < 5 / 귀무 > 95
shapiro.test(acs$age[acs$sex=="Female"])

### 등분산 테스트 
# 주제 : 두 집단의 등분산 여부를 알고 싶다.
# 귀무 가설 : 두 집단이 등분산이다.
# 대립 가설 : 두 집단이 등분산이 아니다.

var.test(age~sex, data=acs) 

### 가설 검정
# MWW 검정
wilcox.test(age~sex, data=acs)

# t-test
t.test(age~sex, data=acs, alt="two.sided",var.equal=T)  #alt : 양측, 단측, var.equal : 등분산이 같은지 다른지

# welch's test
t.test(age~sex, data=acs, alt="two.sided",var.equal=F)



#### 사례2 : 집단이 한개인 경우 ####
# A회사의 건전지 수명이 1000시간일때, 무작위로 뽑아 10개의 건전지 수명에 대해 샘플이 모집단과 다르다고 할 수 있는가?
# 귀무가설 : 표본의 평균은 모집단의 평균과 같다.
# 대립가설 : 표본의 평균은 모집단의 평균과 다르다.

a <- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)
mean.a <- mean(a)
mean.a

## 정규 분포 여부
shapiro.test(a)
t.test(a,mu=1000, alt = "two.sided")
t.test(a,mu=1000, alt = "less")
t.test(a,mu=1000, alt = "greater")

### 어떤 학급의 수학 평균성적이 55점이었다. 0교시 수업을 하고 다시 성적을 살펴보았다.
b <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
mean(b)

shapiro.test(b)
t.test(b, mu=55, alt="two.sided")
t.test(b, mu=55, alt="greater")


#### 사례3: Paried Sample T-Test ####
### 같은 집단에 대해 수면시간이 차이가 나는지 알고싶다.
str(sleep)
View(sleep)

# ID를 제거하여 서로 두 집단으로 테스트를 먼저 해보자.
sleep2 <- sleep[,-3]
View(sleep2)

# 두 집단의 수명증가량 평균은?
tapply(sleep2$extra, sleep2$group, mean)

# 정규분포
shapiro.test(sleep2$extra[sleep2$group==1])
with(sleep2,shapiro.test(extra[group==1]))

shapiro.test(sleep2$extra[sleep2$group==2])

# 등분산
var.test(extra ~ group,sleep2)

# t-test
t.test(extra~group, data=sleep2, paired=F, var.equal=T)

### Paired sample t-test
t.test(extra~group, data=sleep, paired=T, var.equal=T)

### 그래프 그리기

before <- subset(sleep,group==1,extra)
before
after <- subset(sleep,group==2,extra)
after

install.packages("PairedData")
library(PairedData)

#s_graph <- cbind(before,after)
#s_graph

s_graph <- paired(before,after)
s_graph

plot(s_graph, type="profile") + theme_bw()


#### 실습1 ####
# dummy : 0은 군을 나타내고, 1은 시를 나타낸다.
### 주제 : 시와 군에 따라서 합계 출산율의 차이가 있는지 알아보려고 한다.
### 귀무가설 : 차이가 없다.
### 대립가설 : 차이가 있다.

mydata <- read.csv("../data/independent.csv",fileEncoding = "CP949",encoding = "UTF-8")
View(mydata)

gun.mean <- with(mydata, mean(birth_rate[dummy==0]))
si.mean <- with(mydata, mean(birth_rate[dummy==1]))
cat(gun.mean,si.mean)

# tapply(mydata$birth_rate, mydata$dummy, sum)

with(mydata, shapiro.test(birth_rate[dummy==0]))
with(mydata, shapiro.test(birth_rate[dummy==1]))

wilcox.test(birth_rate~dummy, data = mydata)
t.test(birth_rate~dummy, data = mydata)

#### 실습2 ####
### am : 0은 오토, 1은 수동
### mpg : 연비

str(mtcars)
head(mtcars)

a_mpg <- mean(mtcars$mpg[mtcars$am==0])
m_mpg <- mean(mtcars$mpg[mtcars$am==1])
cat(a_mpg, m_mpg)

# 정규 분포
shapiro.test(mtcars$mpg[mtcars$am==0])
shapiro.test(mtcars$mpg[mtcars$am==1])


# 등분산 테스트
var.test(mtcars[mtcars$am==0, 1],mtcars[mtcars$am==1, 1])

t.test(mpg~ am, data=mtcars, var.equal=T,alt = "less")


#### 실습3 ####
### 쥐의 몸무게가 전과 후의 차이가 있는지 없는지 알고싶다.

data <- read.csv("../data/pairedData.csv")
data

install.packages("reshape2")
library(reshape2)

data1 <- melt(data, id=("ID"),variable.name="GROUP",value.name = "RESULT")
data1

# 구조를 바꾸는 또 다른 방법
install.packages("tidyr")
library(tidyr)

data2 <- gather(data, key="GROUP", value="RESULT", -ID)
data2

shapiro.test(data2$RESULT[data2$GROUP=="before"])
shapiro.test(data2$RESULT[data2$GROUP=="After"])

t.test(RESULT ~ GROUP, data=data2, paired=T)

# long형으로 바꾸지 않아도 사용 가능
t.test(data$before, data$After, paired = T)

# 그래프
before <- subset(data2, GROUP=="before", RESULT)
before
after <- subset(data2, GROUP=="After", RESULT)
after

data3 <- paired(before,after)
data3
plot(data3, type="profile") + theme_bw()

moonBook::densityplot(RESULT ~ GROUP, data=data2)


#### 실습4 ####
### 주제 : 시별로 2010년도와 2015년도의 출산율 차이가 있는가?

data <- read.csv("../data/paired.csv",fileEncoding = "CP949",encoding="UTF-8")
View(data)

# long으로 구조 변경
data2 <- gather(data, key="GROUP",value="RESULT",-c(ID,cities))
data2

with(data2,shapiro.test(RESULT[GROUP=="birth_rate_2010"]))
with(data2,shapiro.test(RESULT[GROUP=="birth_rate_2015"]))

wilcox.test(RESULT~GROUP,data=data2, paired=T)

t.test(RESULT~GROUP, data=data2, paired=T)


#### 실습5 ####
### https://www.kaggle.com/kappernielsen/independent-t-test-example
### 주제 ㅣ 수학 점수가 남학생과 여학생에 따라서 같은지 다른지 알고싶다.
### 첫번째 시험(G1)과 세번째 시험(G3)을 사용

mat <- read.csv("../data/student-mat.csv",header = T)
str(mat)
View(mat)

math = subset()
