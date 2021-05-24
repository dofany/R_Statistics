#### 1. 단순 회귀 분석(추세선, 예측선 찾기) ####
# y = ax + b

str(women) # 미국 여성을 대상으로 키와 몸무게 조사(30~39) : 단위 - 인치, 파운드
women

plot(weight ~ height, data=women)
fit <- lm(weight ~ height, data=women) # 기울기와 절편값 확인
fit

# 선그리기
abline(fit, col="blue")
summary(fit) # R-squared : 설명력, Lm()은 p-value말고 R-squared로 확인

# 수치 : R계수^2 
cor.test(women$weight, women$height)
0.9954948^2

62*3.45 - 87.52

# 4가지 조건을 확인하기 위해 방법 
# Residuals vs Fitted : 선형선 -> 0을 기준으로 흩어진것을 곡선으로 나타냄 
# Q-Q : 정규분포 -> 선을 기준으로 데이터와의 간격이 넓으면 정규분포x 좁으면 o
# Scale-Location : 등분선 -> 데이터가 자연스럽게 흩어져 있어야됨, 선의 패턴이 일정하지 않아야함
# Residuals vs Leverage : 이성치
plot(fit) 

# 그래프 동시에 보기
par(mfrow = c(2,2))
plot(fit)

## 다항 회귀 분석(Polynomial Regression) -> 직선이 아닌 곡선을 찾는 분석. 즉 2차 함수
par(mfrow = c(1,1))
plot(weight ~ height,data=women)
abline(fit,col="blue")

fit2 <- lm(weight ~ height + I(height^2), data=women)
fit2
par(mfrow=c(2,2))
plot(fit2)

par(mfrow = c(1,1))
plot(weight ~ height,data=women)
lines(women$height, fitted(fit2), col = "red")

summary(fit2)

### 정규분포
shapiro.test(resid(fit))
shapiro.test(resid(fit2))


############################################################################################################
#### 실습1 ####
mydata <- read.csv("../data/regression.csv",fileEncoding = "CP949",encoding = "UTF-8")
View(mydata)
# social_welfare : 사회 복지 시설
# active_firms : 사업체 수
# urban_park : 도시 공원
# doctor : 의사
# tris : 폐수 배출 업소
# kindergarten : 유치원

# 종속 변수 : birth_rate
# 독립 변수 : kindergarten

## 가설 : 유치원 수가 많은 지역에 합계 출산율도 높은가?
# 또는 합계 출산율이 유치원 수에 영향을 받는가?

attach(mydata)
fit <- lm(birth_rate ~ kindergarten,data=mydata)
summary(fit)

par(mfrow=c(2,2))
plot(fit)

shapiro.test(resid(fit))

fit2 <- lm(log(birth_rate)~log(kindergarten), data=mydata)
summary(fit)

plot(fit2)
shapiro.test(resid(fit2))

# 시,군,구와 관계가 있을까? 
fit3 <- lm(birth_rate ~ dummy, data=mydata)
summary(fit3)


#### 실습2 ####
# www.kaggle.com : House sales price in kings count, USA
house <- read.csv("../data/kc_house_data.csv",header = T)
View(house)

## 주제 : 거실의 크기와 집 가격이 서로 관계가 있는가?
# 종속 변수 : price
# 독립 변수 : sqft_living

fit4 <- lm(price ~ sqft_living, data = house)
summary(fit4)

par(mfrow = c(2,2))
plot(fit4)

options("scipen"=100)
plot(fit4)

plot(house$sqft_living, house$price)

### 다중 회귀
# y = a1x2 + a2x2 + a3x3 + b

# 종속 변수 : price
# 독립 변수 : sqft_living, floors, waterfront

fit5 <- lm(price ~ sqft_living + floors+ waterfront, data=house)
summary(fit5)


### 표준화 계수 : 변수들간의 영향력 확인
install.packages("lm.beta")
library(lm.beta)

fit6 <- lm.beta(fit5)
summary(fit6)


#### 실습3 ####

### 다중 공선성
# 원인 : 독립 변수들끼리 너무 많이 겹쳐서 발생하는 문제
# 확인 방법 
#   1) 산포도, 상관 계수 : 상관 계수가 0.9를 넘게 되면 다중 공산성 문제
#   2) VIF(variance Inflation Factor) : 분산 팽창 지수
#       - 일반적으로 10보다 크면 문제가 있다고 판단(연속형 변수)
#       - 더미 변수일 경우에는 3이상이면 문제가 있다고 본다.
#       - sqrt(vif) > 2 

# 해결 방법
#   1) 변수를 뺀다.
#   2) 주성분 분석
#   3) ...


house <- read.csv("../data/kc_house_data.csv", header = T)
# 독립 변수 : sqft_living, bathrooms, sqft_lot, floors

# 변수들간의 상관 관계 확인
attach(house)
x <- cbind(sqft_living, bathrooms, sqft_lot, floors)
cor(x)

cor(x, price)

reg1 <- lm(price ~ sqft_living, data=house)
summary(reg1)

reg2 <- lm(price ~ sqft_living + floors, data=house)
summary(reg2)

# 조절변수(interactive term, 상호변수, 교호변수)
reg2_1 <- lm(price ~ sqft_living + floors + sqft_living*floors, data=house)
summary(reg2_1)

library(car)

vif(reg2_1)

x <- cbind(floors, sqft_above, sqft_basement)
cor(x)

#### 실습4 ####
View(state.x77)
# 종속 변수 : Murder

states <- as.data.frame(state.x77[, c("Murder","Population","Illiteracy","Income","Frost")])
states

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)

# 다중 공선성
sqrt(vif(fit))

### 이상 관측치
#     1) 이상치(outlier) : 표준 편차보다 2배 이상 크거나 작은 값
#     2) 큰 자레점(High leverage Points) : p(절편을 포함한 인수들의 개수) / n의 값이 2~3배 이상되는 관측치 
#       -> 평균보다 너무 먼 값을 찾는 것
#       관측치 : 5 / 50 = 0.1
#     3) 영향 관측치(Influential Observation, Cook's D)
#       독립변수의 수 / (샘플 수 - 예측 인자의 수 - 1) : 4 / (50 -4 - 1) = 0.1
#       이 값보다 클 경우

par(mfrow = c(1, 1))
influencePlot(fit, id=list(method="identify"))


#### 회귀 모델의 교정 ####

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)

par(mfrow = c(2,2))
plot(fit)

shapiro.test(resid(fit))

# 정규성을 만족하지 않은 때(결과 변수에 람다 승을 해준다.)
powerTransform(states$Murder)
# -2, -1, -0.5, -0, 0.5, 1, 2

summary(powerTransform(states$Murder))


### 선형성을 만족하지 않을 때
boxTidwell(Murder ~ Population + Illiteracy, data=states)

### 등분산을 만족하지 않을 때
ncvTest(fit) # 등분산 여부 확인

spreadLevelPlot(fit)


#### 회귀 모델의 선택 ####
# AIC(Akaike's Information Criteriion)
# Backward Stepwise Regression
#   모든 독립변수를 대상으로 해서 하나씩 빼는 방법

# Forward Stepwise Regression
#   변수를 하나씩 추가하면서 AIC값을 측정

fit7 <- lm(Murder ~ ., data=states)
summary(fit7)

fit8 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit8)

AIC(fit7, fit8)

## Backward Stepwise Regression
full.model <- lm(Murder ~ ., data=states)
reduced.model <- step(full.model, direction = "backward")
reduced.model


## Forward Stepwise Regression
min.model <- lm(Murder ~ 1, data=states)
fwd.model <- step(min.model, direction = "forward",
                  scope = (Murder~Population + Illiteracy + Income + Frost))


# All Subset Regression
install.packages("leaps")
library(leaps)

result <- regsubsets(Murder ~., data=states, nbest=4)
par(mfrow=c(1,1))
plot(result, scale="adjr2")


#### 실습 예제1 ####
# 가장 영향력있는 변수들은 무엇인가?
# 정규성 검증, 등분산성 검증, 다중공선성 검증
# 독립변수들이 출산율과 관계가 있는가? => 큰 영향을 미치진 않는다.

mydata <- read.csv("../data/regression.csv",fileEncoding = "CP949",encoding = "UTF-8")
View(mydata)
str(mydata)
head(mydata)

# 군별 제외
mydata1 <- mydata[, -1]
head(mydata1)

reg1 <- lm(birth_rate ~ ., data=mydata1)
summary(reg1)

# culture_center와 urban park는 제외
reg2 <- lm(birth_rate ~ social_welfare + active_firms + pop + doctors + tris + kindergarten + dummy, data = mydata)
summary(reg2)

# backward(하나씩 빼면서 찾는 것)
full.model <- lm(birth_rate ~ ., data=mydata1)
reduced.model <- step(full.model, direction = "backward")
reduced.model

reg3 <- lm(birth_rate ~ social_welfare + active_firms + pop + tris + kindergarten, data = mydata)
summary(reg3)

par(mfrow=c(2,2))
plot(reg2)
plot(reg3)

# 정규성 검증
shapiro.test(resid(reg2))

# 정규성이 만족하지 않으므로 결과 변수에 람다 승을 해준다.
library(car)
summary(powerTransform(mydata$birth_rate))

reg4 <- lm(birth_rate ~ social_welfare + active_firms + pop + 
             doctors + tris + kindergarten + dummy, data = mydata)
summary(reg4)
shapiro.test(resid(reg4))
plot(reg4)

# 다중 공선성(2를 넘으면 다중 공선성 의심)
sqrt(vif(reg1))
sqrt(vif(reg2))
sqrt(vif(reg3))
sqrt(vif(reg4))

# 등분산성
ncvTest(reg1)
ncvTest(reg2)
ncvTest(reg3)
ncvTest(reg4)

spreadLevelPlot(reg4)

#### 실습 예제2 ####
### 서울시 자전거 분석
## 데이터 준비
data <- read.csv("../data/SeoulBikeData.csv",fileEncoding = "CP949",encoding = "UTF-8")
summary(data)
str(data)

library(dplyr)
# 1. 시간대별로 평균 및 대가 대여 되었을까?
table(data$Hour)

result1 <- data %>% group_by(Hour) %>% summarise(count=mean(Rented.Bike.Count)) %>% arrange(desc(count))
result1


# 2. 위의 결과를 시각화 

library(ggplot2)
par(mfrow= c(1,1))

# ggplot 한글적용
font_import()
theme_set(theme_grey(base_family='NanumGothic'))

ggplot(result1, aes(Hour, count)) + 
  geom_line(color="blue",size=2) + 
  geom_vline(xintercept = 8, size=2, color="red") +
  geom_vline(xintercept = 18, size=2, color="red") + 
  annotate(geom="text", x=6,y=1100, label="출근") + 
  annotate(geom="text", x=15,y=1500, label="퇴근")

  
# 3. 2016년 1월 1일은 금요일이었다. Date변수에서 요일을 뽑아서 파생변수 만들기

fnWeek <- function(n){
  w = n %% 7;
    
  if(w == 0){
    ret = "FRI"
  }else if(w == 1){
    ret = "SAT"
  }else if(w == 2){
    ret = "SUN"
  }else if(w == 3){
    ret = "MON"
  }else if(w == 4){
    ret = "TUE"
  }else if(w == 5){
    ret = "WED"
  }else if(w == 6){
    ret = "THU"
  }
  
  return(ret)
}

View(data)
d <- as.Date(data$Date,"%d/%m/%Y") - as.Date("2016/01/01")
class(as.integer(d))

data$weekdays <- unlist(lapply(as.integer(d), fnWeek))
str(data)


# 4. 요일별로 평균 몇 대가 대여되었을까?
week.mean <- data %>% group_by(weekdays) %>% 
  summarise(count=mean(Rented.Bike.Count)) %>% arrange(desc(count))
week.mean

# 5. 위의 결과를 시각화
# par함수는 ggplot에서는 안됨
ggplot(week.mean, aes(reorder(weekdays,count), count, fill=weekdays)) + 
  geom_col() + coord_flip()

# 6. 요일 별로 시간대별 그래프로 시각화
week.date <- data %>% select(weekdays, Hour, Rented.Bike.Count) %>% 
  group_by(weekdays, Hour) %>%
  summarise(mean=mean(Rented.Bike.Count))
week.date

week.date %>% filter(weekdays=='MON')
week.date[which(week.date$weekdays == "MON"), ]

a <- ggplot(week.date[which(week.date$weekdays =="MON"), ], aes(Hour, mean)) + geom_line() + ggtitle("MON")
b <- ggplot(week.date[which(week.date$weekdays =="TUE"), ], aes(Hour, mean)) + geom_line() + ggtitle("TUE")
c <- ggplot(week.date[which(week.date$weekdays =="WED"), ], aes(Hour, mean)) + geom_line() + ggtitle("WED")
d <- ggplot(week.date[which(week.date$weekdays =="THU"), ], aes(Hour, mean)) + geom_line() + ggtitle("THU")
e <- ggplot(week.date[which(week.date$weekdays =="FRI"), ], aes(Hour, mean)) + geom_line() + ggtitle("FRI")
f <- ggplot(week.date[which(week.date$weekdays =="SET"), ], aes(Hour, mean)) + geom_line() + ggtitle("SET")
g <- ggplot(week.date[which(week.date$weekdays =="SUN"), ], aes(Hour, mean)) + geom_line() + ggtitle("SUN")


# 한 화면에 여러개의 그래프 그리기
ggplot(week.date, aes(Hour, mean)) + geom_line() + facet_wrap(~weekdays)

# 7. 선형 분석
# 각 변수들이 자전거 대여 횟수와 관련이 있는가?
# 온도에 따라 몇 대의 자전거가 대여 될까?(예를 들어 온도가 23도일때 자전거 대여횟수는 998대이다.)

# 산포도 그리기
plot(x=data$Temperature.캜., y=data$Rented.Bike.Count)

linear_reg <- lm(Rented.Bike.Count ~ Temperature.캜., data=data)
summary(linear_reg)

a = 29.0811
b = 329.9525
abline(a=b, b=a, col="red")

y = a * 23 + b
cat("온도가 23도일 때, 예측되는 대여 횟수는 ",y)        
