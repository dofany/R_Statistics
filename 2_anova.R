#### 사례1: One Way Anova ####
library(moonBook)
View(acs)

# LDLC : 저밀도 콜레스테롤 수치 : 종속(결과) 변수
# Dx(진단 결과) : STEMI(급성심근경색), NSTEMI(만성심근경색), unstable angina(협심증) : 독립 변수

moonBook::densityplot(LDLC ~ Dx, data=acs)# 진단 결과에 따라 LDLC를 알고싶다.

# 정규분포 검정
# 0.05 미만이면 정규분포 Fail, 0.05이상이면 정규분포 Pass
with(acs, shapiro.test(LDLC[Dx=="NSTEMI"]))
with(acs, shapiro.test(LDLC[Dx=="STEMI"]))
with(acs, shapiro.test(LDLC[Dx=="Unstable Angina"]))

# 정규분포를 확인하는 또다른 방법
out = aov(LDLC~Dx, data=acs) # 분산 분석
shapiro.test(resid(out)) 

# 등분산 여부
bartlett.test(LDLC~Dx, data=acs)

# anova 검정
out = aov(LDLC ~ Dx, data=acs)
summary(out) # P-value값에 *이 많이 붙을수록 강하게 차이가 있는거다.

# 연속변수가 아니거나 정규분포가 아닐 경(비모수적)
kruskal.test(LDLC~Dx, data=acs)

# 등분산이 아닐경우
oneway.test(LDLC ~ Dx, data=acs, var.equal = F)


### 사후 검정

# aov()를 사용했을 경우 : TukeyHSD()
TukeyHSD(out)

# kruskal.test()를 사용했을 경우
install.packages("pgirmess")
library(pgirmess)

kruskalmc(acs$LDLC, acs$Dx)

# oneway.test를 사용했을 경우
install.packages("nparcomp")
library(nparcomp)

result <- mctp(LDLC ~ Dx, data=acs)
summary(result)

#### 실습1 ####
head(iris)

# 주제 : 품종별로 Sepal.Width의 평균 차이가 있는가? 만약 있다면 어느 품종과 차이가 있는가?

# 정규분포 여부
out <- aov(Sepal.Width ~ Species, data=iris)
shapiro.test(resid(out))

# 등분산 여부
bartlett.test(Sepal.Width ~ Species, data=iris)

#anova검정
summary(out)

# 사후 검정
TukeyHSD(out)

#### 실습2 ####
mydata <- read.csv("../data/anova_one_way.csv",fileEncoding = "CP949",encoding="UTF-8")
View(mydata)

# 주제 : 시, 군, 구에 따라서 합계 출산율의 차이가 있는가? 있다면 어느것과 차이가 있는가?

# 정규분포 여부
out <- aov(birth_rate ~ ad_layer, data=mydata)
shapiro.test(resid(out))

# 비모수적인 방식
kruskal.test(birth_rate ~ ad_layer, data=myadta)

# 모수적인 방식
summary(out)

# 그래프로 확인
moonBook::densityplot(birth_rate ~ ad_layer, data=mydata)

# kruskal 일 경우 사후 검정
kruskalmc(mydata$birth_rate, mydata$ad_layer)

# aov일 경우 사후 검정
TukeyHSD(out)

#### 실습3 ####
# 실습 데이터 : https://www.kaggle.com
library(dplyr)

telco <- read.csv("../data/Telco-Customer-Churn.csv",header = T)
View(telco)
str(telco)
table(telco$PaymentMethod)
unique(telco$PaymentMethod)

# 독립변수 : PaymentMethod
# 종속변수 : TotalCharges

# 주제 : 지불 방식별로 총 지불금액이 차이가 있는가? 있다면 무엇과 차이가 있는가?

# 각 지불 방식별로 갯수(인원수)와 평균 금액을 조회
telco1 <- telco %>% select(PaymentMethod,TotalCharges) %>% group_by(PaymentMethod) %>% summarise(count = n(), mean=mean(TotalCharges,na.rm=T))

# 그래프 확인
moonBook::densityplot(TotalCharges ~ PaymentMethod, data=telco)

# 정규분포 테스트
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Bank transfer (automatic)"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Credit card (automatic)"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Electronic check"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Mailed check"]))

out <- aov(TotalCharges ~ PaymentMethod, data=telco)
shapiro.test(resid(out))

# 앤더슨 달링 테스트

# 등분산 검정
bartlett.test(TotalCharges ~ PaymentMethod, data=telco)

# welch's anova
oneway.test(TotalCharges ~ PaymentMethod, data=telco, var.equal = F)

# 사후 검정
library(nparcomp)
result <- mctp(TotalCharges ~ PaymentMethod, data=telco)
summary(result)

plot(result)

# 만약 정규분포가 아니라는 상황에서 테스트를 한다면?

