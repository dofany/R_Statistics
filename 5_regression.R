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

# 4가지 조건을 확인하기 위해 방법 
# Residuals vs Fitted : 선형선, Q-Q : 정규분포, Scale-Location : 등분선, Residuals vs Leverage : 이성치
plot(fit) 


