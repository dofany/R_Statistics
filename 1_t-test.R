#### Power Analysis ####
# 적정한 표본의 갯수를 산출
# cohen's d (effective size) : 두 집단의 평균 차이를 두 집단의 표준편차의 합으로 나눠준다.

ky <- read.csv("../data/KY.csv",fileEncoding = "CP949", encoding="UTF-8")
View(ky)

table(ky$group)
