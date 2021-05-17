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
bartlett.test(TotalCharges ~ PaymentMethod, data=telco) # 차이가 있기 때문에 사후 검정이 필요

# welch's anova
oneway.test(TotalCharges ~ PaymentMethod, data=telco, var.equal = F) # var.equal : 등분산 여부

# 사후 검정
library(nparcomp)
result <- mctp(TotalCharges ~ PaymentMethod, data=telco)
summary(result)

plot(result)

# 만약 정규분포가 아니라는 상황에서 테스트를 한다면?
kruskal.test(TotalCharges ~ PaymentMethod, data=telco)
kruskalmc(telco$TotalCharges, telco$PaymentMethod)

library(ggplot2)
ggplot(telco,aes(telco$PaymentMethod,telco$TotalCharges)) + geom_boxplot() 



###########################################################################################################3

#### 사례2: Two Way ANOVA ####
mydata <- read.csv("../data/anova_two_way.csv",fileEncodin="CP949",encoding = "UTF-8")
View(mydata)
str(mydata)

# 정규분포 여부
out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, data=mydata) # 독립변수가 두개 이상일때 + 로 이어준다.
shapiro.test(resid(out)) # 정규분포 Fail
summary(out)

# 시, 구,군 별로 사후검정
result <- TukeyHSD(out)
result

# 결론 : 군에 채택 되었을때 그 차이가 있었다.
plot(result)
ggplot(mydata, aes(birth_rate, ad_layer, col=multichild)) + geom_boxplot() # 다자녀는 색깔로 표시(좌표는 두개밖에 못주기 때문)


#### 실습1 ####
telco <- read.csv("../data/Telco-Customer-Churn.csv",header = T)
str(telco)
c <- data.frame(telco$TotalCharges,telco$PaymentMethod,telco$Contract)
View(c)

# contract : 몇개월 단위로 끊었는지 알고 싶다
# 원인 변수 : PaymentMethod, Contract
# 결과 변수 : TotalCharges

############################################################################################################

#### 사례3 : RM ANOVA(two way repeated measured anova) ####
# 구형성(Sphericity) : 이미 독립성이 깨졌으므로 최대한 독립성과 무작위성을 확보하기위한 조건
# 변수들과의 관계는 독립성을 보장받아야 한다.
# 가정 : 반복 측정된 자료들의 시차에 따른 분산이 동일
#   1) Mouchly의 단위행렬 검정 : p-value값이 0.05보다 커야 함.
#   2) 만약 0.05보다 작다면 이 때 사용하는 이차적 방식으로 Greenhouse를 사용하여 검정 : 값이 1에 가까울수록 구형성이 타당하다.

# 6명을 대상으로 운동능력을 테스트한 결과변수 : pre, arter3m, after6m

df <- data.frame(id=c(1,2,3,4,5,6),pre=c(45,42,36,39,51,44),after3m=c(50,42,41,35,55,49),after6m=c(55,45,43,40,59,56))
df

# 모아서 평균
means <- c(mean(df$pre), mean(df$after3m), mean(df$after6m))
means
plot(means, type="o", lty=2, col="purple")

multimodel <- lm(cbind(df$pre,df$after3m,df$after6m) ~ 1) # 독립적이 아닌 전체적인 결과를 뽑아내기 위한 선형분석
multimodel
trials <- factor(c("pre","after3m","after6m"),ordered=F) # 그룹으로 묶어서 처리하기 위한 목적
trials

# anova 설치
install.packages("car")
library(car)

?Anova
model <- Anova(multimodel, idata =data.frame(trials),idesign = ~ trials, type = "II")
model
summary(model, multivariate=F)

# 전체집단 평균 -> 개인간의 평균

### 사후 검정(Anova를 쓰면 복잡해짐) : aov를 사용하면 편함(즉 처음부터 준비)

# data를 long형으로 바꿔줘야함
library(reshape2)

df2 <- melt(df, id.vars = "id")
df2

colnames(df2) <- c("id","time","value")
df2

str(df2)
df2$id = factor(df2$id)
str(df2)

# 개인별로 운동능력변화를 그래프로 출력
df2.mean <- df2 %>% group_by(time) %>% summarise(mean=mean(value),sd = sd(value)) 
df2.mean
ggplot(df2, aes(time,value)) + geom_line(aes(group=id,col=id)) + geom_point(aes(col=id))

# 6명의 평균값
ggplot(df2.mean, aes(time,mean))+ geom_point() + geom_line(group=1)

out <- aov(value ~ time +  Error(id/time), data=df2)
summary(out)

# 사후 검정
with(df2, pairwise.t.test(value, time, paired=T, p.adjust.method = "bonferroni")) # bonferroni : 오차값 계산 및 제거

# 전단계에서 3개월은 차이 x 
# 전단계에서 6개월은 차이 o
# 3개월에서 6개월은 차이 o

#### 실습1 ####
### 주제 : 7명의 학생이 총 4번의 시험을 보았다. 평균 차이가 있는가? 있다면 어느것과 차이가 있는가?

rm <- read.csv("../data/onewaySample.csv", header = T)
rm
View(rm)

rm <- rm[, 2:6]
rm

means <- c(mean(rm$score0),mean(rm$score1),mean(rm$score3),mean(rm$score6))
means
plot(means, type="o", lty=2, col=2)

multimodels <- lm(cbind(rm$score0, rm$score1, rm$score3, rm$score6) ~ 1)
trials <- factor(c("score0","score1","score3","score6"),ordered = F)

model1 <- Anova(multimodels, idata= data.frame(trials), idesign = ~trials, type="III")
summary(model1, multivariate = F)

# 사후 검정 
rmlong <- gather(rm, key="ID",value = "score")
summary(rmlong)

rmlong <- rmlong[8:35, ]
rmlong

out <- aov(score ~ ID, data=rmlong)
shapiro.test(resid(out))

summary(out)

# 사후 검정1
with(rmlong, pairwise.t.test(score, ID, paired = T, p.adjust.method = "bonferroni"))

# 사후 검정2
TukeyHSD(out)
0.05/4
ggplot(rm,aes(multimodels,trials)) + geom_point() + geom_line()


#### 실습2 : 비모수일 경우(서열변수이거나 정규분포가 아닐경우) #### 
# Paired sample t-test : Wilcoxen signed rank test
# rm anova : Friedman Test

?friedman.test
RoundingTimes <-
  matrix(c(5.40, 5.50, 5.55,
           5.85, 5.70, 5.75,
           5.20, 5.60, 5.50,
           5.55, 5.50, 5.40,
           5.90, 5.85, 5.70,
           5.45, 5.55, 5.60,
           5.40, 5.40, 5.35,
           5.45, 5.50, 5.35,
           5.25, 5.15, 5.00,
           5.85, 5.80, 5.70,
           5.25, 5.20, 5.10,
           5.65, 5.55, 5.45,
           5.60, 5.35, 5.45,
           5.05, 5.00, 4.95,
           5.50, 5.50, 5.40,
           5.45, 5.55, 5.50,
           5.55, 5.55, 5.35,
           5.45, 5.50, 5.55,
           5.50, 5.45, 5.25,
           5.65, 5.60, 5.40,
           5.70, 5.65, 5.55,
           6.30, 6.30, 6.25),
         nrow = 22,
         byrow = TRUE,
         dimnames = list(1 : 22,
                         c("Round Out", "Narrow Angle", "Wide Angle")))
RoundingTimes
summary(RoundingTimes)
str(RoundingTimes)

# long형 변환
rt <- melt(RoundingTimes)
rt

# 정규분포 확인
out <- aov(value ~ Var2, data=rt)
shapiro.test(resid(out))

boxplot(value ~ Var2, data=rt)

friedman.test(RoundingTimes)

# 사후 검정

friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # formu is a formula of the shape:     Y ~ X | block
  # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
  # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")
    library(coin)
  }
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]    # In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,    ### all pairwise comparisons
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")    # this is the post hoc of the friedman test
      # plotting
      if(to.plot.parallel & to.plot.boxplot)    par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)    # adding some spacing from both sides of the plot
        if(color.blocks.in.cor.plot)
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  ,
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide")     #[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))    # adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
      }
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}
      return(list.to.return)
    }    else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }
  }
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}

friedman.test.with.post.hoc(value ~ Var2 | Var1, rt)

0.05/3

############################################################################################################

#### 사례4 : Two Way RM ANOVA  ####

df <- read.csv("../data/10_rmanova.csv",header = T)
View(df)

### 데이터를 long형으로 변환
library(reshape2)

# 첫번째 방법(melt)
df1 <- melt(df, id=c("group","id"),variable.name="time",value.name = "month")
df1

# 두번째 방법(reshape)
df2 <- reshape(df,direction = "long", varying = 3:6, sep="")
df2

## 그래프 그리기
?interaction.plot

# 인자를 넘겨주기 위해 factor형으로 변환
df1$group <- factor(df1$group)
df1$id <- factor(df1$id)
str(df1)

interaction.plot(df1$time, df1$group, df1$month)

out <- aov(month ~ group*time + Error(id), data=df1)
summary(out)

# 차이가 있으니 사후검정

# 각자의 그룹을 시점별로 분석하기 위해 데이터를 시점별로 쪼갠다.
df_0 <- df1[df1$time == "month0" ,]
df_1 <- df1[df1$time == "month1" ,]
df_3 <- df1[df1$time == "month3" ,]
df_6 <- df1[df1$time == "month6" ,]

t.test(month ~ group, data=df_0)
t.test(month ~ group, data=df_1)
t.test(month ~ group, data=df_3)
t.test(month ~ group, data=df_6)

0.05 / 6




