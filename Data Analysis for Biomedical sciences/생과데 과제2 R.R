# 4.1
jail_yes=c(42,17,33,53)
jail_no=c(109,75,175,359)
live=c('n','n','y','y')
arrest=c('y','n','y','n')
df4_1=data.frame(live,arrest,jail_yes,jail_no)
attach(df4_1)
live_n=ifelse(live=='n',1,0)
arrest_n=ifelse(arrest=='n',1,0)
n=jail_yes+jail_no
prop.yes=jail_yes/n

## (1)
fit4_1=glm(prop.yes~live_n+arrest_n,binomial(link=logit),weights=n)
summary(fit4_1)

## (2)
exp(cbind(OR=coef(fit4_1),confint(fit4_1)))
# 4.1
jail_yes=c(42,17,33,53)
jail_no=c(109,75,175,359)
live=c('n','n','y','y')
arrest=c('y','n','y','n')
df4_1=data.frame(live,arrest,jail_yes,jail_no)
attach(df4_1)
live_n=ifelse(live=='n',1,0)
arrest_n=ifelse(arrest=='n',1,0)
n=jail_yes+jail_no
prop.yes=jail_yes/n

## (1)
fit4_1=glm(prop.yes~live_n+arrest_n,binomial(link=logit),weights=n)
summary(fit4_1)

## (2)
exp(cbind(OR=coef(fit4_1),confint(fit4_1)))

# 4.5
disability=c(rep(4,12),rep(3,12),rep(2,7),rep(1,9))
status=c(1,1,1,1,0,1,0,1,1,1,0,0,1,0,1,0,1,1,0,1,1,0,1,1,0,1,0,0,1,0,0,1,1,1,0,0,0,1,0,0)
event=c(1,9,4,3,2,0,1,3,3,7,1,2,5,6,3,1,8,2,5,5,9,3,3,1,0,4,3,9,6,4,3,8,2,7,5,4,4,8,8,9)
df4_5=data.frame(disability,status,event)
attach(df4_5)
status_f=ifelse(status==0,1,0)

install.packages('ordinal')
library(ordinal)
fit4_5=clm(factor(disability)~status_f+event,data=df4_5)
summary(fit4_5)

# 4.7
# (1)
q4_7=array(c(659,432,532,269,270,532,347,552),dim=c(2,2,2),dimnames=list(belt=c('y','n'),out=c('y','n'),dead=c('y','n')))
q4_7

library(MASS)
fit4_7=loglm(~belt*out*dead,data=q4_7)
step(fit4_7,direction='backward')
## (2)
dead_yes=c(659,532,432,269);dead_no=c(270,347,532,552)
belt=c('y','y','n','n');out=c('y','n','y','n')
df4_7=data.frame(belt,out,dead_yes,dead_no)
attach(df4_7)
belt_n=ifelse(belt=='n',1,0)
out_n=ifelse(out=='n',1,0)
n=dead_yes+dead_no
prop.yes=dead_yes/n

fit4_7=glm(prop.yes~belt_n+out_n,binomial(link=logit),weight=n)
summary(fit4_7)

# 4.9
# (1)
q4_9_1=array(c(34, 29, 40, 37, 24, 6, 3,49, 31, 37, 32, 11, 0, 4,64, 61, 81, 65, 40, 15, 7,135, 118, 142, 64, 37, 3, 2), dim = c(7, 2, 2),dimnames = list(time=c('없다','0~30분','30분~1시간','1~2시간','2~4시간','4~6시간','6시간 이상'),gender=c("male", "female"),school=c('인문계','실업계')))
q4_9_1

library(MASS)
fit4_9=loglm(~time*gender*school,data=q4_9_1)
step(fit4_9,direction='backward') 

# 마지막 output에서 성별과 학교 계열의 교호작용, 시간과 성별간의 교호작용은 <none>행 밑에 있으므로 더이상 AIC값이 작아질 수 없어 가장 적절한 모형은 성별과 학교 계열의 교호작용, 시간과 성별간의 교호작용을 포함한 모델임을 알 수 있다.

# (2)
q4_9_2=array(c(103,61,9,206,105,22,117,43,4,395,101,5),dim=c(3,2,2),dimnames=list(time=c('1시간이하','1~4시간','4시간이상'),gender=c('male','female'),school=c('인문계','실업계')))
q4_9_2

fit4_9_2=loglm(~time*gender*school,data=q4_9_2)
step(fit4_9_2,direction = 'backward')

# 시간이 없다~ 1시간, 1~4시간, 4~6시간 이상을 범주로 7개의 범주로 나누어진 시간 변수를 3개의 범주로 나누었다. 마지막 output에서 성별과 학교 계열의 교호작용, 시간과 성별간의 교호작용은 <none>행 밑에 있으므로 더이상 AIC값이 작아질 수 없어 가장 적절한 모형은 성별과 학교 계열의 교호작용, 시간과 성별간의 교호작용을 포함한 모델임을 알 수 있다. 즉 (1)의 결과와 로그 선형 모형의 차이가 없다. 

# 5.2
X=c(29,4,18,17,35,3,1,15,32)
Y=c(39,34,36,35,38,32,38,43,44)
method=c(1,1,1,2,2,2,3,3,3)
df5_2=data.frame(method,X,Y)
attach(df5_2)

anova(lm(Y~X*as.factor(method),data=df5_2))

anova(lm(Y~X+as.factor(method),data=df5_2))

# 5.7
X1=c(99,95,99,99,102,101,102,103,102,103,102,104,106,106,107,108,109,111,110,113,95,97,100,100,102,105,102,102,105,105,106,109,107,108,111,108,111,113,114,114,94,95,99,99,99,102,99,101,104,101,104,107,107,106,109,107,109,111,111,112)
X2=c(12,11,13,13,12,12,12,13,13,14,14,13,15,13,13,12,13,13,13,13,12,12,13,13,13,12,13,14,12,13,14,13,14,14,14,13,13,14,13,14,12,12,13,11,14,12,13,13,13,14,15,13,13,14,14,14,14,14,14,13)
Y=c(58,55,59,55,60,57,57,61,58,59,62,59,60,62,60,62,64,63,59,63,58,55,60,55,60,58,57,62,58,61,63,59,61,64,61,63,65,64,62,60,59,56,62,57,61,60,58,63,60,63,65,61,62,64,61,65,63,66,62,64)
method=c(rep(1,20),rep(2,20),rep(3,20))
df5_7=data.frame(method,X1,X2,Y)
attach(df5_7)

anova(lm(Y~(X1+X2)*as.factor(method),data=df5_7))

anova(lm(Y~X1+X2+as.factor(method),data=df5_7))
      