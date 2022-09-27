library(ISLR)
attach(Carseats)
contrasts(Urban)
contrasts(US)
head(Carseats)
lm.fit <- lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.fit)

lm.fit2 <- lm(Sales~Price+US, data=Carseats)
summary(lm.fit2)
confint(lm.fit2)
lm.fit3 <- lm(Sales~Price*US, data=Carseats)
summary(lm.fit3)

vec=1:100
for(i in 1:100){x1 <- rnorm(500,30,9)
error=rnorm(500,0,16)
y1=10*x1+error
m1=lm(x1~y1-1)
k <- coefficients(m1)
print(k)
vec[i]<-k}
print(vec)
mean(vec)

vec=1:100
for(i in 1:100){x2 <- rnorm(500,80,120)
error2=rnorm(500,0,32)
y2=35*x2+error2
m2=lm(x2~y2-1)
k2 <- coefficients(m2)
vec[i]<-k2}
print(vec)
mean(vec)