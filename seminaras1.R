getwd()
setwd("C:/Users/globy/Desktop/ekonometrija/seminaras_01")
ceo <- read.delim("C:/Users/globy/Desktop/ekonometrija/seminaras_01/ceo.txt")
attach(ceo)
#sklaidos diagrama
l.salary <- log(salary)*100
l.sales <- log(sales)*100
plot(l.sales,l.salary)
plot(roe,l.salary)
plot(ros,l.salary)
y <- l.salary
n <- length(y)
intercept <- rep(1,n)
X <- cbind(intercept,l.sales,roe,ros)
b <- solve(t(X)%*%X)%*%t(X)%*%y
k <- ncol(X)
u <- y - X%*%b
s2 <- t(u)%*%u/(n-k)
vcovb <- c(s2)*solve(t(X)%*%X)
seb <- diag(vcovb)^0.5
tb <- b/seb
coef <- cbind(b,seb,tb)
colnames(coef) <- c("Estimate","Std.Error","t value")
coef

tss <- t(y)%*%y - n*mean(y)^2
ess <- t(b)%*%t(X)%*%X%*%b - n*mean(y)^2
R2 <- ess/tss
F <- (ess/(k-1))/s2

mod1 <- lm(l.salary~l.sales + roe +ros)
summary(mod1)
summary(mod1)$coefficients

#kritine statistika
qt(0.975,n-k)
qf(0.95,k-1,n-k)
#pasikliautini intervalai
confint(mod1)

#rps neturi reiksmes salary
mod2 <- lm(l.salary~l.sales + roe)
summary(mod2)
