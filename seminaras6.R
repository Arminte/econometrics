LDB <- read.delim("LDB.txt")
attach(LDB)

l.labour <- log(labour)
l.wage <- log(wage)
l.capital <- log(capital)
l.output <- log(output)

model <- lm(l.labour ~ l.wage + l.output + l.capital)
summary(model)
n <- length(l.labour)
Intercept <- rep(1,n)
X <- cbind(Intercept, l.wage, l.output, l.capital)
res <- residuals(model)
res2 <- res^2
#V yra sigma kvadratas kart omega
V <- diag(res2)
solve(t(X)%*%X)%*%t(X)%*%V%*%X%*%solve(t(X)%*%X)

#galima rasti ir automatizuotu budu
library(car)          
vcov_wh <- hccm(model,type = "hc0")
#issitraukiame pagrindine istrizaine ir gaunam nepaslinktas dispersijas,o kai istraukiam sakni gaunam nepaslinktas standartines paklaidas
diag(vcov_wh)^0.5
#saliname heteroskediskuma
coef(model)/diag(vcov_wh)^0.5
#automatizuotu budu
library(lmtest)
coeftest(model,vcov = vcov_wh)
#arba
coeftest(model,vcov = hccm(model,type = "hc0"))

#abipendrinto maziausiu kvadratu metodo

y <- l.labour
#parko pagalbine regresija
aux <- lm(log(res2)~ l.wage + l.output + l.capital)
summary(aux)
alpha <- coef(aux)[-1]
Z <- cbind(l.wage, l.output, l.capital)
h2 <- exp(Z%*%alpha)
omega <- diag(c(h2))
b.gls <- solve(t(X)%*%solve(omega)%*%X)%*%t(X)%*%solve(omega)%*%y

u.gls <- y - X%*%b.gls
k <- dim(X)[2]
s2 <- c(t(u.gls)%*%solve(omega)%*%u.gls/(n-k))

vcov.gls <- s2*solve(t(X)%*%solve(omega)%*%X)
seb.gls <- diag(vcov.gls)^0.5
t.gls <- b.gls/seb.gls
summ.gls <- cbind(b.gls,seb.gls,t.gls)
colnames(summ.gls) <- c("Estimate","Std.Error","t value")
summ.gls

#dabar pagal t value matom kad l.capital tapo reiksmingas

#automatizuotu budu
model.gls <- lm(l.labour ~ l.wage + l.output + l.capital, weights = h2^-1)
summary(model.gls)

#cia yra skerspjuvio duomenys, tose imonese kur atlyginimai yra aukstesni, ten dirbs maziau zmoniu(uzimtumas mazesnis)
#imoneje, kurioje aukstesnes gamybos apimtys, uzimtumas didesnis
# arba imoneje, kurioje 1 proc. punktu yra didesnes gamybos apimtys, uzimtumas padideja 1 proc punktu
#imoneje, kurioje investicijos yra didesnis, uzimtumas yra mazesnis 
#gamybos apimtys turi didziausia reiksme, nes t value didziausias