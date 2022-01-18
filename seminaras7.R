pclt <- read.delim("pclt.txt")
attach(pclt)
#pasidarom is vektoriu laiko eilutes
CPI <- ts(CPI,start = c(1998,1),frequency = 4)
U <- ts(U,start = c(1998,1),frequency = 4)
#noredami gauti infliacija is kainu indeksu, logaritmuojam ir diferencijuojam
#o kad butu tokie patys vienetai kaip U,dauginam is 100
infl <- diff(log(CPI))*100
#kadangi dabar infliacija nuo 2 ketvircio, U irgi padarom nuo antro
u <- window(U,start=c(1998,2))
u2 <- u^2
modpc <- lm(infl~u+u2)
summary(modpc)
respc <- residuals(modpc)
#autokoreliacijos funkcija
acf(respc)
#daline autokoreliacijos funkcija
pacf(respc)
#durbino watsono
sum(diff(respc)^2)/sum(respc^2)
#automatizuotu
library(lmtest)
dwtest(modpc,alternative = "two.sided")
#breush
library(Hmisc)
aux_bg <- lm(respc~u+u2+Lag(respc,1)+Lag(respc,2)+Lag(respc,3)+Lag(respc,4))
summary(aux_bg)
#t-4 lago paklaidos koreliuoja 

summary(aux_bg)$r.squared*(length(respc)-4)
qchisq(0.95,4)
#gavome didesni daugikli nei qchisq, todelnuline hipoteze atmetam

bgtest(modpc,4)

cons_wages <- read.delim("cons_wages.txt")
attach(cons_wages)

cons <- log(FCEH)
wages <- log(WS)

modcv <- lm(cons~wages)
summary(modcv)

rescv <- residuals(modcv)
acf(rescv)
pacf(rescv)

