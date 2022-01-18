ceec <- read.delim("C:/Users/globy/Desktop/ekonometrija/seminaras_02/ceec.txt")
attach(ceec)
#ts suteikia laiko eilutes pozymius, nuo 1998 antro ketvircio, ketvirtiniai duomenys 

#CEKIJA

GDP_CZ <- ts(GDP_CZ,start = c(1998,2),frequency = 4)
GFCF_CZ <- ts(GFCF_CZ,start = c(1998,2),frequency = 4)
EMPL_CZ <- ts(EMPL_CZ,start = c(1998,2),frequency = 4)
gdp_cz <- log(GDP_CZ)
gfcf_cz <- log(GFCF_CZ)
empl_su_cz <- log(EMPL_CZ)
ts.plot(gdp_cz)
ts.plot(gfcf_cz)
plot.ts(cbind(gdp_cz,gfcf_cz))
ts.plot(empl_su_cz)
# nuimam sezoniskuma
empl_cz <- empl_su_cz - stl(empl_su_cz,s.window = 5)$time.series[,1]
lines(empl_cz,col="blue")
mod_ur_cz <- lm(gdp_cz~gfcf_cz+empl_cz)
summary(mod_ur_cz)
length(gdp_cz)
qt(0.975,84)
#antlogaritmuojame
exp(-4.77358)

b2 <- coef(mod_ur_cz)[2]
b3 <- coef(mod_ur_cz)[3]

varb2 <- diag(vcov(mod_ur_cz))[2]
varb3 <- diag(vcov(mod_ur_cz))[3]

covb2b3 <- vcov(mod_ur_cz)[2,3]

seb2b3 <- (varb2+2*covb2b3+varb3)^0.5

t <- (b2+b3-1)/seb2b3

gdp_pw_cz <- gdp_cz-empl_cz
gfcf_pw_cz <- gfcf_cz-empl_cz

mod_ur_cz2 <- lm(gdp_pw_cz~gfcf_pw_cz+empl_cz)
summary(mod_ur_cz2)$coefficients

b2+b3-1

mod_r_cz <- lm(gdp_pw_cz~gfcf_pw_cz)
summary(mod_r_cz)

deviance(mod_r_cz)
deviance(mod_ur_cz)
rss_r <- anova(mod_r_cz)[2,2]
rss_ur <- anova(mod_ur_cz)[3,2]
s2_ur <- anova(mod_ur_cz)[3,3]

#f statistika
(rss_r - rss_ur/s2_ur)
#kritine f statistika
qf(0.95,1,84)

#musu f statistika yra didesne uz kritine f statistika, todel nuline hipoteze, kad egzistuoja
#pastovi masto graza, yra atmesta

#trumpesnis budas palyginti apribota ir neapribota modeli
library(car)
#pirmas argumentas yra neapribotas modelis, antras apiribojimas b1+b2=1
lht(mod_ur_cz,"gfcf_cz + empl_cz =1")

#t statistika kvadratu yra f statistika

#Venrija

ceec <- read.delim("C:/Users/globy/Desktop/ekonometrija/seminaras_02/ceec.txt")
attach(ceec)
#ts suteikia laiko eilutes pozymius, nuo 1998 antro ketvircio, ketvirtiniai duomenys 
GDP_HU <- ts(GDP_HU,start = c(1998,2),frequency = 4)
GFCF_HU <- ts(GFCF_HU,start = c(1998,2),frequency = 4)
EMPL_HU <- ts(EMPL_HU,start = c(1998,2),frequency = 4)
gdp_hu <- log(GDP_HU)
gfcf_hu <- log(GFCF_HU)
empl_su_hu <- log(EMPL_HU)
ts.plot(gdp_hu)
ts.plot(gfcf_hu)
plot.ts(cbind(gdp_hu,gfcf_hu))
ts.plot(empl_su_hu)
# nuimam sezoniskuma
empl_hu <- empl_su_hu - stl(empl_su_hu,s.window = 5)$time.series[,1]
lines(empl_hu,col="blue")
mod_ur_hu <- lm(gdp_hu~gfcf_hu+empl_hu)
cor(cbind(gdp_hu,gfcf_hu,empl_hu))
summary(mod_ur_hu)

qt(0.975,84)

gdp_pw_hu <- gdp_hu-empl_hu
gfcf_pw_hu <- gfcf_hu-empl_hu

mod_ur_hu2 <- lm(gdp_pw_hu~gfcf_pw_hu+empl_hu)
summary(mod_ur_hu2)

lht(mod_ur_hu,"gfcf_hu + empl_hu =1")
#musu apskaiciuotas f yra mazesni ir f kritini, todel nulines hipotezes, teigiancios jog neapriboto modelio paaiskinimo galimybes niekuo nesiskiria  nei apriboto, yra atmetama
# todel reikia ivertiniti apribota modeli

mod_r_hu <- lm(gdp_pw_hu~gfcf_pw_hu)
summary(mod_r_hu)

#paklaidos
res_r_hu <- residuals(mod_r_hu)
beta1 <- coef(mod_r_hu)[1]
beta2 <- coef(mod_r_hu)[2]
MPL_HU <- (1 - beta2)*exp(beta1)*GFCF_HU^beta2*EMPL_HU^-beta2*exp(res_r_hu)
ts.plot(MPL_HU)

TFP <- exp(beta1 + res_r_hu)
ts.plot(TFP)


#LENKIJA

GDP_PL <- ts(GDP_PL,start = c(1998,2),frequency = 4)
GFCF_PL <- ts(GFCF_PL,start = c(1998,2),frequency = 4)
EMPL_PL <- ts(EMPL_PL,start = c(1998,2),frequency = 4)
gdp_pl <- log(GDP_PL)
gfcf_pl <- log(GFCF_PL)
empl_su_pl <- log(EMPL_PL)
ts.plot(gdp_pl)
ts.plot(gfcf_pl)
plot.ts(cbind(gdp_pl,gfcf_pl))
ts.plot(empl_su_pl)
# nuimam sezoniskuma
empl_pl <- empl_su_pl - stl(empl_su_pl,s.window = 5)$time.series[,1]
lines(empl_pl,col="blue")
mod_ur_pl <- lm(gdp_pl~gfcf_pl+empl_pl)
summary(mod_ur_pl)
length(gdp_pl)
qt(0.975,84)
#antlogaritmuojame
exp(-4.77358)

b2 <- coef(mod_ur_pl)[2]
b3 <- coef(mod_ur_pl)[3]

varb2 <- diag(vcov(mod_ur_pl))[2]
varb3 <- diag(vcov(mod_ur_pl))[3]

covb2b3 <- vcov(mod_ur_pl)[2,3]

seb2b3 <- (varb2+2*covb2b3+varb3)^0.5

t <- (b2+b3-1)/seb2b3

gdp_pw_pl <- gdp_pl-empl_pl
gfcf_pw_pl <- gfcf_pl-empl_pl

mod_ur_pl2 <- lm(gdp_pw_pl~gfcf_pw_pl+empl_pl)
summary(mod_ur_pl2)$coefficients

b2+b3-1

mod_r_pl <- lm(gdp_pw_pl~gfcf_pw_pl)
summary(mod_r_pl)

deviance(mod_r_pl)
deviance(mod_ur_pl)
rss_r <- anova(mod_r_pl)[2,2]
rss_ur <- anova(mod_ur_pl)[3,2]
s2_ur <- anova(mod_ur_pl)[3,3]

#f statistika
(rss_r - rss_ur/s2_ur)
#kritine f statistika
qf(0.95,1,84)

#musu f statistika yra didesne uz kritine f statistika, todel nuline hipoteze, kad egzistuoja
#pastovi masto graza, yra atmesta

#trumpesnis budas palyginti apribota ir neapribota modeli
library(car)
#pirmas argumentas yra neapribotas modelis, antras apiribojimas b1+b2=1
lht(mod_ur_pl,"gfcf_pl + empl_pl =1")

#t statistika kvadratu yra f statistika

\