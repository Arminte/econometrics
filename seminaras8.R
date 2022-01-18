library(Hmisc)
library(lmtest)

pclt <- read.delim("pclt.txt")
attach(pclt)
# Kintam?j? apibr??imas
CPI <- ts(CPI, start = c(1998, 1), frequency = 4)
U <- ts(U, start = c(1998, 1), frequency = 4)
infl <- diff(log(CPI))*100
u <- window(U, start = c(1998, 2))
u2 <- u^2
# Pirminis Phillipso kreiv?s modelis
mod_pc <- lm(infl ~ u + u2)
summary(mod_pc)
res_pc <- residuals(mod_pc)
# Autokorelograma ir dalin? autokorelograma
acf(res_pc)
pacf(res_pc)

#itraukiam unfliacijos ketvirta ir septinta laga, ir pagal kryzmine autokorelograma pamatem kad turbut yra pirmos eiles nedarbo autokoreliacija
mod_pc1 <- lm(infl ~ Lag(u,1) + Lag(u2,1) + Lag(infl,4) + Lag(infl,7))
summary(mod_pc1)
res_pc1 <- residuals(mod_pc1)
# Autokorelograma ir dalin? autokorelograma
acf(res_pc1)
pacf(res_pc1)
#kartu 12 funkciju 12 lage nesukelia autokoreliacijos
bgtest(mod_pc1,12)

#nestacionaru, pasizymi augimo tendecija
ts.plot(log(CPI))
#o infliacija jau stacionari daugmaz, del to nereik peeitui iskrto i pirmuju skirtumu metoda
ts.plot(infl)
#nedarbas sparciai auga, ir letai leteja, kol sugrizta prie atraktoriaus(naturalaus nedarbo lygio)
ts.plot(U)

#kryzmine autokorelograma
ccf(c(infl),c(U))


coef(mod_pc1)[2] + 2*coef(mod_pc1)[3]*(4:18)
#jei nedarbo lygis isaugtu nuo 4 iki 5 proc, tai infliacijos tempas suletetu 0.46 proc punkto, prasidetu defliacija

#VARTOJIMO IR PAJAMU REGRESIJA

cons_wages <- read.delim("cons_wages.txt")
attach(cons_wages)
# Kintamieji
FCEH <- ts(FCEH, start = c(1998, 1), frequency = 4)
WS <- ts(WS, start = c(1998, 1), frequency = 4)
cons <- log(FCEH)
wages <- log(WS)
# Pirminis modelis
mod_cw1 <- lm(cons ~ wages)
summary(mod_cw1)
res_cw1 <- residuals(mod_cw1)
# Autokorelograma ir dalin? autokorelograma
acf(res_cw1)
pacf(res_cw1)

#aiskiai isreikstos augimo tendecijos
ts.plot(cons)
ts.plot(wages)

pacf(cons)
#panasu i pirmos eiles autoregresini procesa, todel itraukiam cons 1 laga

ccf(cons,wages)
#pagal kryzmine koreliacija matome irgi laipnini mazejima, bet is abieju pusiu, tai gali reiksti kad yra abieju koeficientu pirmos eiles autokoreliacija
#todel itraukiam ir wages pirma laga
mod_cw1 <- lm(cons ~ wages + Lag(cons,1) + Lag(wages,1))
summary(mod_cw1)
res_cw1 <- residuals(mod_cw1)
# Autokorelograma ir dalin? autokorelogra
acf(res_cw1)
pacf(res_cw1)

#matome kad labai daug kas autokoreliuoja, todel bandom itraukti ketvirtus lagus

mod_cw1 <- lm(cons ~ wages + Lag(cons,1) + Lag(wages,1 + Lag(cons,4) + Lag(wages,4))
summary(mod_cw1)
res_cw1 <- residuals(mod_cw1)
# Autokorelograma ir dalin? autokorelogra
acf(res_cw1)
pacf(res_cw1)

#matome, kad nereiksmingi koeficientai... netinka
mod_cw1 <- lm(cons ~ wages + Lag(cons,1) + Lag(cons,3) + Lag(wages,3) + Lag(cons,7) + Lag(wages,7))
              summary(mod_cw1)
              res_cw1 <- residuals(mod_cw1)
              # Autokorelograma ir dalin? autokorelogra
              acf(res_cw1)
              pacf(res_cw1)
              
bgtest(mod_cw1,12)

#alfu suma
0.60476+0.33656-0.11445

#diferencijuojam

dc <- diff(cons)
dw <- diff(wages)

acf(dc)
pacf(dc)
ccf(dc,dw)

mod_cw2 <- lm(dc ~ dw + Lag(dw,1) + Lag(dc,1) + Lag(dc,7) +Lag(dc,11)+Lag(dc,12))
summary(mod_cw2)
res_cw2 <-residuals(mod_cw2)

acf(res_cw2)
pacf(res_cw2)

#patikrinam ar statistikai suma yra mazesne uz viena
sum(coef(mod_cw2)[4:7])
sum(coef(mod_cw2)[2:3])

########33

ceec <- read.delim("ceec.txt")
attach(ceec)
# ?ekija
GDP_CZ <- ts(GDP_CZ, start = c(1998, 2), frequency = 4)
GFCF_CZ <- ts(GFCF_CZ, start = c(1998, 2), frequency = 4)
EMPL_CZ <- ts(EMPL_CZ, start = c(1998, 2), frequency = 4)
gdp_cz <- log(GDP_CZ)
gfcf_cz <- log(GFCF_CZ)
empl_cz_su <- log(EMPL_CZ)
empl_cz <- empl_cz_su - stl(empl_cz_su, s.window = 5)$time.series[, 1]
mod_cz <- lm(gdp_cz ~ gfcf_cz + empl_cz)
summary(mod_cz)
res_cz <- residuals(mod_cz)
acf(res_cz)
pacf(res_cz)

#issitraukiam alfa
r1 <- acf(res_cz)$acf[2]
gdp_cz_t <- gdp_cz - r1*Lag(gdp_cz,1)
gfcf_cz_t <- gfcf_cz - r1*Lag(gfcf_cz,1)
empl_cz_t <- empl_cz - r1*Lag(empl_cz,1)
intercept_t <- rep(1-r1,length(res_cz))

mod_cz_t <- lm(gdp_cz_t~intercept_t+gfcf_cz_t+empl_cz_t-1)
summary(mod_cz_t)
res_cz_t <- residuals(mod_cz_t)
acf(res_cz_t)
pacf(res_cz_t)

ar_res_cz <- lm(res_cz ~Lag(res_cz,1)-1)
summary(ar_res_cz)
eps_cz <- residuals(ar_res_cz)
acf(eps_cz)
pacf(eps_cz)
#paklaidos visgi nera baltasis triuksmas, nes autokoreliuoja

#pereinam prie pirmu skirtumu analizes

d_gdp_cz <- diff(gdp_cz)
d_gfcf_cz <- diff(gfcf_cz)
d_empl_cz <- diff(empl_cz)

mod_cz_fd <- lm(d_gdp_cz ~ d_gfcf_cz + d_empl_cz)
summary(mod_cz_fd)

acf(d_gdp_cz)
#matome laipsniskai gestancia
pacf(d_gdp_cz)
#matome pirmos eiles autokoreliacija
#todel ikeliam pirma laga bvp

ccf(d_gdp_cz,d_gfcf_cz)
#koreliauoja ir bvp ir gfcf augimo tempai, todel itraukiam ir gfcf 1 ir 2 laga
ccf(d_gdp_cz,d_empl_cz)

mod_cz_fd <- lm(d_gdp_cz ~ d_gfcf_cz + d_empl_cz +Lag(d_gdp_cz,1)+Lag(d_gfcf_cz,1)+Lag(d_gfcf_cz,2))
summary(mod_cz_fd)
res_cz_fd <- residuals(mod_cz_fd)
acf(res_cz_fd)
pacf(res_cz_fd)

#kagangi empl nereiksmingas ismetam, o kai ji ismetam ir antras gfcf lagas tampa nereiksmingas, ji ismetam

mod_cz_fd <- lm(d_gdp_cz ~ d_gfcf_cz +Lag(d_gdp_cz,1)+Lag(d_gfcf_cz,1))
summary(mod_cz_fd)
res_cz_fd <- residuals(mod_cz_fd)
acf(res_cz_fd)
pacf(res_cz_fd)

bgtest(mod_cz_fd,5)

sum(coef(mod_cz_fd)[2:3])

#######3

gdp_pw_cz <- gdp_cz - empl_cz
gfcf_pw_cz <- gfcf_cz - empl_cz

mod_crs_cz <- lm(gdp_pw_cz ~ gfcf_pw_cz + Lag(gdp_pw_cz,1) +Lag(gfcf_pw_cz,1))
summary(mod_crs_cz)
res_crs_cz <- residuals(mod_crs_cz)
acf(res_crs_cz)
pacf(res_crs_cz)
#pereinam i skirtumu metoda
d_gdp_pw_cz <- diff(res_cz_fd)
d_gfcf_pw_cz <- diff(res_cz_fd)

d_mod_crs_cz <- lm(d_gdp_pw_cz ~ d_gfcf_pw_cz + Lag(d_gdp_pw_cz,1) +Lag(d_gfcf_pw_cz,1))
summary(d_mod_crs_cz)
d_res_crs_cz <- residuals(d_mod_crs_cz)
acf(d_res_crs_cz)
pacf(res_crs_cz)
