CIA <- read.delim("./CIA.txt")
attach(CIA)

l_cons <- log(cons)
l_assets <- log(assets)
l_inc <- log(inc)

model <- lm(l_cons ~  l_inc + l_assets)
summary(model)

qf(0.95,2,7)

#tikrina porines regresijas
summary(lm(l_cons ~ l_inc))
summary(lm(l_cons ~ l_assets))

#tikrinima koreliacijas
cor(cbind(l_cons, l_inc, l_assets))

#pagalbines regresijos
aux <- lm(l_inc ~ l_assets)
summary(aux)

aux0 <- lm(l_inc ~ l_assets)
summary(aux0)

summary(aux)$r.square
summary(aux0)$r.square
summary(model)$r.squared

cor(l_inc,l_assets)^2

#VIF

VIF <- 1/(1-summary(aux)$r.square)

library(car)
vif(model)

#bukles indeksas

#konstruojam matrica
Xr <- cbind(l_inc,l_assets)

ev <- eigen(t(Xr)%*%Xr)$values

#bukles skaicius
ev[1]/ev[2]
#bukles indeksas
(ev[1]/ev[2])^0.5

library(perturb)

colldiag(model, scale = FALSE, center = FALSE, add.intercept = FALSE)

####################################################

RD <- read.delim("./RD.txt")
attach(RD)

m1 <- lm(rd ~ sales + profits)
summary(m1)

m1a <-  lm(rd ~ sales)
summary(m1a)

m1b <-  lm(rd ~ profits)
summary(m1b)

vif(m1)

colldiag(m1,scale=F,center=F,add.intercept = F)

#transformuojam

m2 <- lm(I(rd/sales)~I(1/sales)+I(profits/sales))
summary(m2)
#m2 netinka

m3 <- lm(I(rd/profits)~I(1/profits)+I(sales/profits))
summary(m3)
#m3 modelis statistiskai reiksmingas


m3a <- lm(rd ~ sales + profits, weights = profits^-2)
summary(m3a)

################################3

lt_lm <- read.delim("./lt_lm.txt")
attach(lt_lm)

w <- log(W)*100
cpi <- log(CPI)*100
ppi <- log(PPI)*100

ts.plot(U)

mod_lm <- lm(W ~ cpi + ppi + U)
summary(mod_lm)

vif(mod_lm)

colldiag(mod_lm,scale=F,center=F,add.intercept = F)

mod_lm0 <- lm(W ~ cpi + U)
summary(mod_lm0)

##############3

house <- read.delim("house.txt")
attach(house)
#standartizuojam busto kainas
(hp - mean(hp))/sd(hp)
#arba
hp_s <- scale(hp)
crime_s <- scale(crime)
nitro_S <- scale(nitro)
#be laisvbojo nario - 1 
mod_hp0 <- lm(hp_s ~ crime_s + nitro_S - 1)
summary(mod_hp0)

#itraukiama roomsdev
rooms_dev <- rooms - mean(rooms)
mod_hp1 <- lm(hp_s ~ crime_s + nitro_S + rooms_dev - 1)
summary(mod_hp1)

############333
###NAMU DARBAI


modelis <- lm(ppi ~ cpi + w + U)
summary(modelis)
#porineje siejasi, darbo uzmokestis priklauso nuo ppi
summary(lm(ppi~w))
#visu trendai yra labai panasus
plot.ts(cbind(ppi,w,cpi,U))
#porineje nedarbas reiksmingas
summary(lm(ppi~U))
#multikolinearumas yra ten kur vif didesnis nei 10
#cpi vifas didesnis, todel jis labiausiai interkoreliuojantis
vif(modelis)

colldiag(modelis,scale=F,center=F,add.intercept = F)

modelis2 <- lm(ppi ~ cpi + U)
summary(modelis2)

#mazu imciu kritine t statistika yra didesne
qt(0.975,9)
#nedarbas nereiksmingas su 95 proc pasikliovimo lygmeniu,
#bet su 90 jau reiksmingas (taskas salia)
# todel nedarbas yra irgi reiksmingas


###############

ceec <- read.delim("ceec.txt")
attach(ceec)

GDP_CZ <- ts(GDP_CZ, start = c(1998, 2), frequency = 4)
GFCF_CZ <- ts(GFCF_CZ, start = c(1998, 2), frequency = 4)
EMPL_CZ <- ts(EMPL_CZ, start = c(1998, 2), frequency = 4)
gdp_cz <- log(GDP_CZ)
gfcf_cz <- log(GFCF_CZ)
empl_cz_su <- log(EMPL_CZ)
ts.plot(gdp_cz)
ts.plot(gfcf_cz)
ts.plot(empl_cz_su)
empl_cz <- empl_cz_su - stl(empl_cz_su, s.window = 5)$time.series[, 1]
lines(empl_cz, col = "blue")

mod_ur_cz <- lm(gdp_cz ~ gfcf_cz + empl_cz)
summary(mod_ur_cz)
#pagal t statistikas kaip ir nematome multikolinearumi
#vif multikolinearumo nematome
vif(mod_ur_cz)
#pamate koreliacijoje kad ktarp nepriklausomu kintamuju koreliacija yra silpnesne
#nei tarp priklausomu ir nepriklausomu

cor(cbind(gdp_cz,gfcf_cz,empl_cz))
#o bukles indeksas rodo multikolinearuma
#bet ji mums nerupi, nes nesukelia problemu dideleje imtyje
#todel nereikia multikolinearumo spresti
colldiag(mod_ur_cz,scale=F,center=F,add.intercept = F)
###################################
GDP_HU <- ts(GDP_HU, start = c(1998, 2), frequency = 4)
GFCF_HU <- ts(GFCF_HU, start = c(1998, 2), frequency = 4)
EMPL_HU <- ts(EMPL_HU, start = c(1998, 2), frequency = 4)
gdp_hu <- log(GDP_HU)
gfcf_hu <- log(GFCF_HU)
empl_hu_su <- log(EMPL_HU)
ts.plot(gdp_hu)
ts.plot(gfcf_hu)
ts.plot(empl_hu_su)
empl_hu <- empl_hu_su - stl(empl_hu_su, s.window = 5)$time.series[, 1]
ts.plot(empl_hu_su)
lines(empl_hu, col = "blue")
mod_ur_hu <- lm(gdp_hu ~ gfcf_hu + empl_hu)
summary(mod_ur_hu)
#uzimtumas nereiksmingas

#vifas multikolinearumo nerodo
vif(mod_ur_hu)
#rysys tarp uzimtumo ir investiciju yra stipresnis nei tarp bvp ir uzimtumo
cor(cbind(gdp_hu,gfcf_hu,empl_hu))

#multikolinearumas yra pagal colldiag
colldiag(mod_ur_hu,scale=F,center=F,add.intercept = F)

#tada pasiimam apribota modeli
gdp_pw_hu <- gdp_hu - empl_hu
gfcf_pw_hu <- gfcf_hu - empl_hu
mod_ur_hu2 <- lm(gdp_pw_hu ~ gfcf_pw_hu + empl_hu)
summary(mod_ur_hu2)
mod_r_hu <- lm(gdp_pw_hu ~ gfcf_pw_hu)
summary(mod_r_hu)
lht(mod_ur_hu, "gfcf_hu + empl_hu = 1")
#bet cia porine regresija, todel multikolinearumo tikrinti nereikia

#########################################

CM <- read.delim("./CM.txt")
attach(CM)
cmort_s <- scale(cmort)
gdp_s <- scale(gdp)
flr_s <- scale(flr)
fr_s <- scale(fr)

summary(lm(cmort_s ~ gdp_s + flr_s + fr_s - 1))

#jei bvp padidetu vienu standartiniu nuokrypiu, tai lfr sumazetu 0.6 standartiniais nuokrypiais

