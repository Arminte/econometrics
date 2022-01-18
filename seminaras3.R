TEACHERS2 <- read.delim("C:/Users/globy/Desktop/ekonometrija/seminaras_03/TEACHERS2.txt")
attach(TEACHERS2)
# w=f(regionas)

mod_educ <- lm(Wage~North+South)
summary(mod_educ)

# Pagal mat. viltį šiauriniuose regionuose uždirba:
coef(mod_educ)[1] + coef(mod_educ)[2]
# Pagal mat. viltį pietiniuose regionuose uždirba:
coef(mod_educ)[1] + coef(mod_educ)[3]

#itraukiame outlay i regresijos modelį

mod_educ2 <- lm(Wage~North+South+Outlay)
summary(mod_educ2)

##########

cps <- read.delim("C:/Users/globy/Desktop/ekonometrija/seminaras_03/cps.txt")
attach(cps)

l_wage <- log(wage)*100
exper2 <- exper^2
tenure2 <- tenure^2
educ2 <- educ^2

#ziurim kurio t value modulis maziausias, ir P didziaudias ir tuos ismetam it toliau tikrinam
#pasiemam reiksmigiausia female ir patikrinam jo saveikas su kitais kintamaisiais
mod_w <- lm(l_wage~educ2+exper+exper2+tenure+
    I(female*exper)+I(female*exper2)+I(female*tenure)+
     female+urban+north+south+
     trade+services+
     prof+serv)

summary(mod_w)


############3

romania <- read.delim("C:/Users/globy/Desktop/ekonometrija/seminaras_03/romania.txt")
attach(romania)

Y <- ts(GDP_RO,start=c(2002,1),frequency = 4)
K <- ts(GFCF_RO,start=c(2002,1),frequency = 4)
L <- ts(EMPL_RO,start=c(2002,1),frequency = 4)
y <- log(Y)
k <- log(K)
l_su <- log(L)
#grafike matom kad yra sezoniskumas
ts.plot(l_su)
#pasalinam sezoniskuma is empl
l <- l_su - stl(l_su,s.window=5)$time.series[,1]
lines(l,col="blue")
#bvp tenkantis vienam dirbanciam asmeniui
y_pw <- y - l
#investicijos tenkanciam vienam dirbanciam asmeniui
k_pw <- k - l
mod <- lm(y_pw ~ k_pw)
summary(mod)
 
#CHOC

y_pw_pre <- window(y_pw,end=c(2008,4))
k_pw_pre <- window(k_pw,end=c(2008,4))
y_pw_post <- window(y_pw,start=c(2009,1))
k_pw_post <- window(k_pw,start=c(2009,1))

mod_pre <- lm(y_pw_pre~k_pw_pre)
summary(mod_pre)

mod_post <- lm(y_pw_post~k_pw_post)
summary(mod_post)

#randam F stat

obs <- length(y_pw)

rss_r <- deviance(mod)
rss_ur <- deviance(mod_pre)+deviance(mod_post)

k <- length(coef(mod))
((rss_r - rss_ur)/k)/(rss_ur/(obs-2*k))
qf(0.95,k,obs-2*k)

#Kadangi F didesne uz F kritine, atmetam nuline hipoteze

#automatizuota CHOW test

library(strucchange)

tau <- length(y_pw_pre)
sctest(y_pw~k_pw, type = "Chow", point = tau/obs)
#sukuriam fiktyvu kintamaji
crisis <- c(rep(0,tau),rep(1,obs-tau))

mod2 <- lm(y_pw ~ crisis + k_pw + I(k_pw*crisis))
summary(mod2)

coef(mod2)[1]+coef(mod2)[2]
coef(mod2)[3]+coef(mod2)[4]
