LDB <-read.delim("LDB.txt")
attach(LDB)

l.labour <- log(labour)
l.wage <- log(wage)
l.output <- log(output)
l.capital <- log(capital)

model <- lm(l.labour ~ l.wage + l.output + l.capital)
summary(model)

#Parko testas

res <- residuals(model)
res2 <- res^2

parkausx <- lm(log(res2) ~ l.wage + l.output + l.capital)
summary(parkausx)

#pagal t stat paklaidos koreliuoja su regresoriais output ir capital
#yra keteroskedestiskumas

#Gleizerio testas

glejsaux <- lm(abs(res) ~ l.wage + l.output + l.capital)
summary(glejsaux)

#pagal t stat koreliuoja paklaidos modulis su regresoriu captal
#f statistika didesne nei kritine, maza p reiksme, yra heteroskedestiskumas

#Goldfeld - Quant

#didele imtis, todel nesvarbu kiek centriniu stebejimu pasalinti

#rikiuojam didejimo tvarka pagal wage

n <- length(res)
lLDB <- data.frame(cbind(l.labour,l.wage,l.output,l.capital))
slLDB <- lLDB[order(l.wage),]

#salinam 9 centrinius stebejimus

rs <- (n - 9)/2

#ivertinam mazu ir dideliu x regresiju lygtis

gqaux1 <- lm(slLDB[1:rs,])
summary(gqaux1)
gqaux2 <- lm(slLDB[n-rs+1:n,])
summary(gqaux2)

#palyginsime RSS

RSS1 <- deviance(gqaux1)
RSS2 <- deviance(gqaux2)
RSS2/RSS1
qf(0.95,rs-4,rs-4)
#Kadangi musu apskaiciuotas F yra didesnis uz kritini, nuline hipoteze atmetam

library(lmtest)

gqtest(model,fraction = 9/n,order.by = l.wage)

gqtest(model,fraction = 9/n,order.by = l.capital)

gqtest(model,fraction = 9/n,alternative = "less",order.by = l.output)

#Breush-Pagan

bpaux <- lm(res2 ~ l.wage + l.output + l.capital)
summary(bpaux)

#silpnas hetereskodestiskumas, nes t stat tik su 90 proc reiksmingi

n*summary(bpaux)$r.squared
qchisq(0.95,3)

#musu apsk LM yra mazesnis uz kritini, nulines hipotezes neatmetam

qchisq(0.94,3)

#o su 94 jau atmetam, todel silpnas heteroskediskumas visgi silpnas yra

bptest(model)

#White 

whaux <- lm(res2 ~ l.wage + l.output + l.capital + I(l.wage^2) +
              I(l.output^2) + I(l.capital^2) + I(l.wage*l.output) +
              I(l.wage*l.capital) + I(l.output*l.capital))
summary(whaux)

n*summary(whaux)$r.squared
qchisq(0.95,9)

bptest(model, ~ l.wage + l.output + l.capital + I(l.wage^2) +
         I(l.output^2) + I(l.capital^2) + I(l.wage*l.output) +
         I(l.wage*l.capital) + I(l.output*l.capital))
