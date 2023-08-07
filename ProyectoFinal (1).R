library(tidyverse)
library(ggplot2)
library(forecast)
library(knitr)
library(nnet)
library(caret)
library(xgboost)
library(ModelMetrics)
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

df=read.csv("dataNote.csv")
total= read.csv("dfFinal.csv")
importado= total %>% filter(Origen=="Importado") %>% group_by(Fecha) %>% summarise(Cantidad=sum(Cantidad))
total=total %>% group_by(Fecha) %>% summarise(Cantidad=sum(Cantidad))
total$Importado=importado$Cantidad
total= total %>% mutate(importaciones=Importado/Cantidad)
total$Cantidad=NULL
total$Importado=NULL
df=merge(df,total,by="Fecha")

summary(df)
df$X=NULL
df$RUBRO=NULL
View(df)
table(df$Unidad.de.Negocio)
df$Origen=NULL
df$Unidad.de.Negocio=NULL
df %>%
  ggplot( aes(x=Fecha, y=Cantidad, group=Canal, color=Canal)) +
  geom_line() +
  ggtitle("Ventas x Canal") +
  ylab("Cantidad")
df=df %>% group_by(Fecha,Canal,Año,Mes,Inflacion_1,Inflacion_2,Inflacion_3,Indice_Consumidor_1,Indice_Consumidor_2,Indice_Consumidor_3,Aislamiento,Dias_Evento,importaciones) %>% summarise(Cantidad=sum(Cantidad))

notedistri=df %>% filter(Canal=="Distribucion")
noteonline=df %>% filter(Canal!="Distribucion")
notedistri$Canal=NULL
noteonline$Canal=NULL
notedistri$Fecha=NULL
noteonline$Fecha=NULL


50*0.2
notedistritrain=notedistri[1:40,]
noteonlinetrain=noteonline[1:40,]
notedistritest=notedistri[41:50,]
noteonlinetest=noteonline[41:50,]
distrivar=sd(notedistri$Cantidad)
onlinevar=sd(noteonline$Cantidad)

cantnotedistri=notedistritrain[,c(1,2,12)]
mean(c(317,187,180,346,349,414,265,134,186,71,179))
cantnotedistri = rbind(as.data.frame(t(c(2019, 1, 239))) %>% rename(Año=V1,Mes=V2,Cantidad=V3),cantnotedistri)
cantnotedistri=ts(cantnotedistri$Cantidad,frequency = 12)
cantnoteonline=noteonline[,c(1,2,12)]
mean(c(538,2662,1774,478,1382,1509,983,803,1174,871,989))
cantnoteonline = rbind(as.data.frame(t(c(2019, 1, 1197)))%>% rename(Año=V1,Mes=V2,Cantidad=V3),cantnoteonline)
cantnoteonline=ts(cantnoteonline$Cantidad,frequency = 12)

best=BoxCox(cantnotedistri,BoxCox.lambda(cantnotedistri))
plot(best)
seasonplot(best, col = rainbow(10), year.labels = TRUE)
decomp<-stl(best, s.window = 12)
plot(decomp)
adjust<-seasadj(decomp)
plot(naive(adjust))
plot(snaive(adjust))
snaive(adjust)$mean
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(snaive(adjust)$mean, lambda = BoxCox.lambda(cantnotedistri))
# Mostrar los valores en unidades originales
mape(notedistritest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #79
tsdisplay(best)
h=10
plot(best, type = 'n',xlim=end(b2))
lines(b1)
lines(b2, col = "red")
abline(v = end(b1) + 1, lty = 2, lwd = 2)
# Mean (based on the overall mean value)
f1 <- meanf(b1, h = h)
lines(f1$mean, lwd = 2, col = "yellow")
# Naive (based on the last value)
f2 <- rwf(b1, h = h)
lines(f2$mean, lwd = 2, col = "green")
# Drift (based on 1st and last value)
f3 <- rwf(b1, drift = TRUE, h = h)
lines(f3$mean, lwd = 2, col = "orange")
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f3$mean, lambda = BoxCox.lambda(cantnotedistri))
# Mostrar los valores en unidades originales
mape(notedistritest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #99
# Seasonal naive forecast
f4 <- snaive(b1, h = h)
lines(f4$mean, lwd = 2, col = "blue")
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f4$mean, lambda = BoxCox.lambda(cantnotedistri))
# Mostrar los valores en unidades originales
mape(notedistritest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #99
kable(accuracy(f1, b2))
kable(accuracy(f2, b2))
kable(accuracy(f3, b2))
kable(accuracy(f4, b2))
res <- residuals(f4)
plot(res)
hist(res, breaks = "FD", col = "lightgreen")
acf(res, na.action = na.omit)
fit<-tslm(best~trend)
f<-forecast(fit, h=h)
plot(f)
acf(residuals(f))
fit2<-tslm(best~trend+season)
f2<-forecast(fit2, h=h)
plot(f2)
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f2$mean, lambda = BoxCox.lambda(cantnotedistri))
# Mostrar los valores en unidades originales
mape(notedistritest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #168
acf(residuals(f2))
pacf(residuals(f2))
autoplot(f2, xlab = "Cantidad")
# Aplicar transformación inversa de Box-Cox a los pronósticos generados en la escala transformada
f2_inv <- InvBoxCox(f2$mean, lambda = BoxCox.lambda(best))
# Crear un objeto de serie temporal en la escala original
f2_original <- ts(f2_inv, start = end(cantnotedistri) + 1, frequency = 12)
# Gráfico de la serie original y los pronósticos en la escala original
plot(cantnotedistri, xlab = "Fecha", ylab = "Cantidad",xlim=end(f2_inv))
lines(f2_inv, col = "red")

best=BoxCox(cantnoteonline,BoxCox.lambda(cantnoteonline))
plot(best)
seasonplot(best, col = rainbow(10), year.labels = TRUE)
decomp<-stl(best, s.window = 12)
plot(decomp)
adjust<-seasadj(decomp)
plot(naive(adjust))
plot(snaive(adjust))
snaive(adjust)$mean
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(snaive(adjust)$mean, lambda = BoxCox.lambda(cantnoteonline))
# Mostrar los valores en unidades originales
mape(noteonlinetest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #67
tsdisplay(best)
b1<-window(best, end=4.4)
b2<-window(best, start=4.41)
h=length(b2)
plot(best, type = 'n',xlim=end(b2))
lines(b1)
lines(b2, col = "red")
abline(v = end(b1) + 1, lty = 2, lwd = 2)
# Mean (based on the overall mean value)
f1 <- meanf(b1, h = h)
lines(f1$mean, lwd = 2, col = "yellow")
# Naive (based on the last value)
f2 <- rwf(b1, h = h)
lines(f2$mean, lwd = 2, col = "green")
# Drift (based on 1st and last value)
f3 <- rwf(b1, drift = TRUE, h = h)
lines(f3$mean, lwd = 2, col = "orange")
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f3$mean, lambda = BoxCox.lambda(cantnoteonline))
# Mostrar los valores en unidades originales
mape(noteonlinetest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #100
# Seasonal naive forecast
f4 <- snaive(b1, h = h)
lines(f4$mean, lwd = 2, col = "blue")
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f4$mean, lambda = BoxCox.lambda(cantnoteonline))
# Mostrar los valores en unidades originales
mape(noteonlinetest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #132
kable(accuracy(f1, b2))
kable(accuracy(f2, b2))
kable(accuracy(f3, b2))
kable(accuracy(f4, b2))
res <- residuals(f4)
plot(res)
hist(res, breaks = "FD", col = "lightgreen")
acf(res, na.action = na.omit)
fit<-tslm(best~trend)
f<-forecast(fit, h=h)
plot(f)
acf(residuals(f))
fit2<-tslm(best~trend+season)
f2<-forecast(fit2, h=h)
plot(f2)
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f2$mean, lambda = BoxCox.lambda(cantnoteonline))
# Mostrar los valores en unidades originales
mape(noteonlinetest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #85
acf(residuals(f2))
pacf(residuals(f2))
autoplot(f2, xlab = "Cantidad")
# Aplicar transformación inversa de Box-Cox a los pronósticos generados en la escala transformada
f2_inv <- InvBoxCox(f2$mean, lambda = BoxCox.lambda(best))
# Crear un objeto de serie temporal en la escala original
f2_original <- ts(f2_inv, start = end(cantnoteonline) + 1, frequency = 12)
# Gráfico de la serie original y los pronósticos en la escala original
plot(cantnoteonline, xlab = "Fecha", ylab = "Cantidad",xlim=end(f2_inv))
lines(f2_inv, col = "red")


model=naive(cantnotedistri,10)
notedistritest$naive=3882
mape(notedistritest$Cantidad,notedistritest$naive) #127

model=naive(cantnoteonline,10)
noteonlinetest$naive=1027
mejresultonline=1027
mape(noteonlinetest$Cantidad,noteonlinetest$naive) #36 ESTEE ONLINE

model=ses(cantnotedistri,10)
notedistritest$naive=3555
mape(notedistritest$Cantidad,notedistritest$naive) #109

model=ses(cantnoteonline,10)
noteonlinetest$naive=1282
mape(noteonlinetest$Cantidad,noteonlinetest$naive) #46

model=holt(cantnotedistri,10)
notedistritest$naive=model$mean
mape(notedistritest$Cantidad,notedistritest$naive) #188

model=ses(cantnoteonline,10)
noteonlinetest$naive=1282
mape(noteonlinetest$Cantidad,noteonlinetest$naive) #46

model=auto.arima(cantnotedistri)
forecast=forecast(model,h=10)
notedistritest$naive=forecast$mean
mape(notedistritest$Cantidad,notedistritest$naive) #133

model=auto.arima(cantnoteonline)
forecast=forecast(model,h=10)
noteonlinetest$naive=forecast$mean
mape(noteonlinetest$Cantidad,noteonlinetest$naive) #55

model=tbats(cantnotedistri)
forecast=forecast(model,h=10)
notedistritest$naive=forecast$mean
mape(notedistritest$Cantidad,notedistritest$naive) #154

model=tbats(cantnoteonline)
forecast=forecast(model,h=10)
noteonlinetest$naive=1102
mape(noteonlinetest$Cantidad,noteonlinetest$naive) #39

model <- lm(Cantidad~. +I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. +I(Año^2)+I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2+I(Año^2)+I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2+I(Año^2)+I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2+I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2+I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Inflacion_3^2)+I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Inflacion_3^2)+I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Inflacion_3^2)+I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Aislamiento-importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Inflacion_3^2)+I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Aislamiento-importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2)+I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Dias_Evento-Aislamiento-importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2)+I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2)+I(Dias_Evento^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Dias_Evento-Aislamiento-importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2)+I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Dias_Evento-Aislamiento-importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Indice_Consumidor_3^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_2-Dias_Evento-Aislamiento-importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Indice_Consumidor_3^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_1-Indice_Consumidor_2-Dias_Evento-Aislamiento-importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2+I(Inflacion_1^2) +I(Indice_Consumidor_3^2),data=notedistritrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_1-Indice_Consumidor_2-Dias_Evento-Aislamiento-importaciones-Inflacion_3-Indice_Consumidor_1-Inflacion_2 +I(Indice_Consumidor_3^2),data=notedistritrain)
summary(model)
mape(notedistritest$Cantidad,predict(model,notedistritest)) #170


model <- lm(Cantidad~. +I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. +I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. +I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. +I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2)  +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_1+I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2)  +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2)  +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Año^2)  +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Año^2) +I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Mes-Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Año^2) +I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Aislamiento-Mes-Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Año^2) +I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Aislamiento-Mes-Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2-Aislamiento-Mes-Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2)+I(Inflacion_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2-Aislamiento-Mes-Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2-Aislamiento-Mes-Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2) +I(Indice_Consumidor_3^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_1-Inflacion_2-Aislamiento-Mes-Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2) +I(Indice_Consumidor_3^2),data=noteonlinetrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_1-Inflacion_2-Aislamiento-Mes-Indice_Consumidor_3-Indice_Consumidor_2-Inflacion_3-Indice_Consumidor_1+I(importaciones^2) ,data=noteonlinetrain)
summary(model)
mape(noteonlinetest$Cantidad,predict(model,noteonlinetest)) #97


completo=df
View(completo)
completo$Unidad.de.Negocio=NULL
completo$Origen=NULL
completo$Canal=NULL
completo$Fecha=NULL
completo = completo %>% group_by(Año,Mes,Inflacion_1,Inflacion_2,Inflacion_3,Indice_Consumidor_1,Indice_Consumidor_2,Indice_Consumidor_3,Aislamiento,Dias_Evento,importaciones) %>% summarise(Cantidad=sum(Cantidad))

completotrain=completo[1:40,]
completotest=completo[41:50,]
completovar=sd(completo$Cantidad)

cantcompleto=completotrain[,c(1,2,12)]
mean(c(855,2849,1954,824,1731,1923,1248,937,1360,942,1168))
cantcompleto = rbind(as.data.frame(t(c(2019, 1, 1197))) %>% rename(Año=V1,Mes=V2,Cantidad=V3),cantcompleto)
cantcompleto=ts(cantcompleto$Cantidad,frequency = 12)

best=BoxCox(cantcompleto,BoxCox.lambda(cantcompleto))
plot(best)
seasonplot(best, col = rainbow(10), year.labels = TRUE)
decomp<-stl(best, s.window = 12)
plot(decomp)
adjust<-seasadj(decomp)
plot(naive(adjust))
plot(snaive(adjust))
snaive(adjust)$mean
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(snaive(adjust)$mean, lambda = BoxCox.lambda(cantcompleto))
# Mostrar los valores en unidades originales
mape(completotest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #77
tsdisplay(best)
plot(best, type = 'n',xlim=end(b2))
lines(b1)
lines(b2, col = "red")
abline(v = end(b1) + 1, lty = 2, lwd = 2)
# Mean (based on the overall mean value)
f1 <- meanf(b1, h = h)
lines(f1$mean, lwd = 2, col = "yellow")
# Naive (based on the last value)
f2 <- rwf(b1, h = h)
lines(f2$mean, lwd = 2, col = "green")
# Drift (based on 1st and last value)
f3 <- rwf(b1, drift = TRUE, h = h)
lines(f3$mean, lwd = 2, col = "orange")
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f3$mean, lambda = BoxCox.lambda(cantcompleto))
# Mostrar los valores en unidades originales
mape(completotest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #100
# Seasonal naive forecast
f4 <- snaive(b1, h = h)
lines(f4$mean, lwd = 2, col = "blue")
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f4$mean, lambda = BoxCox.lambda(cantcompleto))
# Mostrar los valores en unidades originales
mape(completotest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #100
kable(accuracy(f1, b2))
kable(accuracy(f2, b2))
kable(accuracy(f3, b2))
kable(accuracy(f4, b2))
res <- residuals(f4)
plot(res)
hist(res, breaks = "FD", col = "lightgreen")
acf(res, na.action = na.omit)
fit<-tslm(best~trend)
f<-forecast(fit, h=h)
plot(f)
acf(residuals(f))
fit2<-tslm(best~trend+season)
f2<-forecast(fit2, h=h)
plot(f2)
# Transformación inversa de Box-Cox
inv_adjust <- forecast::InvBoxCox(f2$mean, lambda = BoxCox.lambda(cantcompleto))
# Mostrar los valores en unidades originales
mape(completotest$Cantidad,as.data.frame(inv_adjust)[1:10,]) #143
acf(residuals(f2))
pacf(residuals(f2))
autoplot(f2, xlab = "Cantidad")
# Aplicar transformación inversa de Box-Cox a los pronósticos generados en la escala transformada
f2_inv <- InvBoxCox(f2$mean, lambda = BoxCox.lambda(best))
# Crear un objeto de serie temporal en la escala original
f2_original <- ts(f2_inv, start = end(cantcompleto) + 1, frequency = 12)
# Gráfico de la serie original y los pronósticos en la escala original
plot(cantcompleto, xlab = "Fecha", ylab = "Cantidad",xlim=end(f2_inv))
lines(f2_inv, col = "red")

model=naive(cantcompleto,10)
completotest$naive=5804
mape(completotest$Cantidad,completotest$naive) #81

model=ses(cantcompleto,10)
completotest$naive=5823
mape(completotest$Cantidad,completotest$naive) #81

model=holt(cantcompleto,10)
completotest$naive=model$mean
mape(completotest$Cantidad,completotest$naive) #111

model=auto.arima(cantcompleto)
forecast=forecast(model,h=10)
completotest$naive=forecast$mean
mape(completotest$Cantidad,completotest$naive) #123

model=tbats(cantcompleto)
forecast=forecast(model,h=10)
completotest$naive=5742
mape(completotest$Cantidad,completotest$naive) #79

model <- lm(Cantidad~. +I(Año^2) +I(Mes^2) +I(Inflacion_1^2) +I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. +I(Año^2) +I(Mes^2)+I(Inflacion_2^2) +I(Inflacion_3^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. +I(Año^2) +I(Mes^2)+I(Inflacion_2^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. +I(Año^2)+I(Inflacion_2^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2) +I(Indice_Consumidor_3^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. +I(Año^2)+I(Inflacion_2^2) +I(Indice_Consumidor_1^2) +I(Indice_Consumidor_2^2)+I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. +I(Año^2)+I(Inflacion_2^2) +I(Indice_Consumidor_1^2)+I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_2+I(Año^2)+I(Inflacion_2^2) +I(Indice_Consumidor_1^2)+I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_1-Indice_Consumidor_2+I(Año^2)+I(Inflacion_2^2) +I(Indice_Consumidor_1^2)+I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. -Indice_Consumidor_1-Indice_Consumidor_2+I(Año^2)+I(Inflacion_2^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_3-Indice_Consumidor_1-Indice_Consumidor_2+I(Año^2)+I(Inflacion_2^2) +I(Aislamiento^2) +I(Dias_Evento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_3-Indice_Consumidor_1-Indice_Consumidor_2+I(Año^2)+I(Inflacion_2^2) +I(Aislamiento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2-Inflacion_3-Indice_Consumidor_1-Indice_Consumidor_2+I(Año^2)+I(Inflacion_2^2) +I(Aislamiento^2)+I(importaciones^2),data=completotrain)
summary(model)
model <- lm(Cantidad~. -Inflacion_2-Inflacion_3-Indice_Consumidor_1-Indice_Consumidor_2+I(Año^2) +I(Aislamiento^2)+I(importaciones^2),data=completotrain)
summary(model)
mape(completotest$Cantidad,predict(model,completotest)) #106


cantcompleto=completotrain[,c(1,2,12)]
mean(c(855,2849,1954,824,1731,1923,1248,937,1360,942,1168))
cantcompleto = rbind(as.data.frame(t(c(2019, 1, 1197))) %>% rename(Año=V1,Mes=V2,Cantidad=V3),cantcompleto)

cantcompletox <- as.matrix(cantcompleto %>%
                                                  dplyr::select(Mes, Año),feature_names = as.character(c("Mes", "Año")))
completotestx <- as.matrix(completotest[,1:2],feature_names = as.character(c("Mes", "Año")))


cantcompletoy <- cantcompleto$Cantidad

xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(10,15,20),
    max_depth = c(2,5,10), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = c(0.25,0.3,0.35), # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))

colnames(cantcompletox)=c("Mes","Año")
colnames(completotestx)=c("Mes","Año")
xgb_model <- caret::train(
  cantcompletox, cantcompletoy,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)

xgb_model$bestTune
predict(xgb_model,as.data.frame(completotestx))
completotest$naive=1678
mape(completotest$Cantidad,completotest$naive) #48 ESTEEEEE COMPLETO

notedistritest$results=1678-mejresultonline
mape(notedistritest$Cantidad,notedistritest$results) #62 ESTEE DISTRI
