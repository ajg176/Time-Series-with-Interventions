# BSAN 710 Assignment 20 Intervention & Outlier Answers
setwd("~/BSAN 710 Statistical Modelling/Downloads") 
library(leaps)
library(boot)
library(MASS)
library(TSA)
Gamb=read.csv("Gambling.csv")

Add=1.60*(seq(Gamb$Expendature)>=39)
Gamb$adjusted=Gamb$Expendature+Add

plot(Gamb$adjusted,xlab='Time',type='o')
acf(Gamb$adjusted,lag.max=36,ci.type='ma')

plot(diff(Gamb$adjusted),xlab='Time',type='o')
acf(diff(Gamb$adjusted),lag.max=36,ci.type='ma')

fit=arima(Gamb$adjusted,order=c(0,1,1),seasonal=list(order=c(0,0,1),period=12))
fit

plot(rstandard(fit),ylab='Standardized Residuals',type='o')
acf(rstandard(fit),lag.max = 36)
Box.test(residuals(fit),lag=36,type="Ljung",fitdf=2)

S39=1*(seq(Gamb$Expendature)>=39)

fit=arimax(Gamb$Expendature,order=c(0,1,1),seasonal=list(order=c(0,0,1),period=12),
           xtransf=data.frame(S39),
           transfer=list(c(0,0)),method='ML')
fit

plot(rstandard(fit),ylab='Standardized Residuals',type='o')
acf(rstandard(fit),lag.max = 12)
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
Box.test(residuals(fit),lag=12,type="Ljung",fitdf=2)
fit=arimax(Gamb$Expendature,order=c(0,1,1),seasonal=list(order=c(0,0,1),period=12),
           xtransf=data.frame(S39),
           transfer=list(c(1,0)),method='ML')
fit

#-------------Question 2--------------
dgship=read.csv("shipments.csv")

plot(dgship$ship,xlab='Time',type='o')

BoxCox.ar(dgship$ship,method=c("yule-walker"))

dgship$logship=log(dgship$ship)

plot(dgship$logship,xlab='Time',type='o')
acf(dgship$logship,lag.max=36,ci.type='ma')

plot(diff(dgship$logship),xlab='Time',type='o')
acf(diff(dgship$logship),lag.max=36,ci.type='ma')

plot(diff(dgship$logship,lag=12),xlab='Time',type='o')
acf(diff(dgship$logship,lag=12),lag.max=36,ci.type='ma')

plot(diff(diff(dgship$logship,lag=12)),xlab='Time',type='o')
acf(diff(diff(dgship$logship,lag=12)),lag.max=36,ci.type='ma')

fit=arima(dgship$logship,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=12))
fit

plot(rstandard(fit),ylab='Standardized Residuals',type='o')
acf(rstandard(fit),lag.max = 36)
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
Box.test(residuals(fit),lag=36,type="Ljung",fitdf=1)

detectAO((fit))
detectIO(fit)

P168=1*(seq(dgship$logship)==168)
P180=1*(seq(dgship$logship)==180)
P192=1*(seq(dgship$logship)==192)
P204=1*(seq(dgship$logship)==204)

fit1=arima(dgship$logship,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=12),
           xreg=data.frame(P168,P180,P192,P204),io=c(20))
fit1

fit1=arima(dgship$logship,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=12),
           xreg=data.frame(P168,P180,P192,P204))
fit1

plot(rstandard(fit1),ylab='Standardized Residuals',type='o')
acf(rstandard(fit1),lag.max = 36)
hist(rstandard(fit1))
qqnorm(residuals(fit1))
qqline(residuals(fit1))
Box.test(residuals(fit1),lag=36,type="Ljung",fitdf=1)

detectAO(fit1)
detectIO(fit1)
