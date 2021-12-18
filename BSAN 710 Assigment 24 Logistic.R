# BSAN 710 Assigment 24 
setwd("~/BSAN 710 Statistical Modelling/Downloads") 
library(leaps)
library(boot)
library(MASS)
library(TSA)

glow=read.csv("glow500.csv")
glow$PRIORFRAC=factor(glow$PRIORFRAC)
glow$PREMENO=factor(glow$PREMENO)
glow$MOMFRAC=factor(glow$MOMFRAC)
glow$ARMASSIST=factor(glow$ARMASSIST)
glow$SMOKE=factor(glow$SMOKE)
glow$RATERISK=factor(glow$RATERISK)

##----Step 1 Identify a possible model-----
set.seed(1)

index <- sample(nrow(glow), 0.8*nrow(glow))
train <- glow[index,]
test <- glow[-index,]

null=glm(FRACTURE~1,family=binomial,data=train)
full=glm(FRACTURE~., family=binomial,data=train)
step(null,scope=list(lower=null,upper=full),direction="both")

##-------Step 2 Estimate the model--------
m1 = glm(FRACTURE~ AGE + PRIORFRAC + RATERISK + HEIGHT + 
           MOMFRAC + ARMASSIST, family = binomial, data = train)
summary(m1)

m1 = glm(FRACTURE~ AGE + PRIORFRAC + HEIGHT + 
           MOMFRAC , family = binomial, data = train)
summary(m1)

#-------Step 3 Diagnostic Checks-------
library(ResourceSelection)
hoslem.test(m1$y, fitted(m1), g=10)


##--------Step 1 Identify a possible model Boxplots-----
attach(glow)
str(glow)
boxplot(AGE~FRACTURE)
boxplot(WEIGHT~FRACTURE)
boxplot(HEIGHT~FRACTURE)
chisq.test(FRACTURE, PRIORFRAC)
chisq.test(FRACTURE, PREMENO)
chisq.test(FRACTURE, MOMFRAC)
chisq.test(FRACTURE, ARMASSIST)
chisq.test(FRACTURE, SMOKE)
chisq.test(FRACTURE, RATERISK)

##-------Step 2 Estimate the model--------
m2 = glm(FRACTURE~ AGE + PRIORFRAC + RATERISK +
           MOMFRAC + ARMASSIST, family = binomial, data = train)
summary(m2)

m2 = glm(FRACTURE~ AGE + PRIORFRAC + 
           MOMFRAC , family = binomial, data = train)
summary(m2)

#-------Step 3 Diagnostic Checks-------
library(ResourceSelection)
hoslem.test(m2$y, fitted(m2), g=10)


##---------Question 2--------------------

cran = read.csv("cranfield.csv")
index <- sample(nrow(cran), 0.8*nrow(cran))
train <- cran[index,]
test <- cran[-index,]

str(cran)
cran$Sector = factor(cran$Sector)
cran$Size = factor(cran$Size)
cran$Incent = factor(cran$Incent)
cran$Eval = factor(cran$Eval)

attach(cran)
chisq.test(Profit, Sector)
chisq.test(Profit, Size)
chisq.test(Profit, Incent)
chisq.test(Profit, Eval)

##-------Step 2 Estimate the model--------
m3 = glm(Profit ~factor(Sector)+Incent, family = binomial, data = train)
summary(m3)

m3 = glm(Profit ~.-Size-Eval, family = binomial, data = train)
summary(m3)

#-------Step 3 Diagnostic Checks-------
library(ResourceSelection)
hoslem.test(m3$y, fitted(m3), g=10)
