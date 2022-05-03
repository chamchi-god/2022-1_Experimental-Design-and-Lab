## --------------------------------------------------------------------------------------
x <- scan("vascgraft.txt")
vasc.graft <- data.frame(PSI=gl(4,6,24),block=gl(6,1,24),x)


## --------------------------------------------------------------------------------------
gl(4,6,24)
as.factor(rep(1:4,each=6))
gl(6,1,24)
as.factor(rep(1:6,4))


## --------------------------------------------------------------------------------------
xtabs(x~block+PSI,vasc.graft)


## --------------------------------------------------------------------------------------
vasc.graft.aov <- aov(x~block+PSI,vasc.graft)
summary(vasc.graft.aov)


## --------------------------------------------------------------------------------------
rocket <- read.table("rocket.txt",header=T)
rocket.lm <- lm(y~as.factor(op)+as.factor(batch)+as.factor(treat),rocket)
anova(rocket.lm)


## --------------------------------------------------------------------------------------
tab.4.22 <- matrix(c(73,NA,73,75,74,75,75,NA,NA,
                     67,68,72,71,72,NA,75),ncol=4)
#c(73,NA,73,75,74,75,75,NA,NA,67,68,72,71,72,NA,75)
tab.4.22.df <- data.frame(rep=as.vector(tab.4.22),
                          treat=as.factor(rep(1:4,4)), #gl(4,1,16)
                          block=as.factor(rep(1:4,each=4))) #gl(4,4,16)
fit1 <- lm(rep~block+treat,tab.4.22.df)
summary(aov(rep~block+treat,tab.4.22.df))
anova(fit1)
fit2 <- lm(rep~treat+block,tab.4.22.df)
anova(fit2)


## --------------------------------------------------------------------------------------
battery = read.table("battery.txt",header=T)
battery$Material = as.factor(battery$Material)
battery$Temperature = as.factor(battery$Temperature)
summary(battery)


## --------------------------------------------------------------------------------------
battery.aov = aov(Life~Material+Temperature, data=battery)
(tuk <- tukey.1df(battery.aov, data=battery))
battery.aov = aov(Life~Material*Temperature, data=battery)
# or `Life~Material+Temperature+Material:Temperature`
summary(battery.aov)


## --------------------------------------------------------------------------------------
with(battery, interaction.plot(Temperature, Material, Life,
                               type="b", pch=19, fixed=TRUE, 
                               xlab="Temperature", ylab="Average Life"))

## --------------------------------------------------------------------------------------
impurity = read.table("impurity.txt",header=T)
impurity$Temperature = as.factor(impurity$Temperature)
impurity$Pressure = as.factor(impurity$Pressure)


## --------------------------------------------------------------------------------------
summary(aov(N~Temperature*Pressure, impurity))


## --------------------------------------------------------------------------------------
#install.packages("dae")
library(dae)
impurity.aov = aov(N~Temperature+Pressure, impurity)
summary(impurity.aov)
(tuk <- tukey.1df(impurity.aov, data=impurity))


## --------------------------------------------------------------------------------------
SSN <- tuk$Tukey.SS
SSE.F <- tuk$Devn.SS
# Test statistic F
(F <- (SSN/1)/(SSE.F/7))
# p.val = P(F(1,7) > F)
(p.val <- pf(F,1,7,lower.tail=FALSE))
