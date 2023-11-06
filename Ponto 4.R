# Teste de Shapiro-Wilk
  
rm(list = ls())
if(!"EnvStats" %in% installed.packages()){install.packages("EnvStats")}
library(EnvStats)
x <- c(9.95,24.45,31.75,35,25.02,16.86,14.38,9.6,24.35,27.5,17.08,37,41.95,11.66,21.65,17.89,69,10.3,34.93,46.59,44.98,54.12,56.63,22.13,21.15)
shapiro.test(x)
hist(x,main="Main",xlab="value",border="light blue",col="blue",las=1)
qqPlot(x)

# Teste de Kolmogorov-Smirnov
  
if(!"kolmim" %in% installed.packages()){install.packages("kolmim")}
library(kolmim)
if(!"EnvStats" %in% installed.packages()){install.packages("EnvStats")}
library(EnvStats)
x = c(9.95,24.45,31.75,35,25.02,16.86,14.38,9.6,24.35,27.5,17.08,37,41.95,11.66,21.65,17.89,69,10.3,34.93,46.59,44.98,54.12,56.63,22.13,21.15)
ks.test.imp(x, 'pnorm',mean=mean(x),sd=sd(x))
hist(x,main="Main",xlab="value",border="light blue",col="blue",las=1)
qqPlot(x, y = NULL, distribution = "norm", param.list = list(mean=mean(x),sd=sd(x) ))

# Coeficiente de Correlação de Pearson
  
rm(list = ls())
if(!"car" %in% installed.packages()){install.packages("car")}
library(car)
x1 <- c(5,8,11,10,8,4,2,2,9,8,4,11,12,2,4,4,20,1,10,15,15,16,17,6,5)
y1 <- c(9.95,24.45,31.75,35,25.02,16.86,14.38,9.6,24.35,27.5,17.08,37,41.95,11.66,21.65,17.89,69,10.3,34.93,46.59,44.98,54.12,56.63,22.13,21.15)
model1 = lm(y1~x1)
summary(model1)
