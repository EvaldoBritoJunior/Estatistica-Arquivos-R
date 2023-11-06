library(tidyverse)
library(readxl)

forceRes <- c(9.45, 24.45, 31.45, 35.00, 25.02, 16.86, 14.38, 9.60, 24.35, 27.50, 17.08, 37.00, 41.95, 11.66, 21.65, 17.89, 69.00, 10.30, 34.93, 46.59, 44.98, 54.12, 56.63, 22.13, 21.15)
comprimento <- c(5, 8, 11, 10, 8, 4, 2, 2, 9, 8, 4, 11, 12, 2, 4, 4, 20, 1, 10, 15, 15, 16, 17, 6, 5)
uni <- c("U1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10", "u11", "u12", "u13", "u14", "u15", "u16", "u17", "u18", "u19", "u20", "u21", "u22", "u23", "u24", "u25")

bd <- data.frame(uni, forceRes, comprimento)

mediaR <- mean(forceRes)
mediaC <- mean(comprimento)

plot(forceRes, comprimento, xlab = "Força de Resistência", ylab = "Comprimento do Fio")
alfa <- lm(comprimento~forceRes)
beta <- lm(forceRes~comprimento)
abline(alfa)
coef(alfa)
print(summary(alfa))