library(tidyverse)
library(readxl)


forceRes <- c(9.45, 24.45, 31.45, 35.00, 25.02, 16.86, 14.38, 9.60, 24.35, 27.50, 17.08, 37.00, 41.95, 11.66, 21.65, 17.89, 69.00, 10.30, 34.93, 46.59, 44.98, 54.12, 56.63, 22.13, 21.15)
comprimento <- c(5, 8, 11, 10, 8, 4, 2, 2, 9, 8, 4, 11, 12, 2, 4, 4, 20, 1, 10, 15, 15, 16, 17, 6, 5)

Y <- forceRes
X <- comprimento

database <- cbind(X, Y)

cor(X, Y)

plot(X, Y, main = "Resistence Force X length")
gama <- (lm(Y ~ X))
abline(gama)
coef(gama)
print(summary(gama))