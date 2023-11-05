# Estatística Descritiva:

# Carregar pacotes:

if (!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if (!require(psych))
  install.packages("psych")
library(psych)

if (!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if (!require(tidyverse))
  install.packages("tidyverse")
library(tidyverse)

# Carregar banco de dados:

dados <- read.csv('banco_de_dados.csv', sep = ',', dec = '.')

# Visualizar banco de dados:

View(dados)

# Extrair vetor de força resistência por unidade de comprimento:
vetor = dados$Força.de.Resistência..y. / dados$Comprimento.do.Fio..x.

# Medidas de resumo:
# Média:
media <- mean(vetor)
# Mediana:
mediana <- median(vetor)
# Moda:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda = getmode(vetor)
# Variância:
variancia <- var(vetor)
# Desvio-Padrão:
desvio_padrao <- sd(vetor)
# Coeficiente de correlação:
coeficiente_de_correlacao = cor(dados$Comprimento.do.Fio..x., dados$Força.de.Resistência..y.)

# Histograma:
dados %>%
  ggplot(aes(x = Comprimento.do.Fio..x.)) +
  geom_histogram()

dados %>%
  ggplot(aes(x = Força.de.Resistência..y.)) +
  geom_histogram()

dados %>%
  ggplot(aes(x = vetor)) +
  geom_histogram()

# Box-plot:
boxplot(dados$Força.de.Resistência..y.)
boxplot(dados$Comprimento.do.Fio..x.)
boxplot(vetor)

# Gráfico de dispersão:
dados %>%
  ggplot(aes(x = Comprimento.do.Fio..x., y = Força.de.Resistência..y.)) +
  geom_point()
