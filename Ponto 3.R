
# H0: μ = 4.5
# H1: μ < 4.5

alfa = 0.05

dados <- read.csv('banco_de_dados.csv', sep = ',', dec = '.')
vetor = dados$Força.de.Resistência..y. / dados$Comprimento.do.Fio..x.

# Amostra de dados (substitua com seus próprios dados)
amostra <- c(1.99, 3.0563, 2.8863, 3.5, 3.1275, 4.215, 7.19, 4.8, 2.7055, 3.4375, 4.27, 3.3636, 3.4958, 5.83, 5.4125, 4.4725, 3.45, 10.3, 3.493, 3.1053, 3.1053, 3.3825, 3.31765, 4.23, 3.6883)

# Valor de referência
valor_referencia <- 4.5

# Realize o teste t
resultado_teste <- t.test(amostra, mu = valor_referencia, alternative = "greater")

# Exiba os resultados
print(resultado_teste)

# Parâmetros do problema
variancia_proposta <- 2  # Variância proposta pela empresa
n <- length(amostra)
alpha <- 0.05  # Nível de significância (5%)

# Calcula a estatística do teste chi-quadrado
variancia_amostral <- var(amostra)
estatistica_chi2 <- ((n - 1) * variancia_amostral) / variancia_proposta

# Graus de liberdade
graus_de_liberdade <- n - 1

# Valor crítico da distribuição chi-quadrado
valor_critico_chi2 <- qchisq(1 - alpha, df = graus_de_liberdade)

# Exibe os resultados
cat("Estatística do teste chi-quadrado:", estatistica_chi2, "\n")
cat("Valor crítico da distribuição chi-quadrado:", valor_critico_chi2, "\n")

# Realiza a decisão com base no nível de significância
if (estatistica_chi2 >= valor_critico_chi2) {
  cat("Rejeitamos a hipótese nula. A variância é maior do que 2 unidades de força por centímetro.\n")
} else {
  cat("Não rejeitamos a hipótese nula. A variância é de até 2 unidades de força por centímetro.\n")
}