dados <- read.csv('banco_de_dados.csv', sep = ',', dec = '.')

vetor = dados$Força.de.Resistência..y. / dados$Comprimento.do.Fio..x.

estimativa_pontual_variancia <- var(vetor)
dp_variavel <- sd(vetor)

estimativa_pontual_media <- mean(vetor)

# Valor t para 95% de confiança 
alfa <- qt(0.025, df = 24, lower.tail = F)

# Limite inferior e superior da media
media_lim_inf <- estimativa_pontual_media - alfa*dp_variavel/sqrt(25)
media_lim_sup <- estimativa_pontual_media + alfa*dp_variavel/sqrt(25)

# Limite inferior e superior da variancia
variancia_lim_inf <- (24*estimativa_pontual_variancia)/qchisq(0.025, df = 24, lower.tail = F)
variancia_lim_sup <- (24*estimativa_pontual_variancia)/qchisq(0.975, df = 24, lower.tail = F)

diferenca_percentual <- (estimativa_pontual_media - media_lim_inf)/estimativa_pontual_media