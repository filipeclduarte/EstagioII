
options(scipen=999)
library(MASS)
library(actuar)

# Adrilayne 
set.seed(14)
sinistros_ad <- data.frame(id = 1:1000, Valor = 10*rlnorm(1000, 3.0, 2))
hist(sinistros_ad$Valor)
summary(sinistros_ad$Valor)
sinistros_ad$Valor <- ifelse(sinistros_ad$Valor <= 200, runif(1, 70, 200), sinistros_ad$Valor)
hist(sinistros_ad$Valor)
summary(sinistros_ad$Valor)
write.csv(sinistros_ad, "base_adrilayne.csv", row.names = FALSE)

# Antônio
sinistros_an <- data.frame(id = 1:250, Valor = rgamma(250, 7, rate = 0.0003))
hist(sinistros_an$Valor)
summary(sinistros_an$Valor)
write.csv(sinistros_an, "base_antonio.csv", row.names = FALSE)

# José de Matos
sinistros_jr <- data.frame(id = 1:500, Valor = 250*rpareto(500, 0.7, 1.5))
hist(sinistros_jr$Valor)
summary(sinistros_jr$Valor)
sinistros_jr$Valor <- ifelse(sinistros_jr$Valor <= 700, runif(1, 100, 10000), sinistros_jr$Valor)
hist(sinistros_jr$Valor)
summary(sinistros_jr$Valor)
write.csv(sinistros_an, "base_jose.csv", row.names = FALSE)

# Lucas
sinistros_lucas <- data.frame(id = 1:400, Valor = 2000*rweibull(400, 1.25, 2.3))
hist(sinistros_lucas$Valor)
summary(sinistros_lucas$Valor)
sinistros_lucas$Valor <- ifelse(sinistros_lucas$Valor <= 2000, runif(1, 2000, 5000), sinistros_lucas$Valor)
hist(sinistros_lucas$Valor)
summary(sinistros_lucas$Valor)
write.csv(sinistros_an, "base_lucas.csv", row.names = FALSE)

# Ludmila
sinistros_ludmila <- data.frame(id = 1:3000, Valor = rnorm(3000, 1000, 400))
hist(sinistros_ludmila$Valor)
summary(sinistros_ludmila$Valor)
sinistros_ludmila$Valor <- ifelse(sinistros_ludmila$Valor <= 700, rgamma(1, 1.2, rate = 0.001), sinistros_ludmila$Valor)
sinistros_ludmila$Valor <- ifelse(sinistros_ludmila$Valor > 3000, rgamma(1, 1, rate = 0.001), sinistros_ludmila$Valor)
hist(sinistros_ludmila$Valor)
summary(sinistros_ludmila$Valor)
write.csv(sinistros_ludmila, "base_ludmila.csv", row.names = FALSE)

######

N <- 0:10
qnt_N <- rpois(N, lambda = 10)
X <- c(10, 20, 50, 100, 200, 350, 500, 700, 900, 1150, 3000)
Acum_X <- pgamma(X, 1.5, 0.003)

frequencia <- data.frame(N, qnt_N)
severidade <- data.frame(X, Acum_X)

frequencia 
severidade

c(severidade[1,2], severidade[2:nrow(severidade),2] - severidade[1:nrow(severidade-1)-1,2])

sum(c(severidade[1,2], severidade[2:nrow(severidade),2] - severidade[1:nrow(severidade-1)-1,2]))


