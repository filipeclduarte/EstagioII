
options(scipen=999)

sinistros <- data.frame(id = 1:50, valores = rgamma(50, 2.5, 0.003))
sinistros
write.csv(sinistros, "base_sin.csv", row.names = FALSE)


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



