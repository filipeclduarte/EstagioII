# Simulação para calculo de benefício com média de todos os salários
# idade = 30
# idade aposentadoria = 30 + 24 = 54
x <- 30:54
# salarios
s <- rep(NA, 25)
# salario inicial
sal_inic <- 1000
# crescimento salarial
cs <- 0.01
# tempo
t <- 0:24
# tabela
dados <- data.frame(idade = x, t ,salario = s)
# imputando o salario inicial
dados$salario[1] <- sal_inic
# ver tabela
dados
# criando funcao que calcula o valor futuro
vf <- function(s, i, t){
  s*(1+i)^t
}
# calculando o salario para cada idade
for(i in 2:25){
  dados$salario[i] <- vf(dados$salario[1], cs, t[i])
}
# vendo tabela
dados
# calculando a media de todos os salarios
mean(dados$salario)
# calculando o quanto a média representa do último salário
mean(dados$salario)/dados$salario[25]


# Simulação para calculo de benefício com média de todos os salários
# idade = 30
# idade aposentadoria = 30 + 28 = 58
x <- 30:58
# salarios
s <- rep(NA, 29)
# salario inicial
sal_inic <- 1000
# crescimento salarial
cs <- 0.01
# tempo
t <- 0:28
# tabela
dados <- data.frame(idade = x, t ,salario = s)
# imputando o salario inicial
dados$salario[1] <- sal_inic
# ver tabela
dados
# criando funcao que calcula o valor futuro
vf <- function(s, i, t){
  s*(1+i)^t
}
# calculando o salario para cada idade
for(i in 2:29){
  dados$salario[i] <- vf(dados$salario[1], cs, t[i])
}
# vendo tabela
dados
# calculando a media de todos os salarios
mean(dados$salario)
# calculando o quanto a média representa do último salário
mean(dados$salario)/dados$salario[29]

