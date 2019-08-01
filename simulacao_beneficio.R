# Simulação para calculo de benefício com média de todos os salários
# idade entrada = 30
x <- 30:64
# salarios
s <- rep(NA, 35)
# salario inicial
sal_inic <- 1000
# crescimento salarial
cs <- 0.01
# tempo
t <- 0:34
# tabela
dados <- data.frame(idade = x, t ,salario = s)
# imputando o salario inicial
dados$salario[1] <- s_e
# ver tabela
dados
# criando funcao que calcula o valor futuro
vf <- function(s, i, t){
  s*(1+i)^t
}
# calculando o salario para cada idade
for(i in 2:35){
  dados$salario[i] <- vf(dados$salario[1], cs, t[i])
}
# vendo tabela
dados
# calculando a media de todos os salarios
mean(dados$salario)
# calculando o quanto a média representa do último salário
mean(dados$salario)/dados$salario[35]

