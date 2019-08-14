# fitditsrplus
# carregando biblioteca
library(fitdistrplus)

# carregando dados
sinistro <- read.csv("sinistros.csv", header = TRUE, sep=",")
# visualizar os dados
head(sinistro)
# criar um vetor apenas com os valores dos sinistros
sinistro <- sinistro$x
# summary
summary(sinistro)
# desvio padrão
sd(sinistro)
# histograma
hist(sinistro)
# ajuste das distribuições
#descdist(sinistro)
# vamos testar para gamma, weibull e lognormal
fit <- fitdist(sinistro, "gamma")
fitdist(sinistro, "weibull")
fitdist(sinistro, "lnorm")
# plotar o histograma com a curva gerada a partir da estimação 
hist(sinistro, pch=20, breaks = 20, prob = TRUE, main = "")
curve(dgamma(x, fit$estimate[1], fit$estimate[2]), add = TRUE, col = "red")
# vamos fazer o histograma e a acumulada
plotdist(sinistro, histo = TRUE, demp = TRUE)
# agora vamos estimar novamente para criar as 3 funções
fit_w  <- fitdist(sinistro, "weibull")
fit_g  <- fitdist(sinistro, "gamma")
fit_ln <- fitdist(sinistro, "lnorm")
summary(fit_w)
summary(fit_g)
summary(fit_ln)
par(mfrow=c(2,2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)

# testar outras distribuições
# carregar outro pacote
library(actuar)
# testaremos a loglogistica e pareto
fit_ll <- fitdist(sinistro, "llogis", start = list(shape = 1, scale = 500))
fit_P  <- fitdist(sinistro, "pareto", start = list(shape = 1, scale = 500))
cdfcomp(list(fit_ln, fit_ll, fit_P), xlogscale = TRUE, ylogscale = TRUE,
        legendtext = c("lognormal", "loglogistic", "Pareto"), lwd=2)

gofstat(list(fit_ln, fit_ll, fit_P), fitnames = c("lnorm", "llogis", "Pareto"))



