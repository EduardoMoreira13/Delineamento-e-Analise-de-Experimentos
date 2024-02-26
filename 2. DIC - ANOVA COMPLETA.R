# DELINEAMENTO INTEIRAMENTE CASUALIZADO

# PACOTES ----

library(tidyverse) # manipulação de dados
library(hnp)      # Gráfico de envelope
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest
library(DescTools) # Variância - Levene
library(agricolae) # Teste LSD

# DADOS ----

val <- c(575, 542, 530, 539, 570,
         565, 593, 590, 579, 610,
         600, 651, 610, 637, 629,
         725, 700, 715, 685, 710)

trat <- rep(c("160","180","200","220"), each = 5) %>% 
        as.factor()

dados <- data.frame("id" = 1:20,
                    "valores" = val,
                    "tratamento" = trat)



# ANÁLISE EXPLORATÓRIA DOS DADOS ----

# medidas
medidas <- dados %>% 
           group_by(tratamento) %>% 
           summarise(media = mean(valores),
                     variancia = var(valores),
                     desvio_padrao = sd(valores),
                     cv = desvio_padrao/media,
                     min = min(valores),
                     max = max(valores),
                     mediana = median(valores))


# Boxplot
boxplot(split(dados$valores, dados$tratamento), 
        xlab = "Potências",
        ylab = "Resistência", col = "#329ea8") 
points(medidas$media, col = "Red", 
       pch = "*", cex = 1.5)


ggplot(dados, aes(x = tratamento, y = valores)) +
  geom_boxplot(fill = c("#329ea8"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 20,
               size = 2, fill = "white") +
  labs(y = "Taxa de Gravacao", 
       x = "Tratamentos") +
  scale_y_continuous(breaks = seq(from = 500,to = 750,
                                  by = 50), 
                     limits = c(500,750)) +
  theme_bw()


# MODELO ESTATÍSTICO E ANOVA ----

# Teste Realizado manualmente
ni <- c(5,5,5,5) # número de obs por tratamento
n = sum(ni) # número total de observaçoes
a = 4 # número de tratamentos
gl1 <- a-1 # grau de liberdade ENTRE - SQTRat
gl2 <- n - a # grau de liberdade DENTRO - SQRes
yi = dados$valores # valores
yi_bar = medidas$media # média por tratamento

SQTot = sum((yi - mean(yi))^2)
SQTot

SQTRat = sum(ni*(yi_bar - mean(yi))^2)
SQTRat

SQRes = sum((yi - rep(yi_bar, each = 5))^2)
SQRes

SQRes = sum(medidas$variancia)*a
SQRes

QMTRat = SQTRat / (a-1)
QMTRat

QMRes = SQRes / (n-a)
QMRes

# Teste F
Teste_F <- (SQTRat/gl1)/(SQRes/gl2) 
Teste_F

pvalor <- pf(Teste_F, gl1, gl2, lower.tail = F)
pvalor


# Teste Realizado por função
modelo <- aov(dados$valores ~ dados$tratamento)
summary(modelo)
anova(modelo)
aov(modelo)
modelo



# COEFICIENTE DE DETERMINAÇÃO ----

Coef_det = SQTRat / SQTot
Coef_det

Coef_det = 1 - SQRes / SQTot
Coef_det


# TESTE DE HOMOGENEIDADE DAS VARIÂNCIAS ----

# Ho: As variâncias são homogêneas
# Ha: As variâncias não são homogêneas

# Bartlett e Levene
bartlett.test(dados$valores ~ dados$tratamento) # Exige normalidade dos dados
LeveneTest(dados$valores, dados$tratamento, center = mean)
LeveneTest(dados$valores, dados$tratamento, center = median)



# TESTE DE NORMALIDADE DOS ERROS ----

shapiro.test(modelo$residuals) # Shapiro-Wilk
gofTest(modelo$residuals, test = "sw")[6:10]

lillie.test(modelo$residuals)  # Lilliefors
gofTest(modelo$residuals, test = "lillie", distribuition = "norm")[6:10]

ad.test(modelo$residuals) # Anderson-Darling
gofTest(modelo$residuals, test = "ad")[6:10]


# COMPARAÇÕES MÚLTIPLAS ----

# Teste T com ajuste de Bonferroni
pairwise.t.test(dados$valores, dados$tratamento, 
                p.adjust.method = "bonferroni")

# Teste de Tukey
tukey = TukeyHSD(modelo)
par(mai = c(1,1,0.7,0.2))
plot(tukey, col = "blue")

# Teste de Fischer
print(LSD.test(modelo,"dados$tratamento"))

# INTERVALOS DE CONFIANÇA PARA MÉDIAS ----

r = 4
alpha = 0.05
medidas$media - qt(1-alpha/(2*r), n-a)*sqrt(QMRes/ni[1])
medidas$media + qt(1-alpha/(2*r), n-a)*sqrt(QMRes/ni[1])


# ANÁLISE GRÁFICA ---- 

plot(modelo) # Normalidade e Variância
plot(modelo$residuals) # Independência
hist(modelo$residuals, breaks = 8)

# Normalidade
qqnorm(modelo$residuals)
qqline(modelo$residuals)

# Variabilidade e erros padronizados
res_padronizados = residuals(modelo) / sqrt(QMRES)
plot(res_padronizados ~ dados$valores)
plot(res_padronizados ~ modelo$fitted.values)
plot(modelo$residuals ~ modelo$fitted.values)

# Gráfico do envelope simulado
hnp(modelo, print.on = T, pch = 19)

# TESTE DE LEVENE E ERROS - MANUALMENTE ----

# Erros

modelo$residuals

erros = yi - rep(yi_bar, each = 5)
erros

# Teste de Levene

yi_median = medidas$mediana

zij = abs(yi - rep(yi_median, each = 5))
grupo = dados$tratamento
dados_lev = data.frame(zij,grupo)

medidas_lev <- dados_lev %>% 
               group_by(grupo) %>% 
               summarise(media = mean(zij),
               variancia = var(zij),
               desvio_padrao = sd(zij),
               mediana = median(zij))

zij_med = medidas_lev$media # média por tratamento

SQTot_lev = sum((zij - mean(zij))^2)
SQTot_lev

SQTRat_lev = sum(ni*(zij_med - mean(zij))^2)
SQTRat_lev

SQRes_lev = sum((zij - rep(zij_med, each = 5))^2)
SQRes_lev

# Teste F
Teste_F_lev <- (SQTRat/gl1)/(SQRes/gl2) 
Teste_F_lev

pvalor_lev <- pf(Teste_F, gl1, gl2, lower.tail = F)
pvalor_lev

modelo_lev <- aov(dados_lev$zij ~ dados_lev$grupo)
summary(modelo_lev)

# TESTE DE TUKEY - MANUALMENTE ----

tukey = TukeyHSD(modelo)
par(mai = c(1,1,0.7,0.2))
plot(tukey, col = "blue")

QMRes

# estatística studentizada
alpha = 0.05
q = qtukey(1 - alpha, a, n-a)

dif_sig = q*sqrt(QMRes/ni[1])
dif_sig # valor de referência

# Comparações
abs(medidas$media[1] - medidas$media[2])
abs(medidas$media[1] - medidas$media[3]) 
abs(medidas$media[1] - medidas$media[4])
abs(medidas$media[2] - medidas$media[3])
abs(medidas$media[2] - medidas$media[4])
abs(medidas$media[3] - medidas$media[4]) 

# Intervalos de confiança
dif1_2 = medidas$media[2] - medidas$media[1]

dif1_2 - q*sqrt(QMRes/ni[1])
dif1_2 + q*sqrt(QMRes/ni[1])


# TESTE DE FISCHER - MANUALMENTE ----

print(LSD.test(modelo,"dados$tratamento"))

alpha = 0.05
LSD = qt(1 - alpha/2, gl2)*sqrt(QMRes*(1/ni[1] + 1/ni[2]))
LSD

# Comparações
abs(medidas$media[1] - medidas$media[2])
abs(medidas$media[1] - medidas$media[3]) 
abs(medidas$media[1] - medidas$media[4])
abs(medidas$media[2] - medidas$media[3])
abs(medidas$media[2] - medidas$media[4])
abs(medidas$media[3] - medidas$media[4])


# CONTRASTES ----

# Contraste 1
som_ci.Yi_bar = sum(c(1,1,-1,-1) * medidas$media)
som_ci.Yi_bar

som_ci2 =  sum(c(1,1,-1,-1)^2)

t0 = som_ci.Yi_bar / sqrt((QMRes/ni[1])*som_ci2)
t0

qt(0.025, gl2)
qt(0.975, gl2)

# Intervalo de confiança
som_ci.Yi_bar - qt(0.975, gl2)*sqrt((QMRes/ni[1])*som_ci2)
som_ci.Yi_bar + qt(0.975, gl2)*sqrt((QMRes/ni[1])*som_ci2)


# Contraste 2
som_ci.Yi_bar = sum(c(1,-1,0,0) * medidas$media)
som_ci.Yi_bar

som_ci2 =  sum(c(1,-1,0,0)^2)

t0 = som_ci.Yi_bar / sqrt((QMRes/5)*som_ci2)
t0

qt(0.025, gl2)
qt(0.975, gl2)

# Contraste 3
som_ci.Yi_bar = sum(c(0,0,1,-1) * medidas$media)
som_ci.Yi_bar

som_ci2 =  sum(c(0,0,1,-1)^2)

t0 = som_ci.Yi_bar / sqrt((QMRes/5)*som_ci2)
t0

qt(0.025, gl2)
qt(0.975, gl2)

# ERRO DO TIPO II ----

u1 = 575
u2 = 600
u3 = 650
u4 = 675
media = (u1 + u2 + u3 + u4) / 4
variancia = 25^2

tal1 = u1 - media
tal2 = u2 - media
tal3 = u3 - media
tal4 = u4 - media
tal = c(tal1,tal2,tal3,tal4)

parametro_central = (ni[1]*sum(tal^2)) / variancia
parametro_central

# Teste F
fcrit = qf(1 - alpha, gl1, gl2)

p_beta <- pf(fcrit, gl1, gl2, parametro_central)
p_beta

# Tamanho da amostra

parametro_central = (c(1:100)*sum(tal^2)) / variancia
parametro_central

p_beta <- pf(fcrit, gl1, gl2, parametro_central)
round(p_beta, 8)
