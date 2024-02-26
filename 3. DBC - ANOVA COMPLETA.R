# DELINEAMENTO EM BLOCOS CASUALIZADOS

# PACOTES ----

library(tidyverse) # manipulação de dados
library(hnp)      # Gráfico de envelope
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest
library(DescTools) # Variância - Levene
library(agricolae) # Teste LSD
library(asbio) # Teste de aditividade


# DADOS ----

trat <- rep(1:4, each = 5) %>% 
        as.factor()

bloco <- rep(c(1:5), 4) %>% 
         as.factor()

valores <- c(73, 68, 74, 71, 67,
            73, 67, 75, 72, 70,
            75, 68, 78, 73, 68,
            73, 71, 75, 75, 69)

dados <- data.frame("tratamento" = trat,
                   "bloco" = bloco,
                   "valores" = valores)

# ALEATORIZAÇÃO DO EXPERIMENTO E CROQUI ----

trat <- paste("T", 1:4, sep = "")

aleatorizacao <- design.rcbd(trat, r = 5,
                             serie = 0, seed = 18)

DBC <- aleatorizacao$book
DBC$plots <- as.factor(rep(1:4, times = 5))

ggplot(DBC, aes(x = block, y = plots, 
                fill = trat, label = trat)) + 
  geom_tile(color = "black") + 
  geom_text() +
  xlab("Blocos") +
  ylab("Unidades Experimentais")


# ANÁLISE EXPLORATÓRIA DOS DADOS ----

# medidas por tratamento
medidas <- dados %>% 
           group_by(tratamento) %>% 
           summarise(media = mean(valores),
                     variancia = var(valores),
                     desvio_padrao = sd(valores),
                     cv = desvio_padrao/media,
                     min = min(valores),
                     max = max(valores),
                     mediana = median(valores))

# medidas por bloco
medidas_bl <- dados %>% 
              group_by(bloco) %>% 
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
  scale_y_continuous(breaks = seq(from = 65,to = 80,
                                  by = 5), 
                     limits = c(60,80)) +
  theme_bw()

# MODELO ESTATÍSTICO E ANOVA ----

# Teste Realizado manualmente
n <- 5 # número de obs por tratamento
a = 4 # número de tratamentos
b = 5 # número de blocos
N = a*n # número total de observaçoes
gl1 <- a-1 # grau de liberdade ENTRE - SQTRat
gl2 <- (a-1)*(b-1) # grau de liberdade DENTRO - SQRes
gl3 <- b-1 # grau de liberdade do Bloco - SQBloco
yi = dados$valores # valores
yi_bar = medidas$media # média por tratamento
yi_bar_bloco = medidas_bl$media # média por bloco

SQTot = sum((yi - mean(yi))^2)
SQTot

SQTRat = b*sum((yi_bar - mean(yi))^2)
SQTRat

SQBloco = a*sum((yi_bar_bloco - mean(yi))^2)
SQBloco

SQRes = SQTot - SQTRat - SQBloco
SQRes

QMTRat = SQTRat / (a-1)
QMTRat

QMRes = SQRes / ((a-1)*(b-1))
QMRes

QMBloco = SQBloco / (b-1)
QMBloco

# Teste F
Teste_F <- (SQTRat/gl1)/(SQRes/gl2) 
Teste_F

pvalor <- pf(Teste_F, gl1, gl2, lower.tail = F)
pvalor

# Teste Realizado por função
modelo <- aov(dados$valores ~ dados$tratamento + dados$bloco)
summary(modelo)
anova(modelo)
aov(modelo)
modelo

# COEFICIENTE DE DETERMINAÇÃO ----

R2 = 1 - SQRes / SQTot
R2


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


# TESTE DE ADITIVIDADE DO MODELO ----

tukey.add.test(dados$valores, dados$tratamento,
               dados$bloco)

# COMPARAÇÕES MÚLTIPLAS ----

# Teste T com ajuste de Bonferroni
pairwise.t.test(dados$valores, dados$tratamento, 
                p.adjust.method = "bonferroni")

# Teste de Tukey
tukey = TukeyHSD(modelo)
par(mai = c(1,1,0.7,0.2))
plot(tukey, col = "blue")

# estatística estudantizada
q = qtukey(0.95, 4, gl2)
dif_sig = q*sqrt(QMRes/n)
dif_sig # valor de referência

# Comparações
abs(medidas$media[1] - medidas$media[2])
abs(medidas$media[1] - medidas$media[3]) 
abs(medidas$media[1] - medidas$media[4])
abs(medidas$media[2] - medidas$media[3])
abs(medidas$media[2] - medidas$media[4])
abs(medidas$media[3] - medidas$media[4]) 

# Teste de Fischer
print(LSD.test(modelo,"dados$tratamento"))


# INTERVALOS DE CONFIANÇA PARA MÉDIAS ----

r = 4
alpha = 0.05
medidas$media - qt(1-alpha/(2*r), n-a)*sqrt(QMRes/n)
medidas$media + qt(1-alpha/(2*r), n-a)*sqrt(QMRes/n)

# ANÁLISE GRÁFICA ---- 

plot(modelo) # Normalidade e Variância
plot(modelo$residuals) # Independência
hist(modelo$residuals, breaks = 8)

# Normalidade
qqnorm(modelo$residuals)
qqline(modelo$residuals)

# Variabilidade e erros padronizados
res_padronizados = residuals(modelo) / sqrt(QMRes)
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
q = qtukey(1 - alpha, a, (a-1)*(b-1))

dif_sig = q*sqrt(QMRes/n)
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

dif1_2 - q*sqrt(QMRes/n)
dif1_2 + q*sqrt(QMRes/n)


# TESTE DE FISCHER - MANUALMENTE ----

print(LSD.test(modelo,"dados$tratamento"))

alpha = 0.05
LSD = qt(1 - alpha/2, gl2)*sqrt(QMRes*(1/n + 1/n))
LSD

# Comparações
abs(medidas$media[1] - medidas$media[2])
abs(medidas$media[1] - medidas$media[3]) 
abs(medidas$media[1] - medidas$media[4])
abs(medidas$media[2] - medidas$media[3])
abs(medidas$media[2] - medidas$media[4])
abs(medidas$media[3] - medidas$media[4])

