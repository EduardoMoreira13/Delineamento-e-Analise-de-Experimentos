# DELINEAMENTO EM QUADRADOS LATINOS

# PACOTES ----

library(tidyverse) # manipulação de dados
library(hnp)      # Gráfico de envelope
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest
library(DescTools) # Variância - Levene
library(agricolae) # Teste LSD
library(asbio) # Teste de aditividade
library(ExpDes.pt) # ANOVA

# DADOS ----

trat <- c("D","A","B","C","E",
          "C","E","A","B","D",
          "E","B","C","D","A",
          "B","D","E","A","C",
          "A","C","D","E","B") %>% 
        as.factor()

coluna <- rep(c(1:5), 5) %>% 
          as.factor()

linha <- rep(c(1:5), each = 5) %>% 
         as.factor()

valores <- c(432, 518, 458, 583, 331,
             724, 478, 524, 550, 400,
             489, 384, 556, 297, 420,
             494, 500, 313, 486, 501,
             515, 660, 438, 394, 318)

dados <- data.frame("tratamento" = trat,
                    "linha" = linha,
                    "coluna" = coluna,
                    "valores" = valores)


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

# medidas por linha
medidas_lin <- dados %>% 
              group_by(linha) %>% 
              summarise(media = mean(valores),
              variancia = var(valores),
              desvio_padrao = sd(valores),
              cv = desvio_padrao/media,
              min = min(valores),
              max = max(valores),
              mediana = median(valores))

# medidas por coluna
medidas_col <- dados %>% 
              group_by(coluna) %>% 
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
p <- 5 # número de tratamentos, linhas e colunas
gl1 <- p-1 # grau de liberdade ENTRE - SQTRat
gl2 <- (p-2)*(p-1) # grau de liberdade DENTRO - SQRes
gl3 <- p-1 # grau de liberdade da Linha - SQLinha
gl4 <- p-1 # grau de liberdade da Coluna - SQColuna

yi = dados$valores # valores
yi_bar = medidas$media # média por tratamento
yi_bar_linha = medidas_lin$media # média por linha
yi_bar_coluna = medidas_col$media # média por coluna

SQTot = sum((yi - mean(yi))^2)
SQTot

SQTRat = p*sum((yi_bar - mean(yi))^2)
SQTRat

SQLinha = p*sum((yi_bar_linha - mean(yi))^2)
SQLinha

SQCol = p*sum((yi_bar_coluna - mean(yi))^2)
SQCol

SQRes = SQTot - SQTRat - SQLinha - SQCol
SQRes

QMTRat = SQTRat / (p-1)
QMTRat

QMRes = SQRes / ((p-2)*(p-1))
QMRes

QMLinha = SQLinha / (p-1)
QMLinha

QMCol = SQCol / (p-1)
QMCol

# Teste F
Teste_F <- (SQTRat/gl1)/(SQRes/gl2) 
Teste_F

pvalor <- pf(Teste_F, gl1, gl2, lower.tail = F)
pvalor

# Teste Realizado por função
modelo <- aov(dados$valores ~ dados$tratamento + dados$linha + dados$coluna)
summary(modelo)
anova(modelo)
aov(modelo)
modelo

Modelo = dql(trat = dados$tratamento, 
             linha = dados$linha, 
             coluna =  dados$coluna,
             resp = dados$valores,
             quali = T,
             mcomp = "tukey",
             sigT = 0.05,
             sigF = 0.05,
             unfold = NULL)


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
               dados$linha)

tukey.add.test(dados$valores, dados$tratamento,
               dados$coluna)


# COMPARAÇÕES MÚLTIPLAS ----

# Teste de Tukey
tukey = TukeyHSD(modelo)
par(mai = c(1,1,0.7,0.2))
plot(tukey, col = "blue")

# estatística estudantizada
q = qtukey(0.95, p , gl2)
dif_sig = q*sqrt(QMRes/p)
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

plotres(Modelo)

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


# Intervalos de confiança
dif1_2 = medidas$media[2] - medidas$media[1]

dif1_2 - q*sqrt(QMRes/n)
dif1_2 + q*sqrt(QMRes/n)






