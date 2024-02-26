# DELINEAMENTO EM QUADRADOS LATINOS - REPLICAÇÃO

# PACOTES ----

library(tidyverse) # manipulação de dados
library(DescTools) # Variância - Levene
library(asbio) # Teste de aditividade

# DADOS ----

trat <- c("A","B","C",
          "B","C","A",
          "C","A","B",
          "B","C","A",
          "A","B","C",
          "C","A","B",
          "C","A","B",
          "A","B","C",
          "B","C","A",
          "A","B","C",
          "B","C","A",
          "C","A","B") %>% 
        as.factor()

coluna <- rep(c(1:3), 12) %>% 
          as.factor()

linha <- rep(c(1:12), each = 3) %>% 
         as.factor()

replicacao <- rep(c(1:4), each = 9) %>% 
              as.factor()

valores <- c(19.56, 23.16, 29.72,
             22.94, 27.51, 23.71,
             25.06, 17.70, 22.32,
             23.24, 23.54, 18.75,
             16.28, 22.29, 28.09,
             18.53, 19.89, 20.42,
             23.98, 20.46, 19.28,
             15.33, 23.02, 24.97,
             24.41, 22.44, 19.23,
             16.65, 22.69, 24.94,
             18.96, 24.19, 21.95,
             21.49, 15.78, 24.65)

dados <- data.frame("tratamento" = trat,
                    "linha" = linha,
                    "coluna" = coluna,
                    "replicacao" = replicacao,
                    "valores" = valores)


# ANÁLISE EXPLORATÓRIA DOS DADOS ----

# medidas por tratamento
medidas <- dados %>% 
           group_by(tratamento) %>% 
           summarise(media = mean(valores))

# medidas por linha
medidas_lin <- dados %>% 
              group_by(linha) %>% 
              summarise(media = mean(valores))

# medidas por coluna
medidas_col <- dados %>% 
              group_by(coluna) %>% 
              summarise(media = mean(valores))

# medidas por replicacao
medidas_rep <- dados %>% 
               group_by(replicacao) %>% 
               summarise(media = mean(valores))

# Estimação dos Parâmetros

mean(dados$valores) # média geral
medidas$media - mean(dados$valores) # tratamentos
medidas_lin$media - mean(dados$valores) # linha
medidas_col$media - mean(dados$valores) # coluna


# MODELO ESTATÍSTICO E ANOVA ----

modelo <- lm(valores ~ tratamento + coluna + linha, dados)
anova(modelo)
summary(modelo)


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

# TESTE DE ADITIVIDADE DO MODELO ----

tukey.add.test(dados$valores, dados$tratamento,
               dados$linha)

tukey.add.test(dados$valores, dados$tratamento,
               dados$coluna)

tukey.add.test(dados$valores, dados$coluna,
               dados$linha)


mod <- lm(valores ~ tratamento + coluna + linha, dados)
anova(mod)
ad = (predict(mod))^2
mod2 = lm(valores ~ tratamento + coluna + linha + ad, dados)
anova(mod,mod2)

# COMPARAÇÕES MÚLTIPLAS ----

# Teste de Tukey
tukey = TukeyHSD(modelo)
tukey

# estatística estudantizada
q = qtukey(0.95, p , gl2)
dif_sig = q*sqrt(QMRes/p)
dif_sig # valor de referência

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

# Intervalos de confiança
dif1_2 = medidas$media[2] - medidas$media[1]

dif1_2 - q*sqrt(QMRes/n)
dif1_2 + q*sqrt(QMRes/n)







# ERRO DO TIPO II ----

u1 = 
u2 = 
u3 = 
u4 = 
media = (u1 + u2 + u3 + u4) / 4
variancia = 

tal1 = u1 - media
tal2 = u2 - media
tal3 = u3 - media
tal4 = u4 - media
tal5 = u5 - media
tal = c(tal1,tal2,tal3,tal4)

parametro_central = (p*sum(tal^2)) / variancia
parametro_central

# Teste F
alpha = 0.05
fcrit = qf(1 - alpha, gl1, gl2)

p_beta <- pf(fcrit, gl1, gl2, parametro_central)
p_beta

# Tamanho da amostra

parametro_central = (c(1:100)*sum(tal^2)) / variancia
parametro_central

p_beta <- pf(fcrit, gl1, gl2, parametro_central)
round(p_beta, 8)