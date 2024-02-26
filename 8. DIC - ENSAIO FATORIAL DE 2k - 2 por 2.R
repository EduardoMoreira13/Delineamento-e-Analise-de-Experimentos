# ENSAIO FATORIAL DE 2 Fatores - 2 por 2 - DIC

# PACOTES ----

library(tidyverse) # manipulação de dados
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest
library(DescTools) # Variância - Levene

# DADOS ----

val <- c(28, 25, 27, 
         36, 32, 32, 
         18, 19, 23,
         31, 30, 29)

trat <- rep(c("1-(1)","2-A","3-B","4-AB"), each = 3) %>% 
        as.factor()

fatA <- rep(rep(c("1-Low","2-High"), each = 3),2) %>% 
        as.factor()

fatB <- rep(c("1-Low","2-High"), each = 6) %>% 
        as.factor()

dados <- data.frame("id" = 1:length(val),
                    "valores" = val,
                    "tratamento" = trat,
                    "fatA" = fatA,
                    "fatB" = fatB)


# ANÁLISE EXPLORATÓRIA DOS DADOS ----

# medidas por Fator A
medidas_fatA <- dados %>% 
                group_by(fatA) %>% 
                summarise(media = mean(valores))

# medidas por Fator B
medidas_fatB <- dados %>% 
                group_by(fatB) %>% 
                summarise(media = mean(valores))

# medidas por tratamento - Interação
medidas_trat <- dados %>% 
                group_by(tratamento) %>% 
                summarise(media = mean(valores))

# Estimação dos Parâmetros - (1), A, B e AB
medidas_trat$media[1] - medidas_fatA$media[1] - medidas_fatB$media[1] + mean(dados$valores)
medidas_trat$media[2] - medidas_fatA$media[2] - medidas_fatB$media[1] + mean(dados$valores)
medidas_trat$media[3] - medidas_fatA$media[1] - medidas_fatB$media[2] + mean(dados$valores)
medidas_trat$media[4] - medidas_fatA$media[2] - medidas_fatB$media[2] + mean(dados$valores)

tali <- medidas_fatA$media - mean(dados$valores)
betj <- medidas_fatB$media - mean(dados$valores)

sum(tali)
sum(betj)


# MODELO ESTATÍSTICO E ANOVA ----

# Informações Importantes
n = 3 # número total de replicações
a = 2 # número de tratamentos do fator a
b = 2 # número de tratamentos do fator b
N = a*b*n # número total de observações

# Teste Realizado por função
modelo <- aov(dados$valores ~ dados$fatA*dados$fatB)
anova(modelo)
summary(modelo)


# Teste realizado manualmente 
SQTot = sum((dados$valores - mean(dados$valores))^2)
SQTot

SQA = b*n*sum((medidas_fatA$media - mean(dados$valores))^2)
SQA

SQB = a*n*sum((medidas_fatB$media - mean(dados$valores))^2)
SQB

SQRes = anova(modelo)[2][4,1]
SQRes

SQAB = SQTot - SQA - SQB - SQRes
SQAB

QMRES = SQRes / (a*b*(n-1))
QMRES

# Por variável codificada
X1 <- rep(rep(c(-1,1), each = 3),2)
X2 <- rep(c(-1,1), each = 6)

modelo_cod <- lm(val ~ X1*X2)
anova(modelo_cod)
summary(modelo_cod)
  
# ESTIMAÇÃO DOS PARÂMETROS E EFEITOS ----

mean(dados$valores) # média geral e B0

# Efeitos e Coeficientes
estimacao <- data.frame("X1"=X1,"X2"=X2)
X <- model.matrix(~X1*X2, data = estimacao)

Contrastes <- crossprod(X,val)
Contrastes

Efeitos <- Contrastes/(2*n)
Efeitos

Coeficientes <- Efeitos/2
Coeficientes

SQA = (Contrastes[2,1]^2) / (4*n)
SQB = (Contrastes[3,1]^2) / (4*n)
SQAB = (Contrastes[4,1]^2) / (4*n)

# Valores ajustados para os 4 níveis - (1), a, b e ab
modelo_cod$fitted.values


# COEFICIENTE DE DETERMINAÇÃO ----

Coef_det = 1 - SQRes / SQTot
Coef_det


# TESTE DE HOMOGENEIDADE DAS VARIÂNCIAS ----

# Ho: As variâncias são homogêneas
# Ha: As variâncias não são homogêneas

# Bartlett e Levene - Var 1
bartlett.test(dados$valores ~ dados$fatA) # Exige normalidade dos dados
LeveneTest(dados$valores, dados$fatA, center = mean)
LeveneTest(dados$valores, dados$fatA, center = median)

# Bartlett e Levene - Var 2
bartlett.test(dados$valores ~ dados$fatB) # Exige normalidade dos dados
LeveneTest(dados$valores, dados$fatB, center = mean)
LeveneTest(dados$valores, dados$fatB, center = median)


# TESTE DE NORMALIDADE DOS ERROS ----

shapiro.test(modelo$residuals) # Shapiro-Wilk
gofTest(modelo$residuals, test = "sw")[6:10]

lillie.test(modelo$residuals)  # Lilliefors
gofTest(modelo$residuals, test = "lillie", distribuition = "norm")[6:10]

ad.test(modelo$residuals) # Anderson-Darling
gofTest(modelo$residuals, test = "ad")[6:10]


# COMPARAÇÕES MÚLTIPLAS ----

# Teste de Tukey
tukey = TukeyHSD(modelo)
tukey

# estatística studentizada
alpha = 0.05
q = qtukey(1 - alpha, a*b, a*b*(n-1))

dif_sig = q*sqrt(QMRES/n)
dif_sig # valor de referência


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

# Interação
interaction.plot(dados$fatA,dados$fatB,dados$valores)
interaction.plot(dados$fatB,dados$fatA,dados$valores)

