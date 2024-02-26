# ENSAIO FATORIAL DE 2 Fatores - 2 por 3 - DIC

# PACOTES ----

library(tidyverse) # manipulação de dados
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest
library(DescTools) # Variância - Levene

# DADOS ----

val <- c(0550,0604,
         0669,0650,
         0633,0601,
         0642,0635,
         1037,1052,
         0749,0868,
         1075,1063,
         0729,0860)

trat <- rep(c("1-(1)","2-A","3-B","4-AB","-5C","6-AC","7-BC","8-ABC"), each = 2) %>% 
        as.factor()

fatA <- rep(rep(c("1-Low","2-High"), each = 2),4) %>% 
        as.factor()

fatB <- rep(rep(c("3-Low","4-High"), each = 4),2) %>% 
        as.factor()

fatC <- rep(c("5-Low","6-High"), each = 8) %>% 
        as.factor()

dados <- data.frame("id" = 1:length(val),
                    "valores" = val,
                    "tratamento" = trat,
                    "fatA" = fatA,
                    "fatB" = fatB,
                    "fatC" = fatC)


# ANÁLISE EXPLORATÓRIA DOS DADOS ----

# medidas por tratamento - ABC 
medidas_trat <- dados %>% 
                group_by(tratamento) %>% 
                summarise(media = mean(valores))

# medidas por Fator A
medidas_fatA <- dados %>% 
                group_by(fatA) %>% 
                summarise(media = mean(valores))

# medidas por Fator B
medidas_fatB <- dados %>% 
                group_by(fatB) %>% 
                summarise(media = mean(valores))

# medidas por Fator C
medidas_fatC <- dados %>% 
                group_by(fatC) %>% 
                summarise(media = mean(valores))

# medidas por AB
medidas_fatAB <- dados %>% 
                 group_by(fatA,fatB) %>% 
                 summarise(media = mean(valores))

# medidas por AC
medidas_fatAC <- dados %>% 
                 group_by(fatA,fatC) %>% 
                 summarise(media = mean(valores))

# medidas por BC
medidas_fatBC <- dados %>% 
                 group_by(fatB,fatC) %>% 
                 summarise(media = mean(valores))


# Estimação dos Parâmetros duplos - (1), B, A e AB
medidas_fatAB$media[1] - medidas_fatA$media[1] - medidas_fatB$media[1] + mean(dados$valores)
medidas_fatAB$media[2] - medidas_fatA$media[1] - medidas_fatB$media[2] + mean(dados$valores)
medidas_fatAB$media[3] - medidas_fatA$media[2] - medidas_fatB$media[1] + mean(dados$valores)
medidas_fatAB$media[4] - medidas_fatA$media[2] - medidas_fatB$media[2] + mean(dados$valores)

tali <- medidas_fatA$media - mean(dados$valores)
betj <- medidas_fatB$media - mean(dados$valores)
lamj <- medidas_fatC$media - mean(dados$valores)

sum(tali)
sum(betj)
sum(lamj)


# MODELO ESTATÍSTICO E ANOVA ----

# Informações Importantes
n = 2 # número total de replicações
a = 2 # número de tratamentos do fator a
b = 2 # número de tratamentos do fator b
c = 2 # número de tratamentos do fator c
N = a*b*c*n # número total de observações

# Teste Realizado por função
modelo <- aov(dados$valores ~ dados$fatA*dados$fatB*dados$fatC)
anova(modelo)
summary(modelo)

SQTot = sum((dados$valores - mean(dados$valores))^2)
SQTot

SQA = b*c*n*sum((medidas_fatA$media - mean(dados$valores))^2)
SQA

SQB = a*c*n*sum((medidas_fatB$media - mean(dados$valores))^2)
SQB

SQC = a*b*n*sum((medidas_fatC$media - mean(dados$valores))^2)
SQC

SQRes = anova(modelo)[2][8,1]
SQRes

QMRES = anova(modelo)[3][8,1]
QMRES


# Por variável codificada
X1 <- rep(rep(c(-1,1), each = 2),4)
X2 <- rep(rep(c(-1,1), each = 4),2)
X3 <- rep(c(-1,1), each = 8)

modelo_cod <- lm(val ~ X1*X2*X3)
anova(modelo_cod)
summary(modelo_cod)


# ESTIMAÇÃO DOS PARÂMETROS E EFEITOS ----

mean(dados$valores) # média geral e B0

# Efeitos e Coeficientes
estimacao <- data.frame("X1"=X1,"X2"=X2,"X3"=X3)
X <- model.matrix(~X1*X2*X3, data = estimacao)

Contrastes <- crossprod(X,val)
Contrastes

Efeitos <- Contrastes[2:8,1]/(4*n)
Efeitos

Coeficientes <- c(Contrastes[1,1]/(8*n),Efeitos/2)
Coeficientes

SQA = (Contrastes[2,1]^2) / (8*n)
SQB = (Contrastes[3,1]^2) / (8*n)
SQC = (Contrastes[4,1]^2) / (8*n)
SQAB = (Contrastes[5,1]^2) / (8*n)
SQAC = (Contrastes[6,1]^2) / (8*n)
SQBC = (Contrastes[7,1]^2) / (8*n)
SQABC = (Contrastes[8,1]^2) / (8*n)


# Valores ajustados para os 8 níveis
modelo_cod$fitted.values


# COEFICIENTE DE DETERMINAÇÃO ----

Coef_det = 1 - SQRes / SQTot
Coef_det


# TESTE DE HOMOGENEIDADE DAS VARIÂNCIAS ----

# Ho: As variâncias são homogêneas
# Ha: As variâncias não são homogêneas

# Obs: Cuidado, preferir análise gráfica

# Bartlett e Levene - Var 1
bartlett.test(dados$valores ~ dados$fatA) # Exige normalidade dos dados
LeveneTest(dados$valores, dados$fatA, center = mean)
LeveneTest(dados$valores, dados$fatA, center = median)

# Bartlett e Levene - Var 2
bartlett.test(dados$valores ~ dados$fatB) # Exige normalidade dos dados
LeveneTest(dados$valores, dados$fatB, center = mean)
LeveneTest(dados$valores, dados$fatB, center = median)

# Bartlett e Levene - Var 3
bartlett.test(dados$valores ~ dados$fatC) # Exige normalidade dos dados
LeveneTest(dados$valores, dados$fatC, center = mean)
LeveneTest(dados$valores, dados$fatC, center = median)


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
q = qtukey(1 - alpha, a*b*c, a*b*c*(n-1))

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

