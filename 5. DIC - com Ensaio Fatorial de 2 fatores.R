# ENSAIO FATORIAL DE 2 Fatores - DIC

# PACOTES ----

library(tidyverse) # manipulação de dados
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest
library(DescTools) # Variância - Levene

# DADOS ----

val <- c(130, 155, 074, 180, 
         034, 040, 080, 075, 
         020, 070, 082, 058,
         150, 188, 159, 126, 
         136, 122, 106, 115,
         025, 070, 058, 045,
         138, 110, 168, 160, 
         174, 120, 150, 139,
         096, 104, 082, 060)

trat <- rep(c("A1","A2","A3","B1","B2","B3","C1","C2","C3"), each = 4) %>% 
        as.factor()

fatA <- rep(c("A","B","C"), each = 12) %>% 
        as.factor()

fatB <- rep(rep(c("1","2","3"), each = 4),3) %>% 
        as.factor()

dados <- data.frame("id" = 1:36,
                    "valores" = val,
                    "tratamento" = trat,
                    "fatA" = fatA, # linha
                    "fatB" = fatB) # coluna


# ANÁLISE EXPLORATÓRIA DOS DADOS ----

# medidas por tratamento
medidas <- dados %>% 
           group_by(tratamento) %>% 
           summarise(media = mean(valores))

# medidas por Fator A
medidas_V1 <- dados %>% 
              group_by(fatA) %>% 
              summarise(media = mean(valores))

# medidas por Fator B
medidas_V2 <- dados %>% 
              group_by(fatB) %>% 
              summarise(media = mean(valores))


# MODELO ESTATÍSTICO E ANOVA ----

# Informações Importantes
n = 4 # número total de replicações
a = 3 # número de tratamentos do fator a
b = 3 # número de tratamentos do fator b
N = a*b*n # número total de observações

# Teste Realizado por função
modelo <- aov(dados$valores ~ dados$fatA + dados$fatB + dados$fatA:dados$fatB)
modelo <- aov(dados$valores ~ dados$fatA*dados$fatB)
summary(modelo)
anova(modelo)
aov(modelo)

SQTot = var(dados$valores)*(length(dados$valores)-1)
SQTot

SQRes = anova(modelo)[2][4,1]
SQRes

QMRES = anova(modelo)[3][4,1]
QMRES

# ESTIMAÇÃO DOS PARÂMETROS ----

mean(dados$valores) # média geral
medidas$media - mean(dados$valores) # tratamentos
medidas_V1$media  - mean(dados$valores) # efeito do fator A
medidas_V2$media  - mean(dados$valores) # efeito do fator B


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

# estatística studentizada
alpha = 0.05
q = qtukey(1 - alpha, a, a*b*(n-1))

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

