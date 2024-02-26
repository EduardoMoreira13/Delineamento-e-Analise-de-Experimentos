# ENSAIO FATORIAL - DBC

# PACOTES ----

library(tidyverse) # manipulação de dados
library(DescTools) # Variância - Levene
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest

# DADOS ----

val <- c(090, 086, 096, 084, 
         100, 092, 092, 081,
         102, 087, 106, 090, 
         105, 097, 096, 080,
         114, 093, 112, 091, 
         108, 095, 098, 083)

fatA <- rep(c("A","B","C"), each = 8) %>% 
        as.factor()

fatB <- rep(c("1","2"),12) %>% 
        as.factor()

bloco <- rep(rep(c("B1","B2","B3","B4"),each=2), 3) %>% 
         as.factor()

cod_int <- c("T1","T2","T1","T2","T1","T2","T1","T2",
             "T3","T4","T3","T4","T3","T4","T3","T4",
             "T5","T6","T5","T6","T5","T6","T5","T6")

dados <- data.frame("id" = 1:24,
                    "valores" = val,
                    "fatA" = fatA, # linha da tabela
                    "fatB" = fatB, # coluna da tabela
                    "bloco" = bloco,
                    "codigo" = cod_int)


# ANÁLISE EXPLORATÓRIA DOS DADOS ----

# medidas por Fator A
medidas_A <- dados %>% 
             group_by(fatA) %>% 
             summarise(media = mean(valores))

# medidas por Fator B
medidas_B <- dados %>% 
             group_by(fatB) %>% 
             summarise(media = mean(valores))

# medidas por Interação AB
medidas_int <- dados %>% 
               group_by(codigo) %>% 
               summarise(media = mean(valores))

# medidas por bloco
medidas_bl <- dados %>% 
              group_by(bloco) %>% 
              summarise(media = mean(valores))

# MODELO ESTATÍSTICO E ANOVA ----

# Informações Importantes
n = 4 # número total de replicações/blocos
a = 3 # número de tratamentos do fator a
b = 2 # número de tratamentos do fator b
N = a*b*n # número total de observações

# Teste Realizado por função
modelo <- aov(dados$valores ~ dados$bloco + dados$fatA*dados$fatB)
summary(modelo)
anova(modelo)
aov(modelo)

SQTot = var(dados$valores)*(length(dados$valores)-1)
SQTot

SQRes = anova(modelo)[2][5,1]
SQRes

QMRES = anova(modelo)[3][5,1]
QMRES

# ESTIMAÇÃO DOS PARÂMETROS ----

mean(dados$valores) # média geral
tali <- medidas_A$media - mean(dados$valores) # fator A
betj <- medidas_B$media - mean(dados$valores) # fator B
delk <- medidas_bl$media - mean(dados$valores) # bloco

sum(tali)
sum(betj)
sum(delk)

# Estimação dos Parâmetros - (1), A, B e AB
medidas_int$media[1] - medidas_fatA$media[1] - medidas_fatB$media[1] + mean(dados$valores)
medidas_int$media[1] - medidas_fatA$media[2] - medidas_fatB$media[1] + mean(dados$valores)
medidas_int$media[1] - medidas_fatA$media[1] - medidas_fatB$media[2] + mean(dados$valores)
medidas_int$media[4] - medidas_fatA$media[2] - medidas_fatB$media[2] + mean(dados$valores)


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
q = qtukey(1 - alpha, a*b, (a*b-1)*(n-1))

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
interaction.plot(dados$fatA,dados$fatB,dados$valores, legend = T)
interaction.plot(dados$fatB,dados$fatA,dados$valores, legend = T)

