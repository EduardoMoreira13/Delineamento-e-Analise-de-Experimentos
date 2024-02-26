# ENSAIO FATORIAL - DQL: Quadrado Latino

# PACOTES ----

library(tidyverse) # manipulação de dados
library(DescTools) # Variância - Levene
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest

# DADOS ----

val <- c(090, 106, 108, 081, 090, 088,
         114, 096, 105, 083, 086, 084,
         102, 090, 095, 092, 085, 104,
         087, 084, 100, 096, 110, 091,
         093, 112, 092, 080, 090, 098,
         086, 091, 097, 098, 100, 092)

linha <- rep(c("1","2","3","4","5","6"), each = 6) %>% 
         as.factor()

coluna <- rep(c("1","2","3","4","5","6"), 6) %>% 
          as.factor()

fatA <- c("F1","F1","F1","F2","F2","F2",
          "F1","F1","F1","F2","F2","F2",
          "F1","F2","F2","F1","F2","F1",
          "F2","F2","F1","F1","F1","F2",
          "F2","F1","F2","F2","F1","F1",
          "F2","F2","F2","F1","F1","F1") %>% 
        as.factor()

fatB <- c("G1","G2","G3","G1","G3","G2",
          "G3","G1","G2","G3","G2","G1",
          "G2","G2","G3","G1","G1","G3",
          "G2","G1","G1","G2","G3","G3",
          "G3","G3","G1","G2","G1","G2",
          "G1","G3","G2","G3","G2","G1") %>% 
        as.factor()

cod_trat <- c("A","B","C","D","F","E",
              "C","A","B","F","E","D",
              "B","E","F","A","D","C",              
              "E","D","A","B","C","F",
              "F","C","D","E","A","B",
              "D","F","E","C","B","A") %>% 
             as.factor()

dados <- data.frame("id" = 1:length(val),
                    "valores" = val,
                    "fatA" = fatA,  
                    "fatB" = fatB,
                    "linha" = linha,
                    "coluna" = coluna,
                    "codigo" = cod_trat)


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

# medidas por linha
medidas_lin <- dados %>% 
               group_by(linha) %>% 
               summarise(media = mean(valores))

# medidas por coluna
medidas_col <- dados %>% 
               group_by(coluna) %>% 
               summarise(media = mean(valores))


# MODELO ESTATÍSTICO E ANOVA ----

# Informações Importantes
n = 6 # número total de replicações
p = 6 # número de tratamentos, linhas e colunas 
N = n*p # número total de observações

# Teste Realizado por função
modelo <- aov(dados$valores ~ dados$linha + dados$coluna + dados$fatA*dados$fatB)
summary(modelo)
anova(modelo)
aov(modelo)

SQTot = var(dados$valores)*(length(dados$valores)-1)
SQTot

SQRes = anova(modelo)[2][6,1]
SQRes

QMRES = anova(modelo)[3][6,1]
QMRES

# ESTIMAÇÃO DOS PARÂMETROS ----

mean(dados$valores) # média geral
medidas_A$media - mean(dados$valores) # fator A
medidas_B$media - mean(dados$valores) # fator B
medidas_int$media - mean(dados$valores) # Interação AB
medidas_lin$media - mean(dados$valores) # linha
medidas_col$media - mean(dados$valores) # coluna



# COEFICIENTE DE DETERMINAÇÃO ----

Coef_det = 1 - SQRes / SQTot
Coef_det


# TESTE DE HOMOGENEIDADE DAS VARIÂNCIAS ----

# Ho: As variâncias são homogêneas
# Ha: As variâncias não são homogêneas

# Cuidado ao realizar esse teste em Quadrados Latinos 
# - preferir análise gráfica

# Bartlett e Levene - FATOR 1
bartlett.test(dados$valores ~ dados$fatA) # Exige normalidade dos dados
LeveneTest(dados$valores, dados$fatA, center = mean)
LeveneTest(dados$valores, dados$fatA, center = median)

# Bartlett e Levene - FATOR 2
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
glRes = anova(modelo)[1][6,1]
q = qtukey(1 - alpha, p, glRes)

dif_sig = q*sqrt(QMRES/p)
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

