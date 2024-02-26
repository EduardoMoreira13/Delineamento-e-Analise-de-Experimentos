# ENSAIO FATORIAL DE 2 Fatores - DIC

# PACOTES ----

library(tidyverse) # manipulação de dados
library(nortest) # Normalidade - lillie e Ad
library(EnvStats) # Normalidade - Goftest
library(DescTools) # Variância - Levene
library(agricolae) # modelo

# DADOS ----

val <- c(30, 34, 29, 28, 31, 31, 31, 35, 32,
         35, 41, 26, 32, 36, 30, 37, 40, 34,
         37, 38, 33, 40, 42, 32, 41, 39, 39,
         36, 42, 36, 41, 40, 40, 40, 44, 45)

trat <- c(rep(c("A1","A2","A3"), 3),
          rep(c("B1","B2","B3"), 3),
          rep(c("C1","C2","C3"), 3),
          rep(c("D1","D2","D3"), 3)) %>% 
        as.factor()

fatA <- rep(c("A","B","C","D"), each = 9) %>% 
        as.factor()

fatB <- rep(rep(c("1","2","3"), 3),4) %>% 
        as.factor()

bloco <- rep(rep(c("1","2","3"), each = 3),4) %>% 
         as.factor()

dados <- data.frame("id" = 1:length(val),
                    "valores" = val,
                    "tratamento" = trat,
                    "fatA" = fatA, # linha
                    "fatB" = fatB, # coluna
                    "bloco" = bloco)


# MODELO ESTATÍSTICO E ANOVA ----

# Informações Importantes
n = 3 # número total de replicações
a = 4 # número de tratamentos do fator a
b = 3 # número de tratamentos do fator b
bl = 3 # número de blocos
N = a*b*n # número total de observações

# Teste Realizado por função - O fator mais difícil é o B 
modelo <- aov(val ~ fatA*fatB + Error(bloco/fatB))
summary(modelo)

SQTot = var(dados$valores)*(length(dados$valores)-1)
SQTot

QMRES = 3.97
QMRES

