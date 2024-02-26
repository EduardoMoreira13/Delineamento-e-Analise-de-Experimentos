# Testes T-student e Normal

# PACOTES ----
library(tidyverse)
library(nortest)
library(EnvStats)
library(DescTools)
library(lmtest)

# Uma população com variancia conhecida ----

# H0: u = 60
# HA: u < 60

n = 9
xbar = 50
u = 60
dp = 20
variancia = dp^2

qnorm(0.05) # Quantil
pnorm((xbar - u)/sqrt(variancia/n)) # pvalor

qnorm(0.05, mean=60,sd = sqrt(variancia/n)) # xcrit
pnorm(50, mean=60,sd = sqrt(variancia/n)) # pvalor

# Uma população com variancia desconhecida ----

# H0: u = 1229
# HA: u > 1229

dados <- c(1230, 582, 576, 2093, 2621, 1045, 1439, 717, 1838, 1359)
n <- length(dados)
u <- 1229
xbar <- mean(dados)
s2 <- var(dados)
n <- 10

tcrit <- qt(0.95,n-1)
pt(tcrit, n-1, lower.tail = FALSE)

xcrit <- qt(0.95,n-1)*sqrt(s2/n) + u
xcrit

pt((xbar-u)/sqrt(s2/n), n-1, lower.tail = F)

t.test(dados, alternative = "greater", mu=1229)

# Uma população - Proporção ----

# H0: p = 0,2
# HA: p > 0,2

p <- 0.2
s2 <- p*(1 - p)
n <- 50
p_chapeu <- 0.27

pcrit <- qnorm(0.90)*sqrt(s2/n) + p
pcrit

pnorm((p_chapeu - p)/sqrt(s2/n), lower.tail = F)
  
# Duas populações - Amostras dependentes ----

# H0: u_cartaz - u_sem  = 0
# HA: u_cartaz - u_sem  > 0

cartaz <- c(16,24,18,14,26,17,29)
sem <- c(13,18,14,16,19,12,22)

D = cartaz - sem
D

n = length(D)
xbar = mean(D)
u = 0
S2 = var(D)

pt((xbar - u)/sqrt(S2/n), n-1, lower.tail = F)

t.test(cartaz,sem, alternative = "greater", paired = T)
t.test(D, conf.level = 0.99, alternative = "greater")


# Teste de Variância - Distribuição F ----

# Ho: Var1 = Var2
# Ha: Var1 != Var2

diurno <- c(7.67, 7.92, 6.28, 5.59, 7.71, 2.86, 7.76, 5.95,
            5.93, 4.66, 6.27, 10.13, 7.24, 7.54, 5.00)

noturno <- c(7.72, 4.14, 5.30, 8.35, 6.63, 7.64, 9.06, 4.65,
             2.74, 2.64, 9.79, 4.99, 7.30, 7.29, 5.66)

var_diur <- var(diurno)
var_not <- var(noturno)

W <- var_diur / var_not
W  # teste bilateral

n1 = length(diurno)
n2 = length(noturno)

fobs1 <- qf(0.05, n1-1, n2-1)
fobs2 <- qf(0.95, n1-1, n2-1)
fobs1
fobs2

pvalor <- 2*pf(W,n1-1,n2-1)
pvalor    # Se w > 1 o pvalor é 2*(1 - pf(w,na-1,nb-1))

var.test(diurno, noturno, conf.level = 0.90)


# Duas populações - Amostras indep. e var iguais ----

lib <- c(6.6,10.3,10.8,12.9,9.2,12.3,7.0)
adm <- c(8.1,9.8,8.7,10.0,10.2,8.2,8.7,10.1)

u <- 0
dif <- mean(lib)- mean(adm)
sa <- sd(lib)
sb <- sd(adm)
na <- length(lib)
nb <- length(adm)
gl <- na + nb - 2

s2c <- ((na-1)*sa^2 + (nb-1)*sb^2)/(gl)
s2c 

tobs <- (dif-u)/sqrt(s2c/na + s2c/nb)
tobs

pvalor <- 2*pt(tobs, na+nb-2, lower.tail = F)
pvalor

xcrit1 <- qt(0.025,gl)*sqrt(s2c/na + s2c/nb) + u
xcrit1

xcrit2 <- qt(0.975,gl)*sqrt(s2c/na + s2c/nb) + u
xcrit2

t.test(lib, adm, alternative = "two.sided", 
       paired = FALSE, 
       var.equal = TRUE)

# Duas populações - Amostras indep. e var dif ----

lib <- c(6.6,10.3,10.8,12.9,9.2,12.3,7.0)
adm <- c(8.1,9.8,8.7,10.0,10.2,8.2,8.7,10.1)

u <- 0
dif <- mean(lib)- mean(adm)
sa <- sd(lib)
sb <- sd(adm)
na <- length(lib)
nb <- length(adm)

v1 <- (sa^2/na + sb^2/nb)^2
v2 <- (sa^2/na)^2/(na-1) + (sb^2/nb)^2/(nb-1)

V <- v1/v2
V

tobs <- (dif-u)/sqrt(sa^2/na + sb^2/nb)
tobs

pvalor <- 2*pt(tobs, V, lower.tail = F)
pvalor

xcrit1 <- qt(0.025,V)*sqrt(sa^2/na + sb^2/nb) + u
xcrit1

xcrit2 <- qt(0.975,V)*sqrt(sa^2/na + sb^2/nb) + u
xcrit2

t.test(lib, adm, alternative = "two.sided", 
       paired = FALSE, 
       var.equal = FALSE)



# Duas populações - Proporção ----

# Ho: us - un <= 0    ## alpha = 0,05
# Ho: us - un > 0     

u <- 0
q1 <- 408
q2 <- 288
na <- 480
nb <- 360
dif <- q1/na - q2/nb

pcomb <- (q1 + q2)/(na+nb)
pcomb

s2 <- pcomb*(1 - pcomb)
s2

qnorm(0.95)
zobs <- (dif-u)/sqrt(s2/na + s2/nb)
zobs 

pvalor <- pnorm(zobs, lower.tail = F)
pvalor 

xcrit <- qnorm(0.95)*sqrt(s2/na + s2/nb) + u
xcrit