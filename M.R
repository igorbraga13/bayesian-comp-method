# pacotes -----------------------------------------------------------------

require(ggplot2)
require(data.table)
require(patchwork)

n <- 10
theta <- 0.5
set.seed(1118)
bernoulli <- rbinom(n, size = 1, prob = 0.5)
soma_n <- sum(bernoulli)

# Beta(1,4) ---------------------------------------------------------------
alpha <- 1
beta <- 1

alpha2 <- 1
beta2 <- 4
##função de verossimilhança ----------------------------------------------
f_x_dado_theta <- (theta^(soma_n))*(1 - theta)^(n - soma_n)

##distribuição a priori --------------------------------------------------
pi_theta <- ((gamma(alpha + beta))/(gamma(alpha)*gamma(beta)))*((theta^(alpha - 1))*(1 - theta)^(beta - 1))


##distribuição de referência ---------------------------------------------
g_theta <- ((gamma(alpha2 + beta2))/(gamma(alpha2)*gamma(beta2)))*((theta^(alpha2 - 1))*(1 - theta)^(beta2 - 1))

##distribuição a posteriori ----------------------------------------------
f_theta_dado_x <- c(alpha + soma_n, beta + n - soma_n)

## encontrando theta que maximiza nosso M --------------------------------
max <- ((theta^(soma_n + alpha - 1))*((1 - theta)^(n + beta - soma_n - 1))/((theta^(alpha2 - 1))*(1 - theta)^(beta2 - 1)))
theta_estrela <- max
M <- (((gamma(alpha + beta))/(gamma(alpha))*(gamma(beta)))*((theta_estrela^(soma_n))*((1 - theta_estrela)^(n - soma_n)))*((theta_estrela)^(alpha - 1))*((1 - theta_estrela)^(beta - 1)))/((((gamma(alpha2 + beta2))/(gamma(alpha2))*(gamma(beta2)))*((theta_estrela^(alpha2 - 1))*((1 - theta_estrela)^(beta2 - 1)))))

##METODO DE REJEIÇÃO -----------------------------------------------------
# 1 condição de existencia de M -------------------------------------------
stopifnot(beta + alpha + n > alpha2 + beta2)
# 2 condição de existencia de M -------------------------------------------
stopifnot(alpha + soma_n > alpha2 & beta + n - soma_n > beta2)

pi_theta #a priori
g_theta #g_heta é a distribuição de referência
pi_theta_dado_x <- (theta^(alpha + soma_n - 1))*((1 - theta)^(beta + n - soma_n - 1))
theta_max_veross <- soma_n/n
f_x_dado_theta_max_veross <- (theta_max_veross^(soma_n))*(1 - theta_max_veross)^(n - soma_n)

M <- (((gamma(alpha2)*gamma(beta2)*gamma(alpha+beta))/(gamma(alpha2 + beta2)*gamma(alpha)*gamma(beta)))*((alpha + soma_n - alpha2)/(beta + n - beta2 + alpha - alpha2))^(alpha + soma_n - alpha2))*(1 - (alpha + soma_n - alpha2)/(beta + n - beta2 + alpha - alpha2))^(beta + n - soma_n - beta2) 
#1 gerando theta* , da funç]ao g_theta que é igual a pi_theta
theta_estrela <- rbeta(n = 20000, 1, 1)
#2 gerando uma uniforme(0,1)
u <- runif(20000, 0, 1)
#3 se u <= f(theta)/M*g(theta_estrela)
M #...
f_x_dado_theta_estrela <- (theta_estrela^(soma_n))*(1 - theta_estrela)^(n - soma_n)#estimador de verossimilhança pra distribuição escolhida
armazenar_theta_estrela <- u <= ((f_x_dado_theta_estrela*pi_theta)/(M*f_x_dado_theta_max_veross))#valor gerado de pi_theta_dado_x que foi aceito)

armazenar_theta_estrela <- ifelse(((u <= f_x_dado_theta_estrela/f_x_dado_theta_max_veross)), 1, 0)
armazenar_theta_estrela <- cbind(theta_estrela, armazenar_theta_estrela)
armazenar_theta_estrela <- as.data.table(armazenar_theta_estrela)
armazenar_theta_estrela <- armazenar_theta_estrela[armazenar_theta_estrela == 1]

dist_beta <- rbeta(20000, 7, 5)#distribuição a priori

par(mfrow = c(1,1))
hist(armazenar_theta_estrela$theta_estrela, prob = T, main = "Beta(7,5)*")#distribuição a posteriori
curve(dbeta(x, 7, 5), add = T, col = "red")
##probabilidade de aceitação ---------------------------------------------
nrow(armazenar_theta_estrela)/20000#0.36205

# Beta(6.5,4.5) ---------------------------------------------------------------
alpha <- 1
beta <- 1

alpha2 <- 6.5
beta2 <- 4.5
##função de verossimilhança ----------------------------------------------
f_x_dado_theta <- (theta^(soma_n))*(1 - theta)^(n - soma_n)

##distribuição a priori --------------------------------------------------
pi_theta <- ((gamma(alpha + beta))/(gamma(alpha)*gamma(beta)))*((theta^(alpha - 1))*(1 - theta)^(beta - 1))


##distribuição de referência ---------------------------------------------
g_theta <- ((gamma(alpha2 + beta2))/(gamma(alpha2)*gamma(beta2)))*((theta^(alpha2 - 1))*(1 - theta)^(beta2 - 1))


##distribuição a posteriori ----------------------------------------------
f_theta_dado_x <- c(alpha + soma_n, beta + n - soma_n)

## encontrando theta que maximiza nosso M ----------------------------------
max <- ((theta^(soma_n + alpha - 1))*((1 - theta)^(n + beta - soma_n - 1))/((theta^(alpha2 - 1))*(1 - theta)^(beta2 - 1)))
theta <- max
M <- (((gamma(alpha + beta))/(gamma(alpha))*(gamma(beta)))*((theta^(soma_n))*((1 - theta)^(n - soma_n)))*((theta)^(alpha - 1))*((1 - theta)^(beta - 1)))/((((gamma(alpha2 + beta2))/(gamma(alpha2))*(gamma(beta2)))*((theta^(alpha2 - 1))*((1 - theta)^(beta2 - 1)))))

# #METODO DE REJEIÇÃO -----------------------------------------------------
# 1 condição de existencia de M -------------------------------------------
stopifnot(beta + alpha + n > alpha2 + beta2)
# 2 condição de existencia de M -------------------------------------------
stopifnot(alpha + soma_n > alpha2 & beta + n - soma_n > beta2)

pi_theta #a priori
g_theta #g_heta é a distribuição de referência
pi_theta_dado_x <- (theta^(alpha + soma_n - 1))*((1 - theta)^(beta + n - soma_n - 1))
theta_max_veross <- soma_n/n
f_x_dado_theta_max_veross <- (theta_max_veross^(soma_n))*(1 - theta_max_veross)^(n - soma_n)

M <- (((gamma(alpha2)*gamma(beta2)*gamma(alpha+beta))/(gamma(alpha2 + beta2)*gamma(alpha)*gamma(beta)))*((alpha + soma_n - alpha2)/(beta + n - beta2 + alpha - alpha2))^(alpha + soma_n - alpha2))*(1 - (alpha + soma_n - alpha2)/(beta + n - beta2 + alpha - alpha2))^(beta + n - soma_n - beta2) 
#1 gerando theta* , da funç]ao g_theta que é igual a pi_theta
theta_estrela <- rbeta(n = 20000, 1, 1)
#2 gerando uma uniforme(0,1)
u <- runif(20000, 0, 1)
#3 se u <= f(theta)/M*g(theta_estrela)
M #estimador de maxima verossimilhança
f_x_dado_theta_estrela <- (theta_estrela^(soma_n))*(1 - theta_estrela)^(n - soma_n)#estimador de verossimilhança pra distribuição escolhida
armazenar_theta_estrela <- u <= ((f_x_dado_theta_estrela*pi_theta)/(M*f_x_dado_theta_max_veross))#valor gerado de pi_theta_dado_x que foi aceito)

armazenar_theta_estrela <- ifelse(((u <= f_x_dado_theta_estrela/f_x_dado_theta_max_veross)), 1, 0)
armazenar_theta_estrela <- cbind(theta_estrela, armazenar_theta_estrela)
armazenar_theta_estrela <- as.data.table(armazenar_theta_estrela)
armazenar_theta_estrela <- armazenar_theta_estrela[armazenar_theta_estrela == 1]

dist_beta <- rbeta(20000, 7, 5)#distribuição a priori

par(mfrow = c(1,3))
hist(dist_beta, main = "Beta(7,5)")#distribuição a priori
curve(dbeta(x, 7, 5), add = F, col = "red")
hist(armazenar_theta_estrela$theta_estrela, main = "Beta(7,5)*")#distribuição a posteriori

##probabilidade de aceitação ---------------------------------------------
nrow(armazenar_theta_estrela)/20000#0.36205

# Beta(50,50) ---------------------------------------------------------------
alpha <- 1
beta <- 1

alpha2 <- 50
beta2 <- 50
##função de verossimilhança ----------------------------------------------
f_x_dado_theta <- (theta^(soma_n))*(1 - theta)^(n - soma_n)

##distribuição a priori --------------------------------------------------
pi_theta <- ((gamma(alpha + beta))/(gamma(alpha)*gamma(beta)))*((theta^(alpha - 1))*(1 - theta)^(beta - 1))


##distribuição de referência ---------------------------------------------
g_theta <- ((gamma(alpha2 + beta2))/(gamma(alpha2)*gamma(beta2)))*((theta^(alpha2 - 1))*(1 - theta)^(beta2 - 1))


## encontrando theta que maximiza nosso M ----------------------------------
max <- ((theta^(soma_n + alpha - 1))*((1 - theta)^(n + beta - soma_n - 1))/((theta^(alpha2 - 1))*(1 - theta)^(beta2 - 1)))
theta <- max
M <- (((gamma(alpha + beta))/(gamma(alpha))*(gamma(beta)))*((theta^(soma_n))*((1 - theta)^(n - soma_n)))*((theta)^(alpha - 1))*((1 - theta)^(beta - 1)))/((((gamma(alpha2 + beta2))/(gamma(alpha2))*(gamma(beta2)))*((theta^(alpha2 - 1))*((1 - theta)^(beta2 - 1)))))

# #METODO DE REJEIÇÃO -----------------------------------------------------
# 1 condição de existencia de M -------------------------------------------
stopifnot(beta + alpha + n > alpha2 + beta2)
# 2 condição de existencia de M -------------------------------------------
stopifnot(alpha + soma_n > alpha2 & beta + n - soma_n > beta2)

pi_theta #a priori
g_theta #g_heta é a distribuição de referência
pi_theta_dado_x <- (theta^(alpha + soma_n - 1))*((1 - theta)^(beta + n - soma_n - 1))
theta_max_veross <- soma_n/n
f_x_dado_theta_max_veross <- (theta_max_veross^(soma_n))*(1 - theta_max_veross)^(n - soma_n)

M <- (((gamma(alpha2)*gamma(beta2)*gamma(alpha+beta))/(gamma(alpha2 + beta2)*gamma(alpha)*gamma(beta)))*((alpha + soma_n - alpha2)/(beta + n - beta2 + alpha - alpha2))^(alpha + soma_n - alpha2))*(1 - (alpha + soma_n - alpha2)/(beta + n - beta2 + alpha - alpha2))^(beta + n - soma_n - beta2) 
#1 gerando theta* , da funç]ao g_theta que é igual a pi_theta
theta_estrela <- rbeta(n = 20000, 1, 1)
#2 gerando uma uniforme(0,1)
u <- runif(20000, 0, 1)
#3 se u <= f(theta)/M*g(theta_estrela)
M #estimador de maxima verossimilhança
f_x_dado_theta_estrela <- (theta_estrela^(soma_n))*(1 - theta_estrela)^(n - soma_n)#estimador de verossimilhança pra distribuição escolhida
armazenar_theta_estrela <- u <= ((f_x_dado_theta_estrela*pi_theta)/(M*f_x_dado_theta_max_veross))#valor gerado de pi_theta_dado_x que foi aceito)

armazenar_theta_estrela <- ifelse(((u <= f_x_dado_theta_estrela/f_x_dado_theta_max_veross)), 1, 0)
armazenar_theta_estrela <- cbind(theta_estrela, armazenar_theta_estrela)
armazenar_theta_estrela <- as.data.table(armazenar_theta_estrela)
armazenar_theta_estrela <- armazenar_theta_estrela[armazenar_theta_estrela == 1]

dist_beta <- rbeta(20000, 7, 5)#distribuição a priori

par(mfrow = c(1,3))
hist(dist_beta, main = "Beta(7,5)")#distribuição a priori
curve(dbeta(x, 7, 5), add = F, col = "red")
hist(armazenar_theta_estrela$theta_estrela, main = "Beta(7,5)*")#distribuição a posteriori

##probabilidade de aceitação ---------------------------------------------
nrow(armazenar_theta_estrela)/20000#0.36205