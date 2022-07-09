# pacotes -----------------------------------------------------------------

require(ggplot2)
require(data.table)
require(patchwork)

n <- 10
theta <- 0.5
set.seed(1118)
bernoulli <- rbinom(n, size = 1, prob = 0.5)
soma_n <- sum(bernoulli)

# Beta(1,1) ---------------------------------------------------------------
alpha <- 1
beta <- 1

##função de verossimilhança ----------------------------------------------
f_x_dado_theta <- (theta^(soma_n))*(1 - theta)^(n - soma_n)

##distribuição a priori --------------------------------------------------
pi_theta <- (theta^(alpha + soma_n -1))*((1 - theta)^(beta + soma_n - 1))


##distribuição preditiva a priori ---------------------------------------
f_x <- ((gamma(alpha + beta))/(gamma(alpha))*(gamma(beta)))*((gamma(alpha + soma_n))*(gamma(beta + n - soma_n)))/(gamma(alpha + beta + n))


##distribuição a posteriori para theta -----------------------------------
f_theta_dado_x <- (f_x_dado_theta*pi_theta)/f_x
alpha + soma_n ; beta + n - soma_n#será uma beta(7,5)


# #METODO DE REJEIÇÃO -----------------------------------------------------
pi_theta#a priori
g_theta <- pi_theta#g_heta é a distribuição de referência
pi_theta_dado_x <- (theta^(alpha + soma_n - 1))*((1 - theta)^(beta + n - soma_n - 1))
theta_max_veross <- soma_n/n
f_x_dado_theta_max_veross <- (theta_max_veross^(soma_n))*(1 - theta_max_veross)^(n - soma_n)

M <- f_x_dado_theta_max_veross
#1 gerando theta* , da funç]ao g_theta que é igual a pi_theta
theta_estrela <- rbeta(n = 20000, 1, 1)
#2 gerando uma uniforme(0,1)
u <- runif(20000, 0, 1)
#3 se u <= f(theta)/M*g(theta_estrela)
M #estimador de maxima verossimilhança
f_x_dado_theta_estrela <- (theta_estrela^(soma_n))*(1 - theta_estrela)^(n - soma_n)#estimador de verossimilhança pra distribuição escolhida
armazenar_theta_estrela <- u <= (f_x_dado_theta_estrela/f_x_dado_theta_max_veross)#valor gerado de pi_theta_dado_x que foi aceito)

armazenar_theta_estrela <- ifelse(((u <= f_x_dado_theta_estrela/f_x_dado_theta_max_veross)), 1, 0)
armazenar_theta_estrela <- cbind(theta_estrela, armazenar_theta_estrela)
armazenar_theta_estrela <- as.data.table(armazenar_theta_estrela)
armazenar_theta_estrela <- armazenar_theta_estrela[armazenar_theta_estrela == 1]
                                 
dist_beta <- rbeta(20000, 7, 5)#distribuição a priori

par(mfrow = c(1,1))
hist(armazenar_theta_estrela$theta_estrela, main = "Beta(7,5)*", probability = T)#distribuição a posteriori
curve(dbeta(x, 7, 5), add = T, col = "red")
##probabilidade de aceitação ---------------------------------------------
nrow(armazenar_theta_estrela)/20000#0.36205

# Beta(5,5) ---------------------------------------------------------------
alpha <- 5
beta <- 5

##função de verossimilhança ----------------------------------------------
f_x_dado_theta <- (theta^(soma_n))*(1 - theta)^(n - soma_n)

##distribuição preditiva a priori ---------------------------------------
f_x <- ((gamma(alpha + beta))/(gamma(alpha))*(gamma(beta)))*((gamma(alpha + soma_n))*(gamma(beta + n - soma_n)))/(gamma(alpha + beta + n))


##distribuição a posteriori para theta -----------------------------------
f_theta_dado_x <- (f_x_dado_theta*pi_theta)/f_x
alpha + soma_n ; beta + n - soma_n#será uma beta(11,9)

dist_beta <- rbeta(10000, 11, 9)#olhar se é igual a distribuição a posteriori

# #METODO DE REJEIÇÃO -----------------------------------------------------
pi_theta#a priori
g_theta <- pi_theta#g_heta é a distribuição de referência
pi_theta_dado_x <- (theta^(alpha + soma_n - 1))*((1 - theta)^(beta + n - soma_n - 1))
theta_max_veross <- soma_n/n
f_x_dado_theta_max_veross <- (theta_max_veross^(soma_n))*(1 - theta_max_veross)^(n - soma_n)

M <- f_x_dado_theta_max_veross
#1 gerando theta* , da funç]ao g_theta que é igual a pi_theta
theta_estrela <- rbeta(n = 20000, 5, 5)
#2 gerando uma uniforme(0,1)
u <- runif(20000, 0, 1)
#3 se u <= f(theta)/M*g(theta_estrela)
M #estimador de maxima verossimilhança
f_x_dado_theta_estrela <- (theta_estrela^(soma_n))*(1 - theta_estrela)^(n - soma_n)#estimador de verossimilhança pra distribuição escolhida
armazenar_theta_estrela <- u <= (f_x_dado_theta_estrela/f_x_dado_theta_max_veross)#valor gerado de pi_theta_dado_x que foi aceito)

armazenar_theta_estrela <- ifelse(((u <= f_x_dado_theta_estrela/f_x_dado_theta_max_veross)), 1, 0)
armazenar_theta_estrela <- cbind(theta_estrela, armazenar_theta_estrela)
armazenar_theta_estrela <- as.data.table(armazenar_theta_estrela)
armazenar_theta_estrela <- armazenar_theta_estrela[armazenar_theta_estrela == 1]

hist(armazenar_theta_estrela$theta_estrela, main = "Beta(11,9)*", probability = T)#distribuição a posteriori
curve(dbeta(x,11, 9), add = T, col = "red")
##probabilidade de aceitação ---------------------------------------------
nrow(armazenar_theta_estrela)/20000#0.6337

# Beta(5,1) ---------------------------------------------------------------
alpha <- 5
beta <- 1

##função de verossimilhança ----------------------------------------------
f_x_dado_theta <- (theta^(soma_n))*(1 - theta)^(n - soma_n)

##distribuição a priori --------------------------------------------------
pi_theta <- (theta^(alpha + soma_n -1))*((1 - theta)^(beta + soma_n - 1))


##distribuição preditiva a priori ---------------------------------------
f_x <- ((gamma(alpha + beta))/(gamma(alpha))*(gamma(beta)))*((gamma(alpha + soma_n))*(gamma(beta + n - soma_n)))/(gamma(alpha + beta + n))


##distribuição a posteriori para theta -----------------------------------
f_theta_dado_x <- (f_x_dado_theta*pi_theta)/f_x
alpha + soma_n ; beta + n - soma_n#será uma beta(11,5)

dist_beta <- rbeta(10000, 11, 5)#olhar se é igual a distribuição a posteriori

# #METODO DE REJEIÇÃO -----------------------------------------------------
pi_theta#a priori
g_theta <- pi_theta#g_heta é a distribuição de referência
pi_theta_dado_x <- (theta^(alpha + soma_n - 1))*((1 - theta)^(beta + n - soma_n - 1))
theta_max_veross <- soma_n/n
f_x_dado_theta_max_veross <- (theta_max_veross^(soma_n))*(1 - theta_max_veross)^(n - soma_n)

M <- f_x_dado_theta_max_veross
#1 gerando theta* , da funç]ao g_theta que é igual a pi_theta
theta_estrela <- rbeta(n = 20000, 5, 1)
#2 gerando uma uniforme(0,1)
u <- runif(20000, 0, 1)
#3 se u <= f(theta)/M*g(theta_estrela)
M #estimador de maxima verossimilhança
f_x_dado_theta_estrela <- (theta_estrela^(soma_n))*(1 - theta_estrela)^(n - soma_n)#estimador de verossimilhança pra distribuição escolhida
armazenar_theta_estrela <- u <= (f_x_dado_theta_estrela/f_x_dado_theta_max_veross)#valor gerado de pi_theta_dado_x que foi aceito)

armazenar_theta_estrela <- ifelse(((u <= f_x_dado_theta_estrela/f_x_dado_theta_max_veross)), 1, 0)
armazenar_theta_estrela <- cbind(theta_estrela, armazenar_theta_estrela)
armazenar_theta_estrela <- as.data.table(armazenar_theta_estrela)
armazenar_theta_estrela <- armazenar_theta_estrela[armazenar_theta_estrela == 1]

hist(armazenar_theta_estrela$theta_estrela, main = "Beta(11,5)*", probability = T)#distribuição a posteriori
curve(dbeta(x, 11, 5), add = T, col = "red")
##probabilidade de aceitação ---------------------------------------------
nrow(armazenar_theta_estrela)/20000#0.2807

# Beta(1,5) ---------------------------------------------------------------
alpha <- 1
beta <- 5

##função de verossimilhança ----------------------------------------------
f_x_dado_theta <- (theta^(soma_n))*(1 - theta)^(n - soma_n)

##distribuição a priori --------------------------------------------------
pi_theta <- (theta^(alpha + soma_n -1))*((1 - theta)^(beta + soma_n - 1))


##distribuição preditiva a priori ---------------------------------------
f_x <- ((gamma(alpha + beta))/(gamma(alpha))*(gamma(beta)))*((gamma(alpha + soma_n))*(gamma(beta + n - soma_n)))/(gamma(alpha + beta + n))


##distribuição a posteriori para theta -----------------------------------
f_theta_dado_x <- (f_x_dado_theta*pi_theta)/f_x
alpha + soma_n ; beta + n - soma_n#será uma beta(7,9)

dist_beta <- rbeta(10000, 7, 9)#olhar se é igual a distribuição a posteriori

# #METODO DE REJEIÇÃO -----------------------------------------------------
pi_theta#a priori
g_theta <- pi_theta#g_heta é a distribuição de referência
pi_theta_dado_x <- (theta^(alpha + soma_n - 1))*((1 - theta)^(beta + n - soma_n - 1))
theta_max_veross <- soma_n/n
f_x_dado_theta_max_veross <- (theta_max_veross^(soma_n))*(1 - theta_max_veross)^(n - soma_n)

M <- f_x_dado_theta_max_veross
#1 gerando theta* , da funç]ao g_theta que é igual a pi_theta
theta_estrela <- rbeta(n = 20000, 1, 5)
#2 gerando uma uniforme(0,1)
u <- runif(20000, 0, 1)
#3 se u <= f(theta)/M*g(theta_estrela)
M #estimador de maxima verossimilhança
f_x_dado_theta_estrela <- (theta_estrela^(soma_n))*(1 - theta_estrela)^(n - soma_n)#estimador de verossimilhança pra distribuição escolhida
armazenar_theta_estrela <- u <= (f_x_dado_theta_estrela/f_x_dado_theta_max_veross)#valor gerado de pi_theta_dado_x que foi aceito)

armazenar_theta_estrela <- ifelse(((u <= f_x_dado_theta_estrela/f_x_dado_theta_max_veross)), 1, 0)
armazenar_theta_estrela <- cbind(theta_estrela, armazenar_theta_estrela)
armazenar_theta_estrela <- as.data.table(armazenar_theta_estrela)
armazenar_theta_estrela <- armazenar_theta_estrela[armazenar_theta_estrela == 1]

hist(armazenar_theta_estrela$theta_estrela, main = "Beta(7,9)*", probability = T)#distribuição a posteriori
curve(dbeta(x, 7, 9), add = T, col = "red")
##probabilidade de aceitação ---------------------------------------------
nrow(armazenar_theta_estrela)/20000#0.0926


