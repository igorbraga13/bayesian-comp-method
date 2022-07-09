#Método SIR(Sampling Importance Resampling) ou Bootstrap Bayesiano
require(purrr)

n <- 10
#theta 
set.seed(1118)
bernoulli <- rbinom(n, size = 1, prob = 0.5)
soma_n <- sum(bernoulli)

# Beta(1,50) ---------------------------------------------------------------
#priori
alpha <- 1
beta <- 1
#candidata (distribuição de importância)
alpha2 <- 10
beta2 <- 90
##função de verossimilhança ----------------------------------------------
f_x_dado_theta <- (theta^(soma_n))*(1 - theta)^(n - soma_n)

##distribuição a priori --------------------------------------------------
pi_theta <- ((gamma(alpha + beta))/(gamma(alpha)*gamma(beta)))*((theta^(alpha - 1))*(1 - theta)^(beta - 1))


##distribuição de importância --------------------------------------------
g_theta <- ((gamma(alpha2 + beta2))/(gamma(alpha2)*gamma(beta2)))*((theta^(alpha2 - 1))*(1 - theta)^(beta2 - 1))

##distribuição a posteriori ----------------------------------------------
f_theta_dado_x <- c(alpha + soma_n, beta + n - soma_n)

##MÈTODO SIR -------------------------------------------------------------
# 1 gerar amostra de tamanho J da função de importância g(theta) que nesse caso é uma uniforme(0,1) (nossa propria a prioir, deveria ser algo próximo pois varre bem nosso epaço parametrico)
set.seed(2745)
vetor_thetas <-rbeta(200000, 1, 1)
# 2 para cada theta_i, i = 1 até J, calcule wi e os pesos qi
#f_x_dado_theta é o nucleo da verossimilhança vezes a apriori
w <- c()
for (i in vetor_thetas) {
  w = c(w,((((i^(soma_n))*(1 - i)^(n - soma_n))*(((gamma(alpha + beta))/(gamma(alpha)*gamma(beta)))*((i^(alpha - 1))*(1 - i)^(beta - 1))))/((gamma(alpha2 + beta2))/(gamma(alpha2)*gamma(beta2)))*((i^(alpha2 - 1))*(1 - i)^(beta2 - 1))))
}
w # = (f_x_dado_theta*pi_theta)/g_theta

q <- w/sum(w)
stopifnot(sum(q) >= 0.999)#garantir que a soma das probabilidades resulte em 1

# 3 selecionar, de um vetor de amostras theta*, uma amostra com reposicao assumindo que a P(theta=thetai) = qi 
u <- runif(20000, 0, 1)



if( x1 < u < x2){
  x1 = 0
  x2 = vetor_q[1]
  vetor_theta_estrela <- J[1]
  x1 = x2
  x2 += x2 
}

funcao <- function(x1, x2, u){
  for(i in u){
    x1 < i < x2
    vetor_theta_estrela = J[i]
  }
}
map(
  .x = J,
  .f = funcao
)

#é recomendado que T = J/20

# 3 utilizando o sample
amostra <- sample(vetor_thetas, size = length(vetor_thetas)/20, replace = TRUE, prob = q)
amostra
hist(amostra, probability = T, xlim = c(0,1))
curve(dbeta(x, 7, 5), add = T, col = "red")

