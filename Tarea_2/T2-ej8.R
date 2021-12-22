set.seed(2000)
P <- c() # Vector de la cantidad de individuos a los que se les paga
NP <- c() # Vector de la cantidad de individuos a los que NO se les paga
n = 10000  # Tamano de muestra

for(j in 1:n){
  X <- rexp(100,1/1000) # Simulamos una exp(1/1000)
  B<- c() # Vector que guarda las muestras Bernoulli del inciso a)
  B_<- c() # Vector que guarda las muestras Bernoulli del inciso b)
  for(i in 1:100){
    if(X[i]>100){
      B[i] = 1 # Sí se paga
      B_[i] = 0
    }else{
      B[i] = 0
      B_[i] = 1
    }
  }
 P[j] = sum(B) # Contar a cuantos se les pagó en ese intento
 NP[j] = sum(B_) # Contar a cuantos NO se les pagó en ese intento
}

# Notemos que en efecto, P + NP = c(100, 100, ..., 100)

"
INCISO C
"

# Los que paga la aseguradora

# Separar en las categorías que se pide
o1 = sum(P<=90)
o3 = sum(P>95)
o2 = n - o1 -o3
Oi = c(o1,o2,o3)
Pi=c()
# Probabilidades Teóricas
Pi[1] = pbinom(90, 100, 0.9048374)
Pi[2] = pbinom(95, 100, 0.9048374)-pbinom(90, 100, 0.9048374)
Pi[3] =1-pbinom(95, 100, 0.9048374)
# Prueba Chi2 para Binom(100, 0.9048374)
chisq.test(x = Oi, p=Pi)
"
Mostrar el valor del p-value (0.5381) y en general el output para la prueba Chi2, 
de esta forma fallamos en rechazarla hipótesis nula, es decir, podemos afirmar 
que la muestra sigue una distribución Binom(100, 0.9048374)
"
"
INCISO D
"

# Los que NO paga la aseguradora

# Separar en las categorías que se pide
o1_ = sum(NP<=5)
o3_ = sum(NP>10)
o2_ = n - o1_ -o3_
Oi_ = c(o1_,o2_,o3_)
Pi_=c()
# Probabilidades Teóricas
Pi_[1] = pbinom(5, 100, 1-0.9048374)
Pi_[2] = pbinom(10, 100, 1-0.9048374)-pbinom(5, 100, 1-0.9048374)
Pi_[3] =1-pbinom(10, 100, 1-0.9048374)
# Prueba Chi2 para Binom(100, 1-0.9048374)
chisq.test(x = Oi_, p=Pi_)
"
Mostrar el valor del p-value (0.4199) y en general el output para la prueba Chi2, 
de esta forma fallamos en rechazarla hipótesis nula, es decir, podemos afirmar 
que la muestra sigue una distribución Binom(100, 1- 0.9048374)
"
