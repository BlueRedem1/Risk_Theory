# Función asociada para la primera ecuación, buscamos la d t.q. se haga 0
f <- function(d){
  x = 0
  for(i in 0:500){
    k<-min(100000*i, d)
    p <- 0.15^i
    q <- 0.85^(500-i)
    c <- choose(500,i)
    x = x + (k*c*p*q)
  }f
  return(x-7000000)
}
# Función asociada para la segunda ecuación, buscamos la d t.q. se haga 0
g<- function(d){
  x = 0
  for(i in 0:500){
    m <- max(100000*i-d,0)
    p <- 0.15^i
    q <- 0.85^(500-i)
    c <- choose(500,i)
    x = x+ (m*c*p*q)
  }
  return(x-500000)
}

# Buscar las raices de ambas funciones
r1<- uniroot(f,lower = 0, upper = 100000*500 ,tol = 1e-8)$root
r2<- uniroot(g,lower = 0, upper = 100000*500 ,tol = 1e-8)$root

print("d que satisface la ecuación 1:")
print(r1)
print("Valor de f en r1:")
print(f(r1))
print("d que satisface la ecuación 2:")
print(r2)
print("Valor de g en r2:")
print(g(r2))

"
En efecto, ambas raices coinciden y al aplicarlas en f y g, respectivamente,
ambas se acercan a 0.
"
