library(kdensity)
set.seed(27)

Z <- function(X,a,d,u,b){
  n = length(X)
  l <- c()
  for(i in 1:n){
    if(a<= X[i] & X[i]<d){
      l[i] = X[i]
    }else{
      if(X[i]<=u){
        l[i] = d
      }else{
        l[i] = X[i] + d-u
      }
    }
  }
  return(l)
}

X <- rexp(100000, 1/100)

Zsamp <- Z(X,0,27,110,200) # Notemos que tanto a como b no se especificaron al inicio del problema

print("Media Muestral de Z:")
print(mean(Zsamp))

print("Mediana Muestral de Z:")
print(quantile(Zsamp, 0.5))


# Densidad [Poner en el inciso 2-b]
kde = kdensity(Zsamp)
plot(kde)
