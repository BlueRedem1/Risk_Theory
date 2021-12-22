library(kdensity)
set.seed(6)

Y <- function(X,d){
  n = length(X)
  l <- c()
  for(i in 1:n){
    if(X[i]>d){
      l[i] = X[i]
    }else{l[i] = 0}
  }
  return(l)
}

X <- rexp(100000, 1/100)
Ysamp <- Y(X,50)

print("Media Muestral de Y:")
print(mean(Ysamp))

print("Mediana Muestral de Y:")
print(quantile(Ysamp, 0.5))

# Densidad [Poner en el inciso 4-b]
kde = kdensity(Ysamp)
plot(kde)

