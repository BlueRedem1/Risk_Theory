"
Tarea 3 - Ejercicio 6
" 

# Función de severidad dado un siniestro X, con un monto máximo de beneficio u 
# y deducible d.
Y<-function(X,u=4,d=2){
  y=max(min(X,u)-d,0)
  return(y)
}

# Función para encontrar la moda en una lista.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Función para simular la v.a. S
regresa.una.S<-function(m,f,size=4,p = 1/5){
  
  #Genera una N
  N=0
  bandera<-sample(x = 0:1,size = 1,replace = T,prob = c(0.7, 0.3)) # Considerando
  # que la probabilidad de ser 0 para la frecuencia N* es 0.7
  
  if(bandera!=0){
    while(N==0){
      N<-rbinom(n=1,size=size, prob = p) # Recordemos que N sigue una dist Binom
    }
  }
  
  #Calcula los datos
  if(N>0){
    Xj <- sample(x = 1:m,size = N,
                 replace = T,prob = f) #Generar los siniestros
    Yj<-lapply(Xj,Y) # Aplicarles monto máximo u y deducible d
    Yj<- unlist(Yj)
  }else{
    Yj <- 0 #Si no hubo, el total es cero.
  }
  
  #Regresa una S
  return(sum(Yj))
  
}

m <- 5 # Pues el soporte de X es {1,2,3,4,5}
f <- c(7/18,5/18,3/18,2/18,1/18) # Probabilidades correspondientes

set.seed(93) # fijamos una semilla, 93
n = 1000000 # realizamos la S 1,000,000 de veces

samp <- c()
for(i in 1:n){
  samp[i] <- regresa.una.S(m,f)
}

# Calculo de algunas métricas
media_muestral <- mean(samp)
var_muestral <- var(samp)
semo_muestal <- var_muestral + media_muestral^2
sd_muestral <-  sd(samp)
var_90 <- quantile(samp,0.9)
coef_var_muestral <- sd_muestral / media_muestral
moda_muestral <- Mode(samp)

"
Resultados:
"

# Histograma:
barplot((table(samp)/length(samp)),col="skyblue",add=T,axisnames = F)

# (a) Media:
sprintf("Media muestral: %f",media_muestral)
# 0.201793

# (b) Segundo Momento:
sprintf("Segundo Momento muestral: %f", semo_muestal)
# 0.394095

# (c) Varianza:
sprintf("Varianza muestral: %f", var_muestral)
# 0.353375

# (d) Coeficiente de variación:
sprintf("Coeficiente de variación muestral: %f", coef_var_muestral)
# 2.945858

#########################################################################

"
A continuación mostraremos algunos resultado más con base en la muestra de 
1,000,000 de S generadas, los cuales no serán de utilidad para ejercicios 
posteriores:
"

# Var al 90%:
sprintf("VaR a un nivel de significancia del 90: %s", var_90)
# 1

# Moda:
sprintf("Moda de la muestra: %s", moda_muestral)
# 0
