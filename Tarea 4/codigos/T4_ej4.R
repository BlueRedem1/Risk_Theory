
"
TAREA-EXAMEN 4; Ejercicio 4:

* En este código de R encontrarás todo lo necesario para la resolución
del ejercicio 4 de la tarea examen 4. Para ello utilizamos la fórmula de Panjer
para el con cero en el soporte de la severidad, a su vez, utilizamos la función
newtonRaphson para encontrar la prima P bajo un siniestro S, dado un capital mínimo u, y
una función de utilidad cuadrática con nivel alpha v(x) = x - alpha*x^2.
"

library(pracma)

##Probabilidades reales
Panjer <- function(x,n,f,todo=F){
  
  #n := nÃºmero de pÃ³lizas
  #f := vector de probabilidades de X (ordenadas desde 0)
  
  #Creamos un vector auxiliar para las probas de S.
  g<-0:x
  names(g)<-0:x
  
  #Le ponemos nombres al vector de probas de f.
  names(f)<-0:(length(f)-1)
  
  #Formula de Panjer
  for(s in 0:x){
    if(s==0){
      g["0"]=f["0"]^n
    }else{aux = 0
    for(j in 1:(min(s,length(f)-1))){
      aux = aux + ((j*(n+1))/s - 1)*f[as.character(j)]*g[as.character(s-j)]
    }
    g[as.character(s)]=aux/f["0"]
    }
  }
  
  if(todo){
    return(g)
  }else{
    return(g[as.character(x)])
  }
  
}

# Función de utilidad. v(x)= x - alpha*x^2
v<-function(x){
  y = x - (x^2)/24
  return(y)
}

u = 10
#NÃºmero de pÃ³lizas
n<-4
#Vector de probabilidades
f<-c(0.4,0.3,0.2,0.1)
#Probabilidades
fS<-Panjer(x = 12,n = n,f = f,todo = T)
print("Las probabilidades están dadas por:")
print(fS)


# Suma 1
sprintf("Veamos que en efecto, suman 1: %f", sum(fS))

prob<- unname(fS)
# Media

media = 0
for(i in 0:12){
  media = media + i*prob[i+1]
}
sprintf("Mostremos la media: %f", media)

f<-function(P){
  x = 0
  for(s in 0:12){
    x = x + v(10+P-s)*prob[s+1]
  }
  y = v(10) - x
  return(y)
}


# Encontrar la prima P
P<- newtonRaphson(f,x0 = 1)$root
# Plot de la raíz de f.
plot(f, xlim = c(0,12))

# Inciso a)
sprintf("Prima: %f", P)

# Inciso b)
(media<=P)

# Inciso c)
(P <= length(fS)-1) # P <= 12?

# Inciso d)

Ev_u_P_S= 0
for(s in 0:12){
  Ev_u_P_S = Ev_u_P_S + v(u+P-s)*prob[s+1]
}


sprintf("E[v(u+P-S)] = E[1-exp(-0.21*(u+P-S))]: %f", Ev_u_P_S)
sprintf("v(u) = 1-exp(-0.21*10): %f", v(u))

(v(u) == Ev_u_P_S)
