"
TAREA-EXAMEN 4; Ejercicio 5:

* En este código de R encontrarás todo lo necesario para la resolución
del ejercicio 5 de la tarea examen 4. Para ello utilizamos la fórmula de Panjer
para el caso (a,b,1) con cero-modificado desarrollado en la tarea pasada (Tarea 3),
a su vez, utilizamos la fórmula desarrollada en clase para el cálculo de la prima
P bajo un riesgo S con función de utilidad, dado una alpha, v(x):= 1-exp(-alpha*x)
y es P = ln(M_S(alpha)) / alpha.
"



# Función Panjer Cero Modificado
PanjerConCeroModificado <- function(r,a,b,f,p0,p1,p0m,g0,todo=F){
  
  "
  PARAMETROS:
  ---------------------------------------------------------
  * r:= Valor de la probabilidad deseada a calcular
  * g0 := P[S=0]
  * (a,b) := Parametros de la familia (a,b,0)
  * f := vector de probabilidades de Y (ordenadas desde 0)
  * p0, p1 := Probilidad en 0 y 1, respectivamente de la
  variable de la familia (a,b,0)
  * p0m := Probabilidad en 0 de la variable N*
  * todo := Booleano que indica si mostramos todas las
  probabilidades calculadas o sólo la deseada (r)
  "
  
  #Creamos un vector auxiliar para las probas de S.
  g<-0:r
  names(g)<-0:r
  
  #Le ponemos nombres al vector de probas de f.
  names(f)<-0:(length(f)-1)
  
  # Panjer cero modificado
  
  for(s in 0:r){
    if(s==0){
      g["0"]=g0
    }else{aux = 0
    p1m <- ((1-p0m)/(1-p0))*p1 # P[N* = 1]
    for(j in 1:(min(s,length(f)-1))){
      aux = aux + ((a+b*j/s)*f[as.character(j)]*g[as.character(s-j)])/(1-a*f["0"])
    }
    aux2 = (p1m - (a+b)*p0m)*f[as.character(s)] / (1-a*f["0"])
    g[as.character(s)]=aux + aux2
    }
  }
  names(g)<-paste("P[S=",names(g),"]",sep = "")
  
  if(todo){
    return(g)
  }else{
    return(g[r+1])
  }
}

# Generadora de probabilidades de una binomial n,p en t
Gn<- function(t,n,p){
  x<- (1-p+p*t)^n
  return(x)
}

# Función de utilidad con alpha = 0.21

v<-function(x){
  y = 1- exp(-0.21*x)
  return(y)
}

# Capital Inicial
u <- 10

f<- c(0.4,0.2,0.1,0.15,0.15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# Colocamos muchos 0, pues si la r que le damos a la función pasa la cantidad de
# elementos en el vector entonces obtenemos NAN, y como la densidad de Y es 0 
# fuera del soporte, entonces dicho vector conserva las propiedades de la 
# densidad de Y.

n<- 3
p<- 1/3
a<- -p/(1-p)
b<- ((n+1)*p)/(1-p)
p0<- dbinom(x=0, size=n, prob=p) # Sabiendo que N~Binom(3, 1/3)
p1<- dbinom(x=1, size=n, prob=p)
p0m<- 0.7
g0<- 1 - (1-p0m)/(1-p0)*(1-Gn(t = f[1], n=n, p=p))

fS<-PanjerConCeroModificado(r=12,a=a,b=b,f=f,p0=p0,p1=p1,p0m=p0m,g0=g0,todo=T)
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
sprintf("Valor teórico: %f", 1.45*(81/190))

# Obtener la prima por medio de la fórmula con v(x) = 1-exp(-x*\alpha)
mgf = 0
for(k in 0:12){
  mgf = mgf + exp(0.21*k)*prob[k+1]
}


# Inciso a)
P = log(mgf)/0.21
sprintf("Prima: %f", P)

# Inciso b)
(media<=P)

# Inciso c)
(P <= length(fS)-1) # P <= 12?

# Inciso d)

# Ev_u_P_S= 0
# for(k in 0:12){
#   Ev_u_P_S = Ev_u_P_S + (1-exp(-0.21*(10+P-k)))*prob[k+1]
# }

Ev_u_P_S= 0
for(s in 0:12){
  Ev_u_P_S = Ev_u_P_S + v(u+P-s)*prob[s+1]
}



sprintf("E[v(u+P-S)] = E[1-exp(-0.21*(u+P-S))]: %f", Ev_u_P_S)
sprintf("v(u) = 1-exp(-0.21*10): %f", v(u))

(v(u) == Ev_u_P_S)

