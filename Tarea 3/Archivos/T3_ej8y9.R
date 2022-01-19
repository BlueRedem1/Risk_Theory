"
Tarea 3 - Ejercicios 8 y 9
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
  probabilidades calculadas o s?lo la deseada (r)
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


#Ejercicio: 

f<- c(12/18, 3/18, 3/18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# Colocamos muchos 0, pues si la r que le damos a la funci?n pasa la cantidad de
# elementos en el vector entonces obtenemos NAN, y como la densidad de Y es 0 
# fuera del soporte, entonces dicho vector conserva las propiedades de la 
# densidad de Y.

n<- 4
p<- 1/5
a<- -p/(1-p)
b<- ((n+1)*p)/(1-p)
p0<- dbinom(x=0, size=n, prob=p) # Sabiendo que N~Binom(4, 1/5)
p1<- dbinom(x=1, size=n, prob=p)
p0m<- 0.7
g0<- 1 - (1-p0m)/(1-p0)*(1-Gn(t = f[1], n=n, p=p))

fS<-PanjerConCeroModificado(r=8,a=a,b=b,f=f,p0=p0,p1=p1,p0m=p0m,g0=g0,todo=T)
print("Las probabilidades están dadas por:")
print(fS)

# Suma 1
sprintf("Veamos que en efecto, suman 1: %f", sum(fS))

prob<- unname(fS)
# Media

media = 0
for(i in 0:8){
  media = media + i*prob[i+1]
}

sprintf("Mostremos la media: %f", media)
sprintf("Valor teórico: %f", 25/123)

# Segundo momento:
semo = 0
for(i in 0:8){
  semo = semo + (i^2)*prob[i+1]
}

sprintf("Mostremos el segundo momento: %f", semo)
sprintf("Valor teórico: %f", 295/738)

# Varianza

varianza = 0
for(i in 0:8){
  varianza = varianza + ((i-media)^2)*prob[i+1]
}

sprintf("Mostremos la varianza: %f", varianza)
sprintf("Varianza teórica: %f", 1835/15129 + 175/738)

# Histograma
barplot(fS, ylab=expression(P(S==x)), 
        main="Panjer: Cero Modificado y cero en Sop(X)",col="gold")

# Comparemos con el histograma del ejercicio 6