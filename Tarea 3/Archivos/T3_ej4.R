

##Probabilidades reales
Panjer <- function(x,p0,a,b,f){
  
  # x := Valor al que se le desea calcular la probabilidad de P[S=x]
  #p0 := Probabilidad en 0 de N
  # (a,b) := Parametros de la familia (a,b,0)
  #f := vector de probabilidades de X
  
  #Creamos un vector auxiliar para las probas de S.
  g<-0:x
  names(g)<-0:x
  
  #Le ponemos nombres al vector de probas de f.
  names(f)<-1:length(f)
  
  #Panjer (de clase (a,b,0) con soporte de Xj sin 0)
  for(s in 0:x){
    if(s==0){
      g["0"]=p0
    }else{aux = 0
    for(j in 1:(min(s,length(f)-1))){
      aux = aux + (a + ((b*j)/s))*f[as.character(j)]*g[as.character(s-j)]
    }
    g[as.character(s)]=aux
    }
  }
  names(g)<-paste("P[S=",names(g),"]",sep = "")
  
  return(g)
}

n<- 10
p<- 0.4
a<- -p/(1-p)
b<- ((n+1)*p)/(1-p)
p0<- dbinom(x=0, size=n, prob = p)
f<- c(0.4, 0.35, 0.25,0,0,0,0,0,0,0,0,0,0,0,0)

fS<- Panjer(x=6,p0=p0,a=a,b=b,f=f)

print("Mostramos las probabilidades calculadas:")
print(fS)

# (a)
sprintf("P[S<=5] = %f", sum(Panjer(x=5,p0=p0,a=a,b=b,f=f)))

# (b)
sprintf("P[S=6] = %f", fS[7])


