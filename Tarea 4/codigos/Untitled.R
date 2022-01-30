####### EJERCICIO 6
severidad<-function(n){
  z<-runif(n)
  for(i in 1:n){
    if(z[i]<=0.1){
      z[i]=120
      next
    }
    else if(z[i]>0.1 && z[i]<=0.8){
      z[i]=150
      next
    }
    else if(z[i]>0.8){
      z[i]=200
      next
    }
  }
  return(z)
}
genera.una.S<-function(){
  n<-(rnbinom(1,5,5/100)+5)
  y<-severidad(n)
  return(sum(y))
}
sims=10000000
s<-replicate(sims,genera.una.S())
mean(s)
var(s)


funcion<-function(x){
  return(x^2)
}

###########EJERCICIO 3

momentos_y<-function(alpha){
  return((7/16)*exp(alpha*1)+(5/16)*exp(alpha*2)+
           (3/16)*exp(alpha*3)+(1/16)*exp(alpha*4))
}
momentos_s<-function(t,p,n){
  return((1-p+p*momentos_y(t))^n)
}
ejercicio_3_b<-function(alpha=0.5,p=0.8,n=3){
  return((1/alpha)*log(momentos_s(alpha,p,n)))
}
ejercicio_3_b()