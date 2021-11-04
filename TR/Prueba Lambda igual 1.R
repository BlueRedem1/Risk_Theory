library(actuar)
gumbel<-function(x, alpha = 1, scale = 0){
  exp(-exp(-lambda * x))} #Definimos la funcion de distribucion obtenida en el 
#inciso a  
lambda<-1  #Tomamos el valor de lambda de nuestro agrado
n<-1000 #Tomamos n suficientemente grande
a<-1   #Definimos el valor de la constante a_n
b<-log(n)/lambda   #Definimos el valor de la constante b_n

set.seed(27)  #Colocamos una semilla
sim<-(replicate(n=10000,
                expr=max(rexp(n=n,rate=lambda)))-b)/a

#Realizamos las 3 pruebas para verificar la distribución deseada

ks.test(unique(sim),"gumbel") #Kolmogorov-Smirnov

goftest::ad.test(sim,null="gumbel", alpha = 1, scale = 0) #Anderson-Darling

corte<- cut(x=sim,breaks=seq(min(sim),max(sim)+2,2)) #Definimos los cortes para 
observados<-table(corte);observados                  #partir nuestros datos
length(observados)
int1<-gumbel(-0.252)-gumbel(-3)
int2<-gumbel(1.75)-gumbel(-0.252)
int3<-gumbel(3.75)-gumbel(1.75)
int4<-gumbel(5.75)-gumbel(3.75)
int5<-gumbel(7.75)-gumbel(5.75)
int6<-gumbel(9.75)-gumbel(7.75)
int7<-gumbel(11.7)-gumbel(9.75)
int8<-gumbel(13.7)-gumbel(11.7)
int9<-gumbel(Inf)-gumbel(13.7)
esperados<-c(int1,int2,int3,int4,int5,int6,int7,int8, int9)
chisq.test(x=observados,p=esperados) #Ji-cuadrada

#Realizamos los plots 

plot(gumbel,from=-3,to=15,col="slateblue2",lwd=2,
     main="Distribución\n Teórica vs Empírica",
     col.main="sienna1",ylab=expression(F[X](x))) # Teórica

plot(ecdf(sim),verticals=T,xlim=c(-3,15),ad=T,col="darkgoldenrod1",lwd=1.5,lty=2)

legend("bottomright", 
       lty=1,lwd=c(1.5),legend=c("Teórica","Empírica"), col=c("slateblue2","darkgoldenrod1"))