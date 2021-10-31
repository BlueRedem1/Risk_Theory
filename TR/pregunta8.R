library(tidyverse)
Data<-read.csv('NumeroReclamos.csv')
Data %>%
  group_by(Numero.de.reclamos.mensuales) %>%
  summarise(n=length(X)) %>%
  ungroup()
Data2<-Data[,-1,drop=F]
Data3<-Data2 %>%
  count(Numero.de.reclamos.mensuales)
ggplot(Data3,aes(x=Numero.de.reclamos.mensuales,y=n))+geom_point()

#poisson
lambda=mean(Data$Numero.de.reclamos.mensuales)
poisson_ej<-function(x){
  return(dpois(x,lambda))
}
probas_p<-c(ppois(36,lambda=lambda))
x<-c(sum(Data3$n[1:3]))
for (i in 37:62){
  if(i==62){
    x[i-35]<-sum(Data3$n[(i-33):32])
    probas_p[i-35]<-ppois(i,lambda=lambda,lower.tail=F)+dpois(i,lambda=lambda)
    break
  }
  probas_p[i-35]<-dpois(i,lambda=lambda)
  x[i-35]<-Data3$n[i-33]
}
sum(probas_p)
sum(x)
freq<-as.data.frame(x)
chisq.test(freq$x,p=probas_p)
plot_probas<-c()
for (i in 1:100){
  plot_probas[i]<-poisson_ej(i)
}
plot_probas<-as.data.frame(plot_probas)
ggplot()+
  geom_point(data=Data3,aes(x=Numero.de.reclamos.mensuales,y=n/150))+
  geom_line(data=plot_probas,aes(y=plot_probas,x=1:100))

media<-mean(Data3$Numero.de.reclamos.mensuales)
varianza<-var(Data3$Numero.de.reclamos.mensuales)

##binomial
primer_momento<-mean(Data$Numero.de.reclamos.mensuales)
segundo_momento<-mean((Data$Numero.de.reclamos.mensuales)^2)
#por momentos
p=primer_momento/(primer_momento^2/(primer_momento^2+primer_momento-segundo_momento))
n=ceiling((primer_momento^2/(primer_momento^2+primer_momento-segundo_momento)))
probas_b<-c(pbinom(36,size=n,p=p))
x<-c(sum(Data3$n[1:3]))
for (i in 37:62){
  if(i==62){
    x[i-35]<-sum(Data3$n[(i-33):32])
    probas_b[i-35]<-pbinom(i,size=n,p=p,lower.tail=F)+dbinom(i,size=n,p=p)
    break
  }
  probas_b[i-35]<-dbinom(i,size=n,p=p)
  x[i-35]<-Data3$n[i-33]
}
sum(probas_b)
sum(x)
freq<-as.data.frame(x)
chisq.test(freq$x,p=probas_b)
plot_probas<-c()
for (i in 1:100){
  plot_probas[i]<-poisson_ej(i)
}
n*p*(1-p)
n*p
plot_probas<-c()
for (i in 1:100){
  plot_probas[i]<-dbinom(i,size=n,p=p)
}
plot_probas<-as.data.frame(plot_probas)
ggplot()+
  geom_point(data=Data3,aes(x=Numero.de.reclamos.mensuales,y=n/150))+
  geom_line(data=plot_probas,aes(y=plot_probas,x=1:100))

##negativa
p=1-(mean(Data3$Numero.de.reclamos.mensuales))/(var(Data3$Numero.de.reclamos.mensuales))
r<-ceiling((mean(Data3$Numero.de.reclamos.mensuales))^2/(var(Data3$Numero.de.reclamos.mensuales)-mean(Data3$Numero.de.reclamos.mensuales)))

probas_n<-c(pnbinom(36,size=r,p=p))
x<-c(sum(Data3$n[1:3]))
for (i in 37:62){
  if(i==62){
    x[i-35]<-sum(Data3$n[(i-33):32])
    probas_n[i-35]<-pnbinom(i,size=r,p=p,lower.tail=F)+dnbinom(i,size=r,p=p)
    break
  }
  probas_n[i-35]<-dnbinom(i,size=r,p=p)
  x[i-35]<-Data3$n[i-33]
}
sum(probas_n)
sum(x)
freq<-as.data.frame(x)
chisq.test(freq$x,p=probas_n)
plot_probas<-c()
n*p*(1-p)
n*p
plot_probas<-c()
for (i in 1:100){
  plot_probas[i]<-dnbinom(i,size=r,p=p)
}
plot_probas<-as.data.frame(plot_probas)
ggplot()+
  geom_point(data=Data3,aes(x=Numero.de.reclamos.mensuales,y=n/150))+
  geom_line(data=plot_probas,aes(y=plot_probas,x=1:100))


#inciso c
probas_teoricas<-c()
for (i in 45:55){
  probas_teoricas[i-44]<-poisson_ej(i)
}
Comparacion<-data.frame('Empiricas'=Data3$n[12:22]/150,'Teoricas'=probas_teoricas)

Comparacion<-Comparacion %>%
  mutate('Diferencia'=Empiricas-Teoricas)
max(abs(Comparacion$Diferencia))
which(Comparacion$Diferencia==max(abs(Comparacion$Diferencia)))
#Como i=1 es para el punto 45, i=2 es para el punto 46
mean(Comparacion$Diferencia)
plot(kernel((Data3$n)/150))
kernel()