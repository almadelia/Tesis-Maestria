


# Encontrar la apriori solo la apriori
a=log(0.05)/log(0.95)
betae=log(a)/log(8)
w=(-log(0.95))^(1/betae)
eta=60/w

betae
eta

x=seq(-5,600,2)
y=dweibull(x,shape=betae,scale=eta)
qweibull(0.05,shape=betae,scale=eta)
qweibull(0.95,shape=betae,scale=eta)

plot(x,y,xlim=c(0,600),type="l",yaxs="i",xaxs="i",main="",xlab="Tiempo ",ylab="",cex.lab=2, cex.main=2,lwd=5)
grid()

etas<-c()
betas<-c()
q5=c()
q95<-c()
a1=seq(1,30,0.1)
a2=seq(1,20,0.1)
for (i in 1:4)
{
  a1=25
  a2=12
  b1=betae/a1
  b2=eta/a2  
  beta1=rgamma(1,shape=a1,scale=b1)
  betas[i]=beta1
  eta1=rgamma(1,shape=a2,scale=b2)
  etas[i]=eta1
  y=dweibull(x, shape=beta1, scale=eta1)
  q5[i]=qweibull(0.05,shape=beta1,scale=eta1)
  q95[i]=qweibull(0.95,shape=beta1,scale=eta1)
  plot(x,y,xlim=c(-5,600),type="l",col="red",yaxs="i", xaxs="i",
       main="Distribución predictiva apriori",xlab="Tiempo",ylab="Densidad",
       cex.lab=1.5,cex.main=2)
  grid()
}



