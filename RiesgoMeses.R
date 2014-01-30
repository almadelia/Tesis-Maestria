
#####################################
#Considerar la funcion de riesgo
#####################################
hazard=function(eta,beta,x)
{
  beta/eta*(x/eta)^(beta-1)  
}

a=log(0.05)/log(0.95)
beta=log(a)/log(8)
w=(-log(0.95))^(1/beta)
eta=60/w

beta
eta

x=seq(-5,600,2)
y=dweibull(x,shape=0.5,scale=eta)
qweibull(0.05,shape=beta,scale=eta)
qweibull(0.95,shape=beta,scale=eta)
y=hazard(eta,beta,x)
plot(x,y,xlim=c(-5,600),type="l",col="red")

par(mfrow=c(2,2))
etas<-c()
betas<-c()
for (i in 1:4)
{
  a1=25
  a2=10
  b1=beta/a1
  b2=eta/a2  
  beta1=rgamma(1,shape=a1,scale=b1)
  #betas[i]=beta1
  eta1=rgamma(1,shape=a2,scale=b2)
  #etas[i]=eta1
  y=sort(rweibull(1000, shape=beta1, scale=eta1))
  riesgo=hazard(eta1,beta1,y)
  plot(y,riesgo,xlim=c(-5,600),type="l",yaxs="i", xaxs="i",
       main="",xlab="Tiempo",ylab="",
       cex.lab=2,cex.main=2,lwd=6)
  grid()
}




x9=y
y9=riesgo
par(mfrow=c(2,2))
plot(x6,y6,xlim=c(-5,600),type="l",col="blue",xlab="T",ylab="Riesgo")
plot(x7,y7,xlim=c(-5,600),type="l",col="blue",xlab="T",ylab="Riesgo")
plot(x8,y8,xlim=c(-5,600),type="l",col="blue",xlab="T",ylab="Riesgo")
plot(x9,y9,xlim=c(-5,600),type="l",col="blue",xlab="T",ylab="Riesgo")


betas
etas
a=18
a2=12
c(a1,b1,a2,b2)


y=sort(rweibull(500, shape=5, scale=5))
riesgo=hazard(4,3,y)
plot(y,riesgo,xlim=c(-5,50),type="l",col=i)

#########################################
etas
betas

#########################################
a1=10
a2=10
b1=0.19
b2=2.28


a1=15
a2=10
