
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
eta=5/w

beta=2.3
eta=22

x=seq(-10,60,0.01)
y=dweibull(x,shape=beta,scale=eta)
plot(x,y,xlim=c(-5,50),type="l",col="red")
media=beta*gamma(1+1/eta)

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
  betas[i]=beta1
  eta1=rgamma(1,shape=a2,scale=b2)
  etas[i]=eta1
  y=sort(rweibull(1000, shape=beta1, scale=eta1))
  riesgo=hazard(eta1,beta1,y)
  plot(y,riesgo,xlim=c(-5,50),type="l",col="blue",main="Función de riesgo apriori",xlab="Tiempo",yla="",
       cex.lab=1.5, cex.main=2)
}


e
e1

w
w1

t
t1

u
u1
par(mfrow=c(2,2))
plot(e,e1,xlim=c(-5,50),type="l",col="blue",xlab="T",ylab="Riesgo")
plot(w,w1,xlim=c(-5,50),type="l",col="blue",xlab="T",ylab="Riesgo")
plot(t,t1,xlim=c(-5,50),type="l",col="blue",xlab="T",ylab="Riesgo")
plot(u,u1,xlim=c(-5,50),type="l",col="blue",xlab="T",ylab="Riesgo")


betas
etas
a=18
a2=12
c(a1,b1,a2,b2)


y=y=rweibull(50, shape=1.9, scale=22)
riesgo=hazard(1.9,22,y)
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
