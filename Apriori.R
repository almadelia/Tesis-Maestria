


# Encontrar la apriori solo la apriori

a=log(0.05)/log(0.95)
beta=log(a)/log(8)
w=(-log(0.95))^(1/beta)
eta=5/w

beta=2.3
eta=22

x=seq(-10,60,0.01)
y=dweibull(x,shape=beta,scale=eta)
qweibull(0.05,shape=beta,scale=eta)
qweibull(0.95,shape=beta,scale=eta)
plot(x,y,xlim=c(-5,50),type="l",col="red")
media=beta*gamma(1+1/eta)

#a1=runif(1,0.5,2)
#a2=runif(17,)
par(mfrow=c(2,2))
etas<-c()
betas<-c()
q5=c()
q95<-c()
for (i in 1:4)
{
  a1=25
  a2=10
  b1=beta/a1
  b2=eta/a2  
  beta1=rgamma(1,shape=a1,scale=b1)
  etas[i]=eta1
  eta1=rgamma(1,shape=a2,scale=b2)
  betas[i]=beta1
  y=dweibull(x, shape=beta1, scale=eta1)
  q5[i]=qweibull(0.05,shape=beta1,scale=eta1)
  q95[i]=qweibull(0.95,shape=beta1,scale=eta1)
  plot(x,y,xlim=c(-5,50),type="l",col=i)
}
q5
q95


#########################################
etas
betas

#########################################
a1=10
a2=10
##########################################

a1=15
a2=10


