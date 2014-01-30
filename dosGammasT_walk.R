

#------------------------------------------------------------------
#MUESTRA T-WALK PARA LOS DATOS 
#------------------------------------------------------------------


rm(list=ls(all=TRUE))
library(Rtwalk) # libreria t-walk

#Leemos la base de datos:
####################################################################
dato=read.csv(file.choose(),header=TRUE) #estos datos son los tiempos de vida de los transformadores, en su
#caso serian los resultados de la variable respuesta. Los tiempo de vida con los que 
#trabaje son weibull con parametros beta y eta 
dato=as.matrix(dato)
n=length(dato)/2
#Yo propuse distribuciones apriori para beta y eta como gammas con parametros eta-gamma(a1,b1), beta-gamma(a2,b2)
betae=1.9
eta=273.9
a1=25
a2=10
b1=betae/a1
b2=eta/a2
#########################################################################
#´Las funciones que se definen a continuacion, son para llamar a la funcion t-walk
#inicializamos la funcion: 

WeibullnitG <- function() 
{
  npars <<- 2 ## beta y eta  
  #suma de los logaritmos, estos son los estadisticos suficientes para la verosimilitud
  r1<<-sum(1-dato[,2])
  r2<<-0
  for (i in 1:n)
  {
    if (dato[i,2]==0)
    {
      r2<<-r2+log(dato[i,1]) 
    }   
  }
}

# Esta solo es una funcion auxiliar que la funcion WeibPostG no sea tan larga
sutnbG=function(be,eta)
{
  aux=0
  for (i in 1:n)
  {
    aux=aux+(dato[i,1]/eta)^(be)  
  } 
  aux 
}

### escribimos la -logposterior 
WeibPostG <- function(x) 
{
  be <- x[1]
  eta <-x[2]
  aux=-r1*(log(be)-be*log(eta))+(1-be)*r2+sutnbG(be,eta)+a1*log(b1)+log(gamma(a1))+(1-a1)*log(b1)+a2*log(b2)+log(gamma(a2))+(1-a2)*log(eta)+be/b1+eta/b2
 return(aux)
  }

#especificamos el soporte
WeibsuppG <- function(x) 
{
  
  (x[1]>0 & x[2]>0)
}

#fijamos dos puntos iniciales dentro del dominio de mi funcion en este caso dentro del dominio de las distribuciones
#gammas que he propuesto

WeibX0G <- function(x) 
  { c( rgamma(1,shape=a1,scale=b1), rgamma(1, shape=a2, scale = b2))}



WeibullnitG()
#info llama a la funcion t-walk
#dim= numero de parametros, Tr=interaciones, Obj=objetivo, Supp=soporte o dominio de la funcion, X0,xp0 son 
#puntos iniciales
info <- Runtwalk(dim=npars, Tr=900000,  Obj=WeibPostG, Supp=WeibsuppG, x0=WeibX0G(), xp0=WeibX0G())

par(mfrow=c(1,2))
#construye histogramas de la muestra obtenida para cada parametro
#es importante quitar el periodo de calentamiento, aki por ejemplo le quite a la muestra los
#primeros 1000 datos, y despues de esos hice el histograma.
PlotHist(info, par=1, from=1000, freq=FALSE,xlab=expression(beta), cex.lab=2, cex.main=1.5,ylab="" )
vector=seq(1.5,4,0.1)
PlotHist(info, par=2, from=1000, freq=FALSE,xlab=expression(eta), cex.lab=2, cex.main=1.5,ylab="")
TS(info,to=90000)
par(mfrow=c(1,2))
PlotLogObj(info,to=40000)
PlotLogObj(info,from= 1000, to=80000)

Ana(info)

#guardamos la salida:
SaveOutput(info,file="buena9000000.csv")

Ana(info) 
###########################################

#Para la muestra de Tr=9000000
#los resultados obtenidos:

#Ratio of moved coodinates per it=
#  0.2744085 0.5760303 0.03655092 0.1166136 
#dim= 2 AcceptanceRatio= 0.4194968 MAPlogPost= -593.5536 IAT= 39.77579 IAT/dim= 19.8879 

#############################################
#muestra pequeña
#Ratio of moved coodinates per it=
#  0.2703762 0.5694591 0.03320562 0.1024096 
#dim= 2 AcceptanceRatio= 0.41486 MAPlogPost= -593.5538 IAT= 49.94529 IAT/dim= 24.97264 

densi<-function(x)
{
  dgamma(x,shape=a1,scale=b1)
}

#densi=dgamma(vector,shape=a1,scale=b1)
#curve(densi,1,4,add=TRUE)

###############una vez generadas la muestra de la distribuci\'on MCMC,
#


#abrimos la muestra buena900000

salida=read.csv(file.choose(),header=TRUE) #muestra MCMC para beta , eta
salida=data.matrix(salida)


M=length(salida)/2;M
mean(salida[,1])
mean(salida[,2])

# se pretende entonces hallar la confiabilidad posterior,

#usando solo las utlimas 4000 simulaciones obtenidas



pdfPosterior=function(x)
{
  ma=0  
  for (i in 1:M)
  {
    ma=ma+ dweibull(x,shape=salida[i,1],scale=salida[i,2])
  }
  
  return(ma/M)
  
}

secuencia=seq(0.1,800,1)
yy=pdfPosterior(secuencia)
#densidad predictica posterior
plot(secuencia,yy,type="l", cex.lab=2, cex.main=2, xlab="Tiempos",ylab="",lwd=6)  #densidad `posterior
grid()
#################################################################################
dar=c(8,20,32,56,80,116,152,164,176,188,272,296,308)
toto=c(8,20,32,56,80,94,107,116,119,139,140,144,146,152,159,160,161,164,167,172,176,183,
       188,203,205,214,215,216,218,225,272,274,275,281,284,286,287,288,296,298,299,300,
       308)

cdfPosterior<-function(x)
{
  www=0
  for (k in 1:M) 
  {
    www=www + pweibull(x,shape=salida[k,1],scale=salida[k,2])
  }
  return(www/M)
}

darcon=cdfPosterior(dar)
tota=cdfPosterior(toto)

plot(toto,1-tota,pch=19)
plot(dar,1-darcon,pch=19,main="Confiabilidad Posterior",xlab="Tiempo",ylab="", cex.lab=1.5, cex.main=1.5)
zz=cdfPosterior(secuencia)
#confiabilidad posterior de todo
plot(secuencia,1-zz,type="l",xlab="Tiempo",ylab="", cex.lab=2, cex.main=2, lwd=6) #funcion de confiabilidad
grid()
#cuantil<-function(qp)
#{
qp=0.05
  q=0
  i=1
  while(zz[i]<=qp)
  {
    q=zz[i]
    i=i+1
  }
  
 # return(q)
#}

cuantil(0.05)



Ccondicional<-function(x,to)
{
  (1-cdfPosterior(to+x))/(1-cdfPosterior(to))
}




dar=c(8,20,32,56,80,116,152,164,176,188,272,296,308)
t0=c(12,12*2,12*7,12*12,12*20)
e2=matrix(c(rep(0,length(dar)*length(t0))),nrow=length(dar),ncol=length(t0))

for(i in 1:length(dar))
{
  for (j in 1:length(t0))
  {
    e2[i,j]=Ccondicional(dar[i],t0[j])
  }
}








##################################################################################
#hallemos la condicional de dado que ha vivido un tiempo


CondEstimada=function(x,delta)
{
  (cdfPosterior(x+delta)-cdfPosterior(x))/(1-cdfPosterior(x))
}

se=seq(0,300,15)

#calculemos la probabilidad de falla en 36 meses adicionales 
#delos transformadores que siguen funconando
ww=CondEstimada(se,36)
plot(se,ww)

#estimados=CondEstimada(se,36)
#plot(se,estimados,type="l")



Remanente<-function(x,delta)
  {
  (1-cdfPosterior(x+delta))/(1-cdfPosterior(x))
  
  }



Rema=Remanente(se,36)

plot(se,Rema,type="l")



hazard=function(eta,beta,x)
  {
  beta/eta*(x/eta)^(beta-1)  
  }



riesgo<-function(x)
{
  aa=0
 for  (i in 1:M)
 {
   aa=hazard(salida[i,2],salida[i,1],x)+aa 
 }
  
  return(aa/M)
}
  
#funcion de riesgo
risk=riesgo(secuencia)
plot(secuencia,risk,type="l",xlab="Tiempo",ylab="", cex.lab=2, cex.main=2,lwd=6)
grid()

plot(salida[1038574:length(salida[,2]),2],salida[1038574:length(salida[,2]),1],pch=15, xlab=expression(beta),ylab=expression(eta))

1038574



                  
                  
