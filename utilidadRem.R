#########################
###FUNCION DE UTILIDAD##
###################################################################################

rm(list=ls(all=TRUE))
salida=read.csv(file.choose(),header=TRUE) #muestra MCMC para beta , eta
salida=as.matrix(salida)
#MUESTRA MCMC SELECCIONADA
#aceptado datos cada 50, selecciono una muestra de 173.

ac=50
i=1
j=0
bs=c()
es=c()
while (i<=200)
{
  j=ac+j
  bs[i]=salida[j,1]
  es[i]=salida[j,2]
  i=i+1 
}
#los vectores bs y es contienen los datos que deseo simular
bs
es
###########################################################################
##################################################################################
#DATOS INICIALES


ks=c()
p=3 #simulaciones de trayectorias
k_inicial=5 # numero de transformadores al inicio del periodo.
t=200 # tiempo de observacion
delta=6 # periodo de espera para recibir el siguiente transformador
#####################################################################################

#SIMULAMOS TIEMPOS DE VIDA USANDO LA MUESTRAS MCMC

tiempos=NULL
for (i in 1:200)
{
  tiempos[i]=round(rweibull(1,shape=bs[i],scale=es[i]),0)
}
tiempos=sort(tiempos) #Para facilitar los calculos ordenamos los tiempos de vida.
tam=length(tiempos)-1     #tamaño del vector de tiempos de vida
k=k_inicial
pila<-c()                #En la variable pila se almacenaran los tiempos para los cuales tendremo
                         #transformador que pedimos, si t=10 entonces pila=10 + delta.
tiem=c()                #Almacena todos los tiempos de vida menores o iguales a tiempo
                        #obtenido
j=1
h=1
while(tiempos[h]<=t)
{
  
  tiem[j]=tiempos[h]
  pila[j]=tiem[j]+delta
  aux=tiempos[h]
  tiempos[h]=rweibull(1,shape=bs[h],scale=es[h])+aux
  tiempos=tiempos[1:tam]           
  tiempos=round(sort(tiempos),0)
  j=j+1
  k=k-1
}
pila
tiem

pila=c(pila,0)
pil=c()
s=1
while(pila[s]<=t && pila[s]!=0)
{
  pil[s]=pila[s]
  k=k+1 
  s=s+1
}

#para hacer la grafica

k=k_inicial;x=c();y=c();r=1
x[r]=0;y[r]=k;r=2;pil=c(pil,0);tiem=c(tiem,0)
cont1=1;cont2=1;r=2

while( tiem[cont1]!=0  || pil[cont2]!=0)
{
  if ((tiem[cont1]<pil[cont2] && tiem[cont1]!=0 && pil[cont2]!=0) || (tiem[cont1]!=0 && pil[cont2]==0))
  {
    x[r]=x[r+1]=tiem[cont1]
    y[r]=k;k=k-1;y[r+1]=k
    cont1=cont1+1;r=r+2
  }
  
  if ((tiem[cont1]>pil[cont2] && tiem[cont1]!=0 & pil[cont2]!=0) || (tiem[cont1]==0 && pil[cont2]!=0))
  {
    x[r]=pil[cont2]
    x[r+1]=pil[cont2]
    y[r]=k;k=k+1
    y[r+1]=k
    cont2=cont2+1;r=r+2  
  }
  
  if (tiem[cont1]==pila[cont2] && tiem[cont1]!=0 &&  pil[cont2]!=0 )
  {
    x[r]=tiem[cont1]
    y[r]=k
    cont1=cont1+1;cont2=cont2+1   
  }   
}
jj=1
ks[jj]=k
#x=seq(0,10,1)
#y=rep(0,11)
#par(lab=c(20,10,10))

plot(x,y,type="l",lwd=3,main="Almacenamiento de transformadores",xlab="Tiempos",ylab="",cex.lab=2, cex.main=2)
#grid()
abline(0,0)
###################################################################
 for (jj in 2:p)
 {
  tiempos=NULL
   for (i in 1:173)
    {
    tiempos[i]=round(rweibull(1,shape=bs[i],scale=es[i]),0)
    }
  tiempos=c(sort(tiempos),0)# simulaciones de los tiempos de vida
  k=k_inicial
  pila<-c() 
  tiem=c()
  j=1
 while(tiempos[j]<=t && tiempos[j]!=0)
  {
  pila[j]=tiempos[j]+delta
  tiem[j]=tiempos[j]
  j=j+1
  k=k-1
 }
 pila=c(pila,0)
 pil=c()
 s=1
 while(pila[s]<=t && pila[s]!=0)
  {
   pil[s]=pila[s]
   k=k+1 
   s=s+1
   }

#para hacer la grafica

k=k_inicial;x=c();y=c();r=1
x[r]=0;y[r]=k;r=2;pil=c(pil,0);tiem=c(tiem,0)
cont1=1;cont2=1;r=2

while( tiem[cont1]!=0  || pil[cont2]!=0)
{
  if ((tiem[cont1]<pil[cont2] && tiem[cont1]!=0 && pil[cont2]!=0) || (tiem[cont1]!=0 && pil[cont2]==0))
  {
    x[r]=x[r+1]=tiem[cont1]
    y[r]=k;k=k-1;y[r+1]=k
    cont1=cont1+1;r=r+2
  }
  
  if ((tiem[cont1]>pil[cont2] && tiem[cont1]!=0 & pil[cont2]!=0) || (tiem[cont1]==0 && pil[cont2]!=0))
  {
    x[r]=pil[cont2]
    x[r+1]=pil[cont2]
    y[r]=k;k=k+1
    y[r+1]=k
    cont2=cont2+1;r=r+2  
  }
  
  if (tiem[cont1]==pila[cont2] && tiem[cont1]!=0 &&  pil[cont2]!=0 )
  {
    x[r]=tiem[cont1]
    y[r]=k
    cont1=cont1+1;cont2=cont2+1   
  }   
}
ks[jj]=k
lines(x,y,type="l",col=jj,lwd=3)

}
