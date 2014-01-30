#------------------------------------------------------------
###FUNCION DE UTILIDAD##
#------------------------------------------------------------

rm(list=ls(all=TRUE))

###################################################################################
#buena9000000.cvs archivo a cargar
#################################################################################

salida=read.csv(file.choose(),header=TRUE) #muestra MCMC para beta , eta
salida=as.matrix(salida)

#MUESTRA MCMC SELECCIONADA
#aceptado datos cada 50, selecciono una muestra de 200.

ac=50
i=1
j=500000
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
###############################################################################
#--------------------------------------------------------------------------
# DATOS INICIALES
#---------------------------------------------------------------------------

inicia=seq(1,50,1) # numero de transformadores al incio de cada periodo
deltas=seq(6,12,1) #periodos de  variacion de las deltas

promedio=matrix(c(rep(0,length(deltas)*length(inicia))),nrow=length(deltas),ncol=length(inicia)) #almacena los promedios de las areas negativas, es decir, la utilidad esperada
#promedioA=matrix(c(rep(0,length(deltas)*length(inicia))),nrow=length(deltas),ncol=length(inicia)) #almacena los promedios por arriba
#PromedioJ=matrix(c(rep(0,length(deltas)*length(inicia))),nrow=length(deltas),ncol=length(inicia))#almacena el promedio de las dos areas
for (vdeltas in 1:length(deltas))
{
delta=deltas[vdeltas]  # periodo de espera para recibir el siguiente transformador

for (has in 1:length(inicia))
{
ks=c()
p=10 #simulaciones de trayectorias
k_inicial=inicia[has] # numero de transformadores al inicio del periodo.
t=480 # tiempo de observacion
#####################################################################################

#SIMULAMOS TIEMPOS DE VIDA USANDO LA MUESTRAS MCMC

tiempos=NULL

for (i in 1:200)
{
  tiempos[i]=round(rweibull(1,shape=bs[i],scale=es[i]),0)
}
tiempos=sort(tiempos)     #Para facilitar los calculos ordenamos los tiempos de vida.
tam=length(tiempos)-1     #tamaño del vector de tiempos de vida
k=k_inicial
pila<-c()                 #En la variable pila se almacenaran los tiempos para los cuales tendremo                       #transformador que pedimos, si t=10 entonces pila=10 + delta.
tiem=c()                  #Almacena todos los tiempos de vida menores o iguales a tiempo                    #obtenido
j=1
h=1
sumas=NULL
sumasArriba=NULL
junto=NULL
while(tiempos[h]<=t)
{
  
  tiem[j]=tiempos[h]
  pila[j]=tiem[j]+delta
  aux=tiempos[h]
  tiempos[h]=rweibull(1,shape=bs[h],scale=es[h])+aux #reemplazo por un nuevo transformador.
  tiempos=tiempos[1:tam]           
  tiempos=round(sort(tiempos),0) #vuelvo a ordenar los tiempos de falla
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
    x[r+1]=tiem[cont1]
    y[r]=k
    y[r+1]=k
    cont1=cont1+1;cont2=cont2+1;r=r+2 
  }   
}

####################################################################
#OBTENER EL AREA PARA LOS VALORES NEGATIVOS
x1=c(x)
y2=c(y)
cuenta=3
suma=0
while(cuenta<length(x1))
  {
    if (y2[cuenta]<0)
    {
      suma=suma + (x1[cuenta+1]-x1[cuenta])*(abs(y2[cuenta]))
      
    }
    cuenta=cuenta+2
    #else { cuenta=cuenta+2 }
  }

####################################################################
sumas[1]=suma
jj=1
ks[jj]=k
####################################################################
#plot(x,y,type="l",main="Almacenamiento de transformadores",xlab="Tiempos",ylab="Número de transformadores",ylim=c(-5,k_inicial))
#abline(0,0,col="red")
###################################################################
#para obtener las areas por arriba:
cuenta=2
sumaA=0
while(cuenta<length(x1))
{
  if (y2[cuenta]>0)
  {
    sumaA=sumaA + (x1[cuenta]-x1[cuenta-1])*(y2[cuenta])
    
  }
  cuenta=cuenta+2
  
}
###################################################################
sumasArriba[1]=sumaA
junto[1]=suma+sumaA
#########################################################################



for (jj in 2:p)
{
  
  tiempos=NULL
  for (i in 1:200)
  {
    tiempos[i]=round(rweibull(1,shape=bs[i],scale=es[i]),0)
  }
  tiempos=sort(tiempos) #Para facilitar los calculos ordenamos los tiempos de vida.
  tam=length(tiempos)-1     #tamaño del vector de tiempos de vida
  k=k_inicial
  pila<-c()                #En la variable pila se almacenaran los tiempos para los cuales tendremo                       #transformador que pedimos, si t=10 entonces pila=10 + delta.
  tiem=c()                #Almacena todos los tiempos de vida menores o iguales a tiempo                    #obtenido
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
      x[r+1]=tiem[cont1]
      y[r]=k
      y[r+1]=k
      cont1=cont1+1;cont2=cont2+1;r=r+2 
    }   
  }
  
  ####################################################################
  #OBTENER EL AREA PARA LOS VALORES NEGATIVOS
  x1=c(x)
  y2=c(y)
  cuenta=3
  suma=0
  costo=600
  while(cuenta<length(x1))
  {
    if (y2[cuenta]<0)
    {
      suma=suma + (x1[cuenta+1]-x1[cuenta])*(abs(y2[cuenta]))*costo
      
    }
    cuenta=cuenta+2
    #else { cuenta=cuenta+2 }
  }
  ####################################################################
#para obtener las areas por arriba:
  cuenta=2
  sumaA=0
  while(cuenta<length(x1))
  {
    if (y2[cuenta]>0)
    {
      sumaA=sumaA + (x1[cuenta]-x1[cuenta-1])*(y2[cuenta])*cc
      
    }
    cuenta=cuenta+2
    
  }
  ###################################################################
  sumas[jj]=suma 
#lines(x,y,type="l",col=jj)   
}

promedio[vdeltas,has]=mean(sumas)

} #fin for de has que hace variar los periodos

} # fin for de vdeltas que hace variar las deltas


#el codigo tarda dos o tres horas en correr,
#el archivo con la salida del programa esta guardado con el
#nombre de deltas.cvs en esta misma carpeta

promedio=read.csv(file.choose(),header=FALSE)
promedio=as.matrix(promedio)
inicia=seq(1,23,1)


#hacemos las graficas:
#-----------------------------------------------------------------

#Grafica que describe la cantidad de las areas bajo las trayectorias
#que se encuentran por debajo de cero a lo largo del periodo analizado
#para un valor de delta=6 meses, el eje x representa para distintos valores
#de transformadores inciales desde 1 hasta 23, el eje x representa lo mismo 
#en las demas graficas y lo que se varia es la delta
par(lab=c(20,10,10))
plot(inicia,promedio[1,],pch=19,col="red",xlab="Transformadores iniciales",ylab="Transformadores faltantes",
     main="Utilidad Esperada", cex.lab=1.5, cex.main=2)
grid()

#aqui haremos una grafica para la presentacion, en base a como quedara
#la grafica considerando los dos costos los de arriba y los de abajo
#mues=c(1029.433,697.822,437.725,251.263,131.085,62.514,27.148,
 #      11.136,4.003, 1.343, 0.446, 0.167, .8, 4, 0.00,10,
#       30,100, 180, 400, 600, 800,1000 )
#par(lab=c(20,10,10))
#plot(inicia,mues,pch=19,col="red",xlab="Transformadores iniciales",ylab="Número de meses sin transformadores",main="Utilidad esperada")
#grid()
#abline(v=13)
#----------------------------------------------------------------

#grafica
#delta=7
plot(inicia,promedio[2,],pch=16,col="red",xlab="Transformadores iniciales",ylab="")
grid()
par(lab=c(20,10,10))
promedio[2,]
#----------------------------------------------------------------

#delta=8
par(lab=c(20,10,10))
plot(inicia,promedio[3,],pch=19,col="red",xlab="Transformadores iniciales",ylab="", main="Utilidad Esperada", cex.lab=1.5, cex.main=2)
grid()
promedio[3,]
#----------------------------------------------------------------

#delta=9
par(lab=c(20,10,10))
plot(inicia,promedio[4,],pch=16,col="red",xlab="Transformadores iniciales",ylab="")
grid()
promedio[4,]
#----------------------------------------------------------------

#delta=10
par(lab=c(20,10,10))
plot(inicia,promedio[5,],pch=16,col="red",xlab="Transformadores iniciales",ylab="")
grid()
promedio[5,]
#----------------------------------------------------------------

#delta=11
par(lab=c(20,10,10))
plot(inicia,promedio[6,],pch=16,col="red",xlab="Transformadores iniciales",ylab="")
grid()
promedio[6,]
#----------------------------------------------------------------

#delta=12 meses 
par(lab=c(20,10,10))
plot(inicia,promedio[7,],pch=19,col="red",xlab="Transformadores iniciales",ylab="Transformadores faltantes",main="Utilidad esperada",
     cex.lab=1.5, cex.main=2)
grid()

promedio[7,]


a=runif(1)
if(a>0.5){
  print("Bryan va por la libreta")
  }