
tiempos=c(0.730,0.812,0.868,2.786,3.064,3.401)
llegan=fallas+3
tiem=pila=NULL



tiempo=c(0,0.730,0.730, 0.812,0.812,0.868,0.868,2.786,2.786,
         3.064,3.064,3.401,3.401)

t=c(3,3,2,2,1,1,0,0,-1,-1,-2,-2,-3)

y=c(0,0,1,1,2,2,3,3,4,4,5,5,6)

#grafica de la pagina 43 

par(lab=c(20,10,10))
par(mfrow=c(1,2))
plot(tiempo,t,type="l",xlab="Tiempos",ylab="X(t)")
grid()
plot(tiempo+delta,y,type="l",xlab="Tiempos",ylab="Y(t)")
grid()
h=1
t=7
delta=3
j=1
while(tiempos[h]<=t & h<=length(tiempos))
{
  
  tiem[j]=tiempos[h]
  pila[j]=tiem[j]+3
  j=j+1
  h=h+1
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
k_inicial=3
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
plot(x,y,type="l",xlab="Tiempos",ylab="Z(Tiempos)",xaxs="i",cex.main=2,cex.lab=1.5)
grid()
w=NULL
x1=NULL
for (i in 1:length(y))
{
  
  if(y[i]<0)
  {
    w[i]=y[i]
    x1[i]=x[i]   
    
  }
  if (y[i]>=0)
  {
    w[i]=0
    x1[i]=x[i]
    
  }
}

plot(x1,w,type="l",xlab="Tiempos", ylab="W(Tiempos)",xaxs="i",cex.main=2,cex.lab=1.5)
grid()


