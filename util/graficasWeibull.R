


hazard=function(eta,beta,x)
{
  beta/eta*(x/eta)^(beta-1)  
}




vector=sort(rweibull(100,scale=1,shape=0.5))
y=dweibull(vector,scale=1,shape=0.5 )
d=pweibull(vector,scale=1,shape=0.5)
r=hazard(1,0.5,vector)

vector1=sort(rweibull(100,scale=1,shape=1))
y1=dweibull(vector1,scale=1,shape=1 )
d1=pweibull(vector1,scale=1,shape=1)
r1=hazard(1,1,vector1)


vector2=sort(rweibull(100,scale=1,shape=1.5))
y2=dweibull(vector2,scale=1,shape=1.5 )
d2=pweibull(vector2,scale=1,shape=1.5)
r2=hazard(1,1.5,vector2)


vector3=sort(rweibull(100,scale=2,shape=5))
y3=dweibull(vector3,scale=2,shape=5 )
d3=pweibull(vector3,scale=2,shape=5)
r3=hazard(2,5,vector3)


#GRAFICAS-------------
##############densidades
plot(vector,y,type="l",xlim=c(0,2.5),ylim=c(-0.05,2.5),col=1,xlab="Tiempo",ylab="",cex.lab=2, cex.main=2,lwd=6,lty=3)
legend(1.5,2.5,bg="white",c(expression(paste(eta,"=",1,"",",","",beta,"=",0.5)),
                            expression(paste(eta,"=",1,"",",","",beta,"=",1)),
                            expression(paste(eta,"=",1,"",",","",beta,"=",1.5)),
                            expression(paste(eta,"=",2,"",",","",beta,"=",5))),
       col=1:4,lwd=6,lty=3:6)
lines(vector1,y1,xlim=c(0,2.5),ylim=c(0,2.5),col=2,lwd=6,lty=4)
lines(vector2,y2,xlim=c(0,2.5),ylim=c(0,2.5),col=3,lwd=6,lty=5)
lines(vector3,y3,xlim=c(0,2.5),ylim=c(0,2.5),col=4,lwd=6,lty=6)
grid()


#distribuciones acumuladas
plot(vector,d,type="l",xlim=c(0,2.5),ylim=c(-0.05,1.05),col=1,xlab="Tiempo",ylab="",cex.lab=2, cex.main=2,lwd=6,lty=3)
legend(0,1,bg="white",c(expression(paste(eta,"=",1,"",",","",beta,"=",0.5)),
                            expression(paste(eta,"=",1,"",",","",beta,"=",1)),
                            expression(paste(eta,"=",1,"",",","",beta,"=",1.5)),
                            expression(paste(eta,"=",2,"",",","",beta,"=",5))),
       col=1:4,lwd=6,lty=3:6)
lines(vector1,d1,type="l",xlim=c(0,2.5),ylim=c(0,1),col=2,lwd=6,lty=4)
lines(vector2,d2,type="l",xlim=c(0,2.5),ylim=c(0,1),col=3,lwd=6,lty=5)
lines(vector3,d3,type="l",xlim=c(0,2.5),ylim=c(0,1),col=4,lwd=6,lty=6)
grid()

#funcion de confiabilidad

plot(vector,1-d,type="l",xlim=c(0,2.5),ylim=c(0,1),col=1,xlab="Tiempo",ylab="",cex.lab=2, cex.main=2,lwd=6,lty=3)
legend(1.7,1,bg="white",c(expression(paste(eta,"=",1,"",",","",beta,"=",0.5)),
                        expression(paste(eta,"=",1,"",",","",beta,"=",1)),
                        expression(paste(eta,"=",1,"",",","",beta,"=",1.5)),
                        expression(paste(eta,"=",2,"",",","",beta,"=",5))),
       col=1:4,lwd=6,lty=3:6)
lines(vector1,1-d1,type="l",xlim=c(0,2.5),ylim=c(0,1),col=2,lwd=6,lty=4)
lines(vector2,1-d2,type="l",xlim=c(0,2.5),ylim=c(0,1),col=3,lwd=6,lty=5)
lines(vector3,1-d3,type="l",xlim=c(0,2.5),ylim=c(0,1),col=4,lwd=6,lty=6)
grid()


#funcion de riesgo
plot(vector,r,type="l",col=1, ylim=c(0,5), xlim=c(0,4),xlab="Tiempo",ylab="",cex.lab=2, cex.main=2,lwd=6,lty=3)
legend(2.5,5,bg="white",c(expression(paste(eta,"=",1,"",",","",beta,"=",0.5)),
                        expression(paste(eta,"=",1,"",",","",beta,"=",1)),
                        expression(paste(eta,"=",1,"",",","",beta,"=",1.5)),
                        expression(paste(eta,"=",2,"",",","",beta,"=",5))),
       col=1:4,lwd=6,lty=3:6)
lines(vector1,r1,type="l",col=2,lwd=6,lty=4)
lines(vector2,r2,type="l",col=3,lwd=6,lty=5)
lines(vector3,r3,type="l",col=4,lwd=6,lty=6)
grid()



