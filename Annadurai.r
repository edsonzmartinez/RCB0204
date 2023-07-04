##################################################################################
#
# Edson Zangiacomi Martinez
# 03/07/2023
# Curvas de crescimento - dados do artigo de Annadurai et al. (2000)
#
##################################################################################
#
#  Reference:
#
#  Annadurai, G., Rajesh Babu, S., & Srinivasamoorthy, V. R. (2000). 
#  Development of mathematical models (Logistic, Gompertz and Richards models) 
#  describing the growth pattern of Pseudomonas putida (NICM 2174). 
#  Bioprocess Engineering, 23, 607-612.
#  https://doi.org/10.1007/s004490000209
#
##################################################################################

x <- 1:27
y <- c(0.216,0.220,0.240,0.250,0.260,
       0.270,0.280,0.290,0.330,0.360,
       0.380,0.400,0.460,0.482,0.492,
       0.522,0.542,0.584,0.594,0.620,
       0.644,0.644,0.648,0.648,0.648,
       0.648,0.648)
plot(x,y,pch=19,ylim=c(0,0.65),
 xlab="Time (h)",
 ylab="Optical density of cell mass",
 bty="l",las=1)

# curves
curveL  <- function(t,K,a,r)     curveL <- K/(1+exp(a-r*t)) 
curveG  <- function(t,K,a,r)     curveG <- K*exp(-a*exp(-r*t)) 
curveR  <- function(t,K,r,a,ti)  curveR <- K*(1+exp(-r*a*(t-ti-log(a)/(r*a))))^(-1/a) 

t <- seq(0,30,0.1)

# Logistic model
modelL <- nls(y~K/(1+exp(a-r*x)),start=list(K=0.65,a=0.81,r=0.2))
modelL

plot(x,y,pch=19,ylim=c(0,0.7),xlab="Time (h)",ylab="Optical density of cell mass",bty="l",las=1)
points(t,curveL(t,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3]),col="red",type="l",lwd=2)

# Gompertz model
modelG <- nls(y~K*exp(-a*exp(-r*x)),start=list(K=0.76,a=0.8,r=0.13))
modelG

plot(x,y,pch=19,ylim=c(0,0.7),xlab="Time (h)",ylab="Optical density of cell mass",bty="l",las=1)
points(t,curveL(t,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3]),col="red",type="l",lwd=2)
points(t,curveG(t,K=coef(modelG)[1],a=coef(modelG)[2],r=coef(modelG)[3]),col="blue",type="l",lwd=2)
legend(15,0.3,lwd=3,col=c("red","blue"), legend=c("Logistic","Gompertz"))

# Richards model
modelR <- nls(y~K*(1+exp(-r*a*(x-ti-log(a)/(r*a))))^(-1/a),start=list(K=0.65,r=0.1,a=0.2,ti=12),algorithm = "port")
modelR

plot(x,y,pch=19,ylim=c(0,0.7),xlab="Time (h)",ylab="Optical density of cell mass",bty="l",las=1)
points(t,curveL(t,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3]),col="red",type="l",lwd=2)
points(t,curveG(t,K=coef(modelG)[1],a=coef(modelG)[2],r=coef(modelG)[3]),col="blue",type="l",lwd=2)
points(t,curveR(t,K=coef(modelR)[1],r=coef(modelR)[2],a=coef(modelR)[3],ti=coef(modelR)[4]),col="orange",type="l",lwd=2)
legend(15,0.3,lwd=3,col=c("red","blue","orange"), legend=c("Logistic","Gompertz","Richards"))

# Akaike information criterion (AIC) 
AIC(modelL,modelG,modelR)







