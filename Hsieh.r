##################################################################################
#
# Edson Zangiacomi Martinez
# 03/07/2023
# Curvas de crescimento - dados do artigo de Hsieh et al. (2015)
#
##################################################################################
#
#  Reference:
#
#  Hsieh, Y. H. (2015). 2015 Middle East respiratory syndrome coronavirus (MERS-CoV) 
#  nosocomial outbreak in South Korea: insights from modeling. PeerJ, 3, e1505.
#  https://doi.org/10.7717/peerj.1505
#
##################################################################################

y <- c(1, 1, 1, 1, 1, 1, 1, 1, 3, 9, 13, 15, 19, 21, 25, 27, 30, 31, 
34, 42, 51, 71, 85, 96, 108, 119, 128, 134, 142, 145, 154, 159, 
162, 165, 166, 170, 176, 176, 177, 177, 177, 179, 181, 182, 182, 
182, 183, 183, 183, 183, 184, 184, 186, 186, 186, 186, 186)

n <- length(y)

x <- 1:n

plot(x,y,pch=19,ylim=c(0,190),
 xlab="Days",
 ylab="Cumulative reported MERS case data",
 bty="l",las=1)

# curves
curveL  <- function(t,K,a,r)     curveL <- K/(1+exp(a-r*t)) 
curveG  <- function(t,K,a,r)     curveG <- K*exp(-a*exp(-r*t)) 
curveR  <- function(t,K,r,a,ti)  curveR <- K*(1+exp(-r*a*(t-ti-log(a)/(r*a))))^(-1/a) 

# Logistic, Gompertz and Richards models
modelL <- nls(y~K/(1+exp(a-r*x)),start=list(K=200,a=0.81,r=0.2))
modelG <- nls(y~K*exp(-a*exp(-r*x)),start=list(K=200,a=0.8,r=0.13))
modelR <- nls(y~K*(1+exp(-r*a*(x-ti-log(a)/(r*a))))^(-1/a),start=list(K=200,r=0.1,a=0.2,ti=20),algorithm = "port")

t <- seq(0,n,0.1)

# Plot

plot(x,y,pch=19,ylim=c(0,190),xlab="Days",ylab="Cumulative reported MERS case data",bty="l",las=1)
points(t,curveL(t,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3]),col="red",type="l",lwd=2)
points(t,curveG(t,K=coef(modelG)[1],a=coef(modelG)[2],r=coef(modelG)[3]),col="blue",type="l",lwd=2)
points(t,curveR(t,K=coef(modelR)[1],r=coef(modelR)[2],a=coef(modelR)[3],ti=coef(modelR)[4]),col="orange",type="l",lwd=2)
legend(35,100,lwd=3,col=c("red","blue","orange"), legend=c("Logistic","Gompertz","Richards"))

# Akaike information criterion (AIC) 
AIC(modelL,modelG,modelR)







