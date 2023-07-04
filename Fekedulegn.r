##################################################################################
#
# Edson Zangiacomi Martinez
# 03/07/2023
# Curvas de crescimento - dados do artigo de Fekedulegn et al. (1999)
#
##################################################################################
#
#  Reference:
#
#  Fekedulegn, D., Mac Siurtain, M. P., & Colbert, J. J. (1999). Parameter 
#  estimation of nonlinear growth models in forestry. Silva Fennica, 33(4), 
#  327-336.
#
##################################################################################
#
#  O experimento de desbaste de abetos Bowmont Norway (1930-1974) foi estabelecido 
#  no distrito de Roxburghe, na região da fronteira da Escócia, para investigar os 
#  efeitos de quatro tratamentos de desbaste no crescimento e rendimento do abeto 
#  da Noruega
#
##################################################################################


# Dados

y <- c(7.3,9.0,10.9,12.6,13.9,15.4,16.9,18.2,19.0,20.0)
t <- c(20,25,30,35,40,45,50,55,60,64)

plot(t,y,ylim=c(0,20),pch=19,bty="l",las=1,ylab="Top height (m)",xlab="Age (years)")

# Curvas

curve.Blumberg <- function(t,K,t0,w0,m) curve.Blumberg  <- (K * (t - t0)^m)/(w0 + (t - t0)^m)
curve.Brody    <- function(t,K,w0,r)    curve.Brody     <-  K - (K - w0)*exp(-r*t) 
curve.monomolc <- function(t,K,b,r)     curve.monomolc  <-  K* (1 - b*exp(-r*t))
curve.mitcherl <- function(t,K,b,r)     curve.mitcherl  <-  K - b * r^t
curve.Stannard <- function(t,K,b,r,m)   curve.Stannard  <-  K*(1+exp(-(b+r*t)/m))^(-m) 
curveG         <- function(t,K,b,r)     curveG          <-  K*exp(-b*exp(-r*t)) 
curve.vonBerta <- function(t,K,b,r,m)   curve.vonBerta  <-  (K^(1-m) - b * exp(-r * t))^(1/(1-m)) 

# Ajuste dos modelos

model.Brody    <- nls(y~K-(K-w)*exp(-r*t),start=list(K=34,w=-1.8,r=0.01))
model.monomolc <- nls(y~K*(1-b*exp(-r*t)),start=list(K=34,b=1.8,r=0.01))
model.mitcherl <- nls(y~K-b*r^t,start=list(K=34,b=36,r=0.9))
model.negexpnc <- nls(y~K*(1-exp(-r*t)),start=list(K=34,r=0.01))
model.Gompertz <- nls(y~K*exp(-b*exp(-r*t)),start=list(K=34,b=1,r=0.01))
model.vonBerta <- nls(y~(K^(1-m)-b*exp(-r*t))^(1/(1-m)),start=list(K=34,b=4,r=0.02,m=0.5))

# Gráfico comparando os ajustes

plot(t,y,ylim=c(5,20),pch=19,bty="l",las=1,ylab="Top height (m)",xlab="Age (years)")
points(t,curve.Brody(t,K=coef(model.Brody)[1],w0=coef(model.Brody)[2],r=coef(model.Brody)[3]),col="magenta",type="l",lwd=2)
points(t,curve.monomolc(t,K=coef(model.monomolc)[1],b=coef(model.monomolc)[2],r=coef(model.monomolc)[3]),col="red",
 type="l",lwd=2)
points(t,curve.mitcherl(t,K=coef(model.mitcherl)[1],b=coef(model.mitcherl)[2],r=coef(model.mitcherl)[3]),col="blue",
 type="l",lwd=2)
points(t,curve.monomolc(t,K=coef(model.negexpnc)[1],b=1,r=coef(model.negexpnc)[2]),col="orange",type="l",lwd=2)
points(t,curveG(t,K=coef(model.Gompertz)[1],b=coef(model.Gompertz)[2],r=coef(model.Gompertz)[3]),col="green",type="l",lwd=2)
points(t,curve.vonBerta(t,K=coef(model.vonBerta)[1],b=coef(model.vonBerta)[2],r=coef(model.vonBerta)[3],
 m=coef(model.vonBerta)[4]),col="cyan",type="l",lwd=2)
legend(40,11,lwd=2,col=c("magenta","red","blue","orange","green","cyan"),
                legend=c("Brody","Monomolecular","Mitcherlich","Negative exponential","Gompertz","von Bertalanffy"))

# Resumos dos modelos

summary(model.Brody)
summary(model.monomolc)
summary(model.mitcherl)
summary(model.negexpnc)
summary(model.Gompertz)
summary(model.vonBerta)

# Akaike information criterion (AIC) 

AIC(model.Brody,model.monomolc,model.mitcherl,model.negexpnc, model.Gompertz,model.vonBerta)

