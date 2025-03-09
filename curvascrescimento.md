# Curvas de crescimento

<img src="https://cdn.jsdelivr.net/gh/devicons/devicon/icons/r/r-original.svg" width="40" height="40"/>

Curvas de crescimento - comparando os modelos logístico, Gompertz e Richards

Na definição dos objetos modelL, modelG e modelR, substituir xxxxx pelos valores iniciais.

```
# Curvas de crescimento
# Modelos logístico, Gompertz e Richards

x <- c(0, 5, 8, 13, 24, 26, 29, 32, 37, 48, 53, 56, 72, 157, 176)
y <- c(0, 0, 0.32, 1.69, 5.34, 5.73, 6.43, 7.16, 8.02, 8.81, 9.29, 9.35, 9.33, 9.35, 9.31)

modelL <- nls(y~K/(1+exp(a-r*x)),start=list(K=xxxxx,a=xxxxx,r=xxxxx))
modelG <- nls(y~K*exp(-a*exp(-r*x)),start=list(K=xxxx,a=xxxx,r=xxxx))
modelR <- nls(y~K*(1+exp(-r*a*(x-ti-log(a)/(r*a))))^(-1/a), start=list(K=xxx,r=xxx,a=xxx,ti=xx),algorithm = "port")

curveL <- function(t,K,a,r) curveL <- K/(1+exp(a-r*t))
curveG <- function(t,K,a,r) curveG <- K*exp(-a*exp(-r*t))
curveR <- function(t,K,r,a,ti) curveR <- K*(1+exp(-r*a*(t-ti-log(a)/(r*a))))^(-1/a)
plot(x,y,pch=19,ylim=c(0,10),xlab="xxxxxx",ylab="xxxxxxxxxxx",bty="l",las=1)
t      <- seq(0,200,1)
points(t,curveL(t,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3]),col="red",type="l",lwd=2)
points(t,curveG(t,K=coef(modelG)[1],a=coef(modelG)[2],r=coef(modelG)[3]),col="blue",type="l",lwd=2)
points(t,curveR(t,K=coef(modelR)[1],r=coef(modelR)[2],a=coef(modelR)[3],ti=coef(modelR)[4]),col="orange",type="l",lwd=2)
legend(5,1,lwd=3,col=c("red","blue","orange"), legend=c("Logistic","Gompertz","Richards"))
AIC(modelL,modelG,modelR)
```
