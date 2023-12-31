
# População do Brasil

urlfile="https://raw.githubusercontent.com/edsonzmartinez/basesdedados/main/PopCensosBrasil.csv"

w <- read.csv(urlfile,head=TRUE,sep=";",dec=",")

area <- "BR"

pop  <- rep(NA,12)

ano <- c(1872,1890,1900,1920,1940,1950,1960,1970,1980,1991,2000,2010)

t   <- ano-1872

pop[1]  <- w$POP1872[w$sigla==area]  
pop[2]  <- w$POP1890[w$sigla==area]  
pop[3]  <- w$POP1900[w$sigla==area]  
pop[4]  <- w$POP1920[w$sigla==area]  
pop[5]  <- w$POP1940[w$sigla==area]  
pop[6]  <- w$POP1950[w$sigla==area]  
pop[7]  <- w$POP1960[w$sigla==area]  
pop[8]  <- w$POP1970[w$sigla==area]   
pop[9]  <- w$POP1980[w$sigla==area]   
pop[10] <- w$POP1991[w$sigla==area]   
pop[11] <- w$POP2000[w$sigla==area]   
pop[12] <- w$POP2010[w$sigla==area]

plot(ano,pop,type="l",col="blue",bty="l")
points(ano,pop,pch=19,cex=1.2,col="blue")

plot(t,pop,type="l",col="blue",bty="l")
points(t,pop,pch=19,cex=1.2,col="blue")


# Definindo as curvas
curveL  <- function(t,K,a,r)  curveL <- K/(1+exp(a-r*t)) 
curveG  <- function(t,K,a,r)  curveG <- K*exp(-a*exp(-r*t)) 
curveR  <- function(t,K,r,a,ti)  curveR <- K*(1+exp(-r*a*(t-ti-log(a)/(r*a))))^(-1/a) 


g <- seq(0,140,1)

# Modelo logístico
modelL <- nls(pop ~ K/(1+exp(a-r*t)),start=list(K=240000000,a=6,r=0.06))
modelL

plot(t,pop,type="l",col="blue",bty="l")
points(t,pop,pch=19,cex=1.2,col="blue")
points(t,curveL(t,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3]),col="red",type="l",lwd=2)

# Modelo de Gompertz
modelG <- nls(pop ~ K*exp(-a*exp(-r*t)),start=list(K=240000000,a=6,r=0.03))
modelG

plot(t,pop,type="l",col="blue",bty="l")
points(t,pop,pch=19,cex=1.2,col="blue")
points(t,curveG(t,K=coef(modelG)[1],a=coef(modelG)[2],r=coef(modelG)[3]),col="green",type="l",lwd=2)

# Comparando os modelos logísticos e de Gompertz

plot(t,pop,type="l",col="black",bty="l")
points(t,pop,pch=19,cex=1.2,col="black")
points(t,curveL(t,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3]),col="red",type="l",lwd=2)
points(t,curveG(t,K=coef(modelG)[1],a=coef(modelG)[2],r=coef(modelG)[3]),col="green",type="l",lwd=2)
legend(1,1.8e8,lwd=3,col=c("red","green"), legend=c("Logistic","Gompertz"))

AIC(modelL,modelG)

# Modelo de Richards

modelR <- nls(pop ~ K*(1+exp(-r*a*(t-ti-log(a)/(r*a))))^(-1/a), start=list(K=240000000,r=0.1,a=0.2,ti=90),algorithm = "port")
modelR

plot(t,pop,type="l",col="blue",bty="l")
points(t,pop,pch=19,cex=1.2,col="blue")
points(t,curveR(t,K=coef(modelR)[1],r=coef(modelR)[2],a=coef(modelR)[3],ti=coef(modelR)[4]),col="orange",type="l",lwd=2)

# Comparando os três modelos

plot(t,pop,type="l",col="black",bty="l")
points(t,pop,pch=19,cex=1.2,col="black")
points(t,curveL(t,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3]),col="red",type="l",lwd=2)
points(t,curveG(t,K=coef(modelG)[1],a=coef(modelG)[2],r=coef(modelG)[3]),col="green",type="l",lwd=2)
points(t,curveR(t,K=coef(modelR)[1],r=coef(modelR)[2],a=coef(modelR)[3],ti=coef(modelR)[4]),col="orange",type="l",lwd=2)
legend(1,1.8e8,lwd=3,col=c("red","green","orange"), legend=c("Logistic","Gompertz","Richards"))


AIC(modelL,modelG,modelR)

# Predições para 2022

pop2022 <- w$POP2022[w$sigla==area]

t0   <- 2022-1872

predL <- curveL(t0,K=coef(modelL)[1],a=coef(modelL)[2],r=coef(modelL)[3])
predG <- curveG(t0,K=coef(modelG)[1],a=coef(modelG)[2],r=coef(modelG)[3])
predR <- curveR(t0,K=coef(modelR)[1],r=coef(modelR)[2],a=coef(modelR)[3],ti=coef(modelR)[4])

c1  <- c("Logístico","Gompertz","Richards","Real")
c2  <- c(predL,predG,predR,pop2022)
res <- data.frame(c1,c2)

cat("População predita para o ano de 2022:\n")
res

# Taxa média geométrica de crescimento anual (%)

# 2010/2000
r <- ((pop[12]/pop[11])^(1/10)-1)*100

# Projeção da população para 2022:
pop[12]*(1+r/100)^12







