

DecToBin <- function(x) {
x0 <- x
s0 <- ""
while (x0>0) {
s0 <- paste(x0 %% 2,s0,sep="")
x0 <- x0 %/% 2 }
return(s0) }

BinToDec <- function(x) {
k <- as.numeric(strsplit(as.character(x),"")[[1]])
s0 <- 0
for (g in (length(k)-1):0) s0 <- s0 + rev(k)[g+1]*2^g
return(s0) }


fx <- \(x) x^2 - 2*x - 5
cbind(-5:5,fx(-5:5))


################################################
################################################
# Método da bissecção

# A função que se deseja encontrar a raiz
f <- \(x) x^3 - 2*x - 5
# Valores iniciais do intervalo [a, b]
a <- 2
b <- 3
# Precisão
epsilon <- 1e-6
# Número máximo de iterações
max_iterations <- 100
for (k in 1:max_iterations) {
c <- (a + b) / 2
if ((b-a) < epsilon) break
if (f(c) * f(a) < 0) { b <- c } else { a <- c }
txt <- paste("iteração ",k,", x = ",c,sep="")
print(txt)
}
# Mostrar a raiz e o número de iterações
cat("A raiz é", c, "encontrada em", k-1, "iterações.\n")

################################################
################################################
# Método de Newton-Raphson

# A função que se deseja encontrar a raiz
f <- \(x) x^3 - 2*x - 5

# A derivada de f(x)
df <- \(x) 3*x^2 - 2

# Valor inicial
x0 <- 2.5

# Precisão
epsilon <- 1e-6

# Número máximo de iterações
max_iterations <- 100

for (k in 1:max_iterations) {
  x1  <- x0 - f(x0)/df(x0)
  if (abs(x1-x0) < epsilon) break
  x0  <- x1
  txt <- paste("iteração ",k,", x = ",x0,sep="")
  print(txt)
}

# Mostrar a raiz e o número de iterações
cat("A raiz é", x1, "encontrada em", k-1, "iterações.\n")



################################################
################################################
# Método de Newton-Raphson

# A função que se deseja encontrar a raiz
f <- \(x) x^3 - 9*x + 3

# A derivada de f(x)
df <- \(x) 3*x^2 - 9

# Valor inicial
x0 <- 1.5

# Precisão
epsilon <- 1e-6

# Número máximo de iterações
max_iterations <- 100

for (k in 1:max_iterations) {
  x1  <- x0 - f(x0)/df(x0)
  if (abs(x1-x0) < epsilon) break
  x0  <- x1
  txt <- paste("iteração ",k,", x = ",x0,sep="")
  print(txt)
}

# Mostrar a raiz e o número de iterações
cat("A raiz é", x1, "encontrada em", k-1, "iterações.\n")



