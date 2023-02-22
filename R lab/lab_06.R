## Esercizio ex Tab. 4.1
benz <- read.table(file="data/fig.4_1.dat")[,1]

plot(benz, type="l")
#Si può vedere graficamente che è stazionaria: allora posso usare lisciamento esponenziale semplice
## per equazione di y con lisciamento esponenziale
delta <- 0.1

y <- benz

## usando (4.1):
yhat <- rep(NA, length(y))
n <- length(y)
## valore iniziale
yhat[1] <- mean(y)
for(i in 1:(n-1)) {
    yhat[i+1] <- (1-delta) * sum(delta^(0:(i-1))*y[i:1])
}

## usando (4.2):
yhat <- rep(NA, length(y))
n <- length(y)
## valore iniziale
yhat[1] <- mean(y)
for(i in 1:(n-1)) {
    yhat[i+1] <- delta*yhat[i] + (1-delta)*y[i]
}

## una funzione le()
le <- function(y, delta) {
    yhat <- rep(NA, length(y))
    n <- length(y)
    ## valore iniziale
    yhat[1] <- mean(y)
    for(i in 1:(n-1)) {
        yhat[i+1] <- delta*yhat[i] + (1-delta)*y[i]
    }
    return(yhat)
}

le(benz, 0.1) # produce i risultati di p. 143

## Figura 4.2:
plot(benz, type="l")
lines(le(benz, 0.1), lty=2)
lines(le(benz, 0.8), lty=3)

## esempio: stima
## "grid search"
m <- 5
for(i in 1:9/10) {
    res2 <- (benz - le(benz, delta=i))^2
    ssres <- sum(res2[(m+1):length(res2)])
    print(ssres)
}

## stima con ottimizzazione numerica,
## criterio dei MQ
crit.fun <- function(delta, y, m) {
    res2 <- (y - le(y, delta=delta))^2
    ssres <- sum(res2[(m+1):length(res2)])
    return(ssres)
}
## es.
crit.fun(0.2, y, 5)

myopt <- optim(par=0.1, fn=crit.fun, y=benz, m=5, 
               method="Brent", lower=0, upper=1)
myopt$par
myopt$value

######################################

## es. Fig. 4.3 tasso di disocc in Veneto
u <- read.table(file="data/fig.4_3.dat")[,1]
plot(u, type="l")

## vedi cosa succederebbe con ES:
lines(le(u, 0.3), col="red")
lines(le(u, 0.8), col="blue")

## Metodo di Holt e Winters:
n <- length(u)
Lhat <- rep(NA, n)
That <- rep(NA, n)
yhat <- rep(NA, n)
alfa <- 0.1
beta <- 0.7

## valore iniziale
Lhat[1] <- yhat[1] <- u[1]
That[1] <- 0
yhat[2] <- Lhat[1]
Lhat[2] <- u[2]
That[2] <- u[2] - u[1]
yhat[3] <- Lhat[2] + That[2]*1
## da 3 in poi
for(i in 3:(n-1)) {
    Lhat[i] <- alfa * (Lhat[i-1]+That[i-1]) + (1-alfa)*u[i]
    That[i] <- beta*That[i-1] + (1-beta)*(Lhat[i]-Lhat[i-1])
    yhat[i+1] <- Lhat[i] + That[i]*1
}

lines(yhat, col="red")

## in una funzione:
hw <- function(y, alfa, beta) {
    n <- length(y)
    Lhat <- rep(NA, n)
    That <- rep(NA, n)
    yhat <- rep(NA, n)
    
    ## valore iniziale
    Lhat[1] <- yhat[1] <- y[1]
    That[1] <- 0
    yhat[2] <- Lhat[1]
    Lhat[2] <- y[2]
    That[2] <- y[2] - y[1]
    yhat[3] <- Lhat[2] + That[2]*1
    
    ## da 3 in poi
    for(i in 3:(n-1)) {
        Lhat[i] <- alfa * (Lhat[i-1]+That[i-1]) + (1-alfa)*y[i]
        That[i] <- beta*That[i-1] + (1-beta)*(Lhat[i]-Lhat[i-1])
        yhat[i+1] <- Lhat[i] + That[i]*1
    }
    return(yhat)
}

lines(hw(u, 0.1, 0.7), lty=2)
lines(hw(u, 0.3, 0.7), lty=4)
lines(hw(u, 0.4, 0.9), lty=3)

## chi sono i parametri ottimali?
## criterio dei MQ
crit.fun.hw <- function(betas, y, m) {
    alfa <- betas[1]
    beta <- betas[2]
    res2 <- (y - hw(y, alfa=alfa, beta=beta))^2
    ssres <- sum(res2[(m+1):length(res2)])
    return(ssres)
}
## es.
crit.fun.hw(c(0.1, 0.7), u, 5)

myopt <- optim(par=c(0,0), fn=crit.fun.hw, y=u, m=5, 
               method="L-BFGS-B",
               lower=c(0,0), upper=c(1,1))
myopt$par
myopt$value
lines(hw(u, alfa=myopt$par[1],
         beta=myopt$par[2]), col="red")

###### HW con stagionalità:

## direttamente in una funzione
hws <- function(y, alfa, beta, gamma) {
    ## fisso per semplicità la stagionalità a 4;
    ## sempre per semplicità partiamo sempre dal
    ## primo periodo
    n <- length(y)
    Lhat <- rep(NA, n)
    That <- rep(NA, n)
    Shat <- rep(NA, n)
    yhat <- rep(NA, n)
    
    ## valore iniziale: come pag. 147
    Lhat[3] <- 1/8*y[1] + 1/4*sum(y[2:4]) + 1/8*y[5]
    Lhat[4] <- 1/8*y[2] + 1/4*sum(y[3:5]) + 1/8*y[6]
    That[4] <- Lhat[4] - Lhat[3]
    Shat[4] <- y[4] - Lhat[4]
    Shat[3] <- y[3] - Lhat[3]
    Shat[2] <- y[2] - Lhat[3] -(-1)*That[4] # +That[4]
    Shat[1] <- y[1] - Lhat[3] -(-2)*That[4] # +2*That[4]  
    
    ## legge di aggiornamento:
    ## da 5 in poi
    for(i in 5:(n-1)) {
        Lhat[i] <- alfa * (Lhat[i-1]+That[i-1]) +
            (1-alfa)*(y[i]-Shat[i-4])    # aggiunta stagionalità
        That[i] <- beta*That[i-1] +
            (1-beta)*(Lhat[i]-Lhat[i-1]) # trend non cambia
        Shat[i] <- gamma*Shat[i-4] + 
            (1-gamma)*(y[i]-Lhat[i])
        ## qui è k=1 allora k<S
        yhat[i+1] <- Lhat[i] + That[i]*1 + Shat[i+1-4]
    }
    return(yhat)
}

## esempio del libro (vedi piccole differenze nel commento
## alla funzione)
plot(u, type="l")
lines(hws(u, 0.3, 0.7, 0.1), lty=2, col="blue")

## cerca parametri ottimali:
crit.fun.hws <- function(betas, y, m) {
    res2 <- (y - hws(y, alfa=betas[1],
                     beta=betas[2], gamma=betas[3]))^2
    ssres <- sum(res2[(m+1):length(res2)])
    return(ssres)
}

myopt <- optim(par=c(0,0,0), fn=crit.fun.hws, y=u, m=5, 
               method="L-BFGS-B",
               lower=c(0,0,0), upper=c(1,1,1))
myopt$par
myopt$value
lines(hws(u, alfa=myopt$par[1],
          beta=myopt$par[2],
          gamma=myopt$par[3]),
      col="red")

#### stesso esempio, funzioni "pronte" ####

## funzione "pronta" di R base, stesso metodo
tu <- ts(u, start=c(1992,4), freq=4)
hwmod <- HoltWinters(tu, seasonal="additive")

plot(tu)
fit.hw <- ts(c(rep(NA, 4), fitted(hwmod)[,"xhat"]),
             start=c(1992,4), freq=4)
lines(fit.hw, col="green3")

## idem, package "forecast"
library(forecast)
etsmod <- ets(tu, model="AAA")
lines(fitted(etsmod), col="red")

plot(forecast(etsmod, 8))


##### Analisi dei punti di svolta #####

## esempi:
## serie "del tutto casuale" (=generata plausibilmente
## da un processo white noise)
wn <- rnorm(100)
plotl <- function(x, ...) {
    plot(x, type="l", ...)
    abline(h=0, lty=2)
}
plotl(wn, col="red", lty=2)

## serie positivamente autocorrelata
ar1 <- function(n=100, rho=0) {
    y <- rep(NA, n)
    y[1] <- rnorm(1)
    for(i in 2:n) y[i] <- rho*y[i-1] + rnorm(1)
    return(y)
}
arp <- ar1(100, 0.7)
plotl(arp, col="blue")

## serie correlata negativamente: "attraversa" pi�
## spesso l'asse x
arn <- ar1(100, -0.7)
plotl(arn, col="blue")


## calcola punti di svolta della serie originale:
chpu <- c(NA, rep(0, length(u)-2), NA)

for(i in 2:(length(u)-1)) {
    if((u[i]>u[i-1] & u[i]>u[i+1]) | 
       (u[i]<u[i-1] & u[i]<u[i+1])) {
        chpu[i] <- 1
    }
}


u.hat <- hws(u, alfa=myopt$par[1],
             beta=myopt$par[2],
             gamma=myopt$par[3])
## fill first 5 vals.
u.hat[1:5] <- u[1:5]

chp.hat <- c(NA, rep(0, length(u)-2), NA)

for(i in 2:(length(u.hat)-1)) {
    if((u.hat[i]>u.hat[i-1] & u.hat[i]>u.hat[i+1]) | 
       (u.hat[i]<u.hat[i-1] & u.hat[i]<u.hat[i+1])) {
        chp.hat[i] <- 1
    }
}

## tabella di contingenza: quanti previsti correttamente?
table(chpu, chp.hat)



## Test dei punti di svolta (p.127):

## funzione che calcola i punti di svolta per una
## serie qualunque:
chp <- function(x) {
    ## init a zero
    mychp <- c(NA, rep(0, length(x)-2), NA)
    for(i in 2:(length(x)-1)) {
        ## se svolta poni a 1
        if((x[i]>x[i-1] & x[i]>x[i+1]) | 
           (x[i]<x[i-1] & x[i]<x[i+1])) {
            mychp[i] <- 1
        }
    }
    return(mychp)
}

chp.test <- function(x) {
    p.hat <- sum(chp(x), na.rm=TRUE)
    n <- length(x)
    tp.n <- (p.hat - 2*(n-2)/3)/sqrt((16*n-29)/90)
    p.val <- 2*(1 - pnorm(abs(tp.n)))
    
    names(tp.n) <- "Z test"
    dname <- paste(deparse(substitute(x)))
    
    mytest <- list(statistic = tp.n, 
                   method = "Test dei punti di svolta",
                   p.value = p.val, data.name = dname,
                   alternative = "Series 'not random'")
    class(mytest) <- "htest"
    return(mytest)
}
chp.test(u)

## analogamente, valutiamo la "casualità" dei residui
## (errori di previsione) u - u.hat

## prima li disegniamo
par(mfrow=c(2,1))
res <- u-u.hat
plot(u, type="l")
lines(u.hat, col="red")
plot(res, type="l", col="blue")
## ripristino
par(mfrow=c(1,1))

## test formale:
chp.test(res)

## proviamo sulle serie simulate
chp.test(arn)


## simulazione per determinare le proprietà empiriche
## del nostro test (in particolare il livello di sign.)

## simuliamo 100 serie perfettamente casuali di 50 oss.
simdatamat <- matrix(nrow=50, ncol=100)
for(i in 1:100) simdatamat[, i] <- ar1(50, -0.7) #rnorm(50)

myres <- apply(simdatamat, 2,
               FUN=function(x) chp.test(x)$p.value)
## empirical size:
sum(myres < 0.05)/length(myres)


## Ancora sulle medie mobili: decomposizione di una serie storica
decompose(tu)
plot(decompose(tu))