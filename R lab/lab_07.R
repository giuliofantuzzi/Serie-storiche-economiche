#NOTA: in r la funzione PACF NON disegna la linea al lag 0 

#-------------------------------------------------------
# Processo autocorrelato di ordine 2-->AR(2)
#-------------------------------------------------------
phi0 <- 0
phi1 <- 0.7
phi2 <- -0.3
y <- rep(NA, 100)
y[1] <- rnorm(1)
y[2] <- rnorm(1)

#genero i valori del processo
for(t in 3:100) {
    y[t] <- phi0 + phi1*y[t-1] + phi2*y[t-2] + rnorm(1)
}
plot(y, type="l")
#Qui posso dubitare della stazionarietà!

#Mi aspetto che le lineette degradino verso 0 esponenzialmente dal lag >=3 in poi stanno sotto le barre di confidenza
acf(y)
#Nella teoria dovrebbe avere i primi 2 lag sopra le barre e poi andare LENTAMENTE a 0, ma non sempre troviamo una corrispondenza nel caso empirico
pacf(y)

y.1 <- c(NA, y[1:99])
y.2 <- c(NA, NA, y[1:98])
dati <- as.data.frame(cbind(y, y.1, y.2))

ar2mod <- lm(y~y.1+y.2, dati)
summary(ar2mod)
#-------------------------------------------------------
# Processo a media mobile di ordine 2-->MA(2)
#-------------------------------------------------------
n <- 100
theta1 <- 0.7
theta2 <- -0.3
y <- rep(NA, n)
e <- rep(NA, n)
y[1] <- e[1] <- rnorm(1)
y[2] <- e[2] <- rnorm(1)
for(i in 3:n) {
    e[i] <- rnorm(1)
    y[i] <- e[i] - theta1*e[i-1] - theta2*e[i-2]
}

plot(y, type="l")
#a vedere il grafico c'è evidente stazionarietà (che effettivamente c'è) e autocorrelazione
mean(y)  
acf(y) #essendo di ordine 2 dal lag >=3 dovrei avere linee entro le bande tratteggiate (questo nella teoria, ma nel caso empirico vediamo che non avviene)
pacf(y)
