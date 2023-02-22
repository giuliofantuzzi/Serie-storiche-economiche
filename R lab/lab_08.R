data("AirPassengers")
class(AirPassengers)#è già in formato serie storiche
plot(AirPassengers)
#Cosa vediamo: trend crescente, non stazionarietà, stagionalità evidente (e pare moltiplicativa)

#NB: sappiamo che acf è insensata per dati non stazionari, ma se facessimo:
acf(AirPassengers)#e guardando questo vedo che non ha senso

#Ha più senso fare acf della differenza prima
acf(diff(AirPassengers)) 
#non sembra violare hp di stazionarietà...sembra omogeneo di grado 1
#NOTA: vedo che la scala del grafico acf è quella MENSILE!
#--->in generale la struttura che rivela questo acf è complessa
pacf(diff(AirPassengers))
#Che modello stimare? un AR, un MA, un ARMA, ARIMA?
#--->sia acf che pacf decrescono entrambe piano...vedo che c'è stagionalità...allora tento con un arima

#NB: qui faccio il modello sui logaritmi...xke? i logaritmi stabilizzano la alta varianza dei dati
airmod <- arima(log(AirPassengers), order=c(1,1,1), seasonal=c(1,1,1))
#NB....passo c(p,d,q) e c(P,D,Q)...d e D li metto io...R mi stima p,P,q,Q, ossia 4 coefficienti
#NB: anche se la funzione si chiama arima, col parametro seasonal è come se definissi un SARIMA
#I coefficienti saranno:
airmod$coef
#Sono significativamente diversi da 0?
#Vediamo quanto valgono i coefficienti e quanto gli standard error
#la qta coeff_stimato/S.E ~ N(0,1)
airmod #nell'output considero la tabella coefficients (in cui ho i coff e gli SE)
#------------------------------------------------------------
#Per confrontare modelli è utile
#il valore della logverosimiglianza (serve x criterio akaike)
airmod$loglik
#Calcolo la quantità del criterio di akaike(vogliamo sia bassa) 
#1)AIC secondo formula di fonzio lis
AIC<- -2/length(AirPassengers) *(airmod$loglik - length(coef(airmod)))
#2)tuttavia usando AIC del software il risultato è molto diverso
airmod$aic
#NB: questo perchè come qtà aic R usa quella di wikipedia! 2*length(coefficients(airmod)) -2* airmod$loglik
#------------------------------------------------------------


#-----------------------------------------------------------------------
#confronto tra vari modelli (in termini di tempo e seconda del metodo)
system.time(airmod1 <- arima(log(AirPassengers), order=c(2,1,1), seasonal=c(1,1,1)))
system.time(airmod2 <- arima(log(AirPassengers), order=c(2,1,1), seasonal=c(1,1,1), method="CSS"))
system.time(airmod3 <- arima(log(AirPassengers), order=c(2,1,1), seasonal=c(1,1,1), method="ML"))
#-----------------------------------------------------------------------

#VALIDITÀ DEL MODELLO airmod1 (casualità residui)
#1) metodo grafico
plot(residuals(airmod1))#già qui sembra molto bene
#2)test di jung e Box (applicato sui residui)
Box.test(residuals(airmod), lag=12, type="L",
         fitdf=length(coef(airmod1))) 
#il test non rifiuta l'hp di assenza di autocorrelazione!
#--->(i residui non risultano autocorrelati...BENE, xke vogliamo siano casuali)

#3)test di jarque bera (per normalità residui)
library(tseries)
jarque.bera.test(residuals(airmod1))
#--->p value non tremendo, ma è un campanello d'allarme


