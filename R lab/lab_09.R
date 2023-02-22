#--------------------------------------------------------------
vend<-read.table(file="data/Tab7.1.txt", h=TRUE) #h=T è per dire che c'è header
#Passiamo a formato ts [il valore è nella 3 colonna]
vend<- ts(vend[,3], start=c(2005,1),freq=12)
plot(vend)

#Proviamo ad approssimare con arima
library("forecast")
#trovo in automatico l'arima migliore
fvend<- auto.arima(vend)
fvend
#--->risulta un ARIMA(0,0,0)(1,1,0)
#NB: il termine "drift" è l'intercetta del modello delle differenze
#Proviamo a vedere come funziona il modello in previsione
#La funzione seguente fa le previsioni per i prossimi 2 anni e plotta con le bande di confidenza
plot(forecast(fvend))
#Volendo posso specificare quanto andare avanti con le previsioni
#Ad esempio, se volessi solo un anno
plot(forecast(fvend, 12)) #1 anno = 12 mesi (il PERIODO del nostro modello)

#Cosa sarebbe successo con il lisciamento esponenziale?
evend<- ets(vend)
plot(forecast(evend,12))
#vediamo che anche questo modello ci prende bene
#--------------------------------------------------------------

#--------------------------------------------------------------
#Qui ci è andata bene...prendiamo una serie più spinosa, ad es. una serie prima di struttura (i dati dell'es 4)
#Consideriamo dunque questa serie white noise:
e1 <- c(0.4,-0.7,0.8,0,-0.9,-0.5,-0.6,0.4,-0.9,0.1,-0.2,-0.9,0.8,-0.1,0.4,-0.7,0.9,-0.5,0.5,-0.9,0.1,-0.3,0.3,0.9,0.8,0.8,0,-0.1,0.2,0.5)

plot(e1, type="l")
abline(h=0, lty=2)

#1) modello a media mobile
lines(ma(e1,5), col="red")

#2) provo a vedere autoarima
fe1<- auto.arima(e1)
fe1 #--->da un arima(0,0,0), che è un modo equivalente di dire white noise
#cosa può prevedere un arima(0,0,0)? un valore fisso=0, con una certa banda di confidenza
plot(forecast(fe1))

#3) e lisciamento?
ee1<- ets(e1)
ee1
plot(forecast(e1)) #anche questo prevede 0 come l'arima
#--------------------------------------------------------------

#--------------------------------------------------------------
#Dati dei pantaloni
pant<- read.table("data/fig.2_18.dat", h=FALSE)
pant<-  ts(data=pant, start=1971)

#1)AUTOARIMA
fpant<- auto.arima(pant)
fpant
#--->viene un ARIMA(3,1,0) 
plot(forecast(fpant))
#NB: mi proietta un trend a bassa pendenza
#--------------------------------------------------------------


#--------------------------------------------------------------
#ora dati sulla disoccupazione in veneto
u<- read.table(file="data/fig.4_3.dat")[,1]
plot(u, type="l")

#se non ci risulta stagionalità ottengo il modello
fu<- auto.arima(u)
fu #-->ARIMA(2,1,2) 
plot(forecast(fu))
#In realtà sul libro diceva che sti dati erano trimestrali. Aggiungiamo sta info
tu<- ts(u, freq=4)
ftu<- auto.arima(tu)
ftu #-->ARIMA(0,1,0)(1,1,0)
plot(forecast(ftu))
#NB: vediamo che in base alle info che andiamo a specificare i modelli stimati sono diversi

#Proviamo il lisciamento esp
#1) senza specificare la frequenza
eu<- ets(u)
plot(forecast(eu))
#2) specificando la frequenza
etu<- ets(tu)
plot(forecast(etu))
#NB: anche qui differenza sostanziale
#--------------------------------------------------------------
