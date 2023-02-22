#Importo i dati e le librerie
source("import_data.R")
library("forecast")
#-------------------------------------------------------------------
#############################################################################
# A) analisi serie storiche pil
#############################################################################
#PIL NOMINALI ANNUI A CONFRONTO
plot(ts(gdp_nom_ita, start=c(2000,1), frequency = 1), 
     ylim=c(0,3000000), col=2, xlab="Time", ylab="NOMINAL GDP",
     main="Confronto GDP nominale")
lines(ts(gdp_nom_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(gdp_nom_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.7, c("ITA", "IRL", "DNK"), col=2:4, lty=1)
#-------------------------------------------------------------------
#PIL NOMINALI ANNUI PRO CAPITE  A CONFRONTO
plot(ts(gdp_nom_cap_ita, start=c(2000,1), frequency = 1),
     ylim=c(20000,120000),
      col=2, xlab="Time", ylab="NOMINAL GDP pro capita",
     main="Confronto GDP nominale pro capite")
lines(ts(gdp_nom_cap_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(gdp_nom_cap_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.7, c("ITA", "IRL", "DNK"), col=2:4, lty=1)
#-------------------------------------------------------------------
#PASSIAMO AL PIL REALE ANNUALE

#Plottiamo i pil reali
plot(ts(gdp_real_ita, start=c(2000,1), frequency = 1),
     col=2, xlab="Time", ylab="REAL GDP", ylim=c(0,20000),
     main="Confronto GDP reale")
lines(ts(gdp_real_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(gdp_real_dnk, start=c(2000,1), frequency = 1), type="l",col=4)

#anche qui i termini totali non sono significativi
#Passiamo ai pro capite
plot(ts(gdp_real_cap_ita, start=c(2000,1), frequency = 1),
     col=2, xlab="Time", ylab="REAL GDP pro capita", ylim=c(200,800),
     main="Confronto GDP reale pro capite")
lines(ts(gdp_real_cap_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(gdp_real_cap_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.7, c("ITA", "IRL", "DNK"), col=2:4, lty=1)

#STIMA DEL TREND SUL PIL REALE PRO CAPITE DEI VARI PAESI
#HP: sembra che ita e dnk abbiano trend lineare...irl polinomiale/exp
gdp_ttrend<- 1:length(gdp_real_cap_ita) #stesso n° anche per irl e dnk

#ITALIA
LM_gdp_real_cap_ita<-lm(gdp_real_cap_ita ~gdp_ttrend)
PM_gdp_real_cap_ita<-lm(gdp_real_cap_ita ~gdp_ttrend + I(gdp_ttrend^2)+ I(gdp_ttrend^3))
summary(LM_gdp_real_cap_ita)$adj.r.squared   
summary(PM_gdp_real_cap_ita)$adj.r.squared 
#---->per l'italia è opportuno un polinomio di 3 grado
plot(gdp_real_cap_ita,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND GDP REALE PRO CAPITE ITA")
lines(PM_gdp_real_cap_ita$fitted.values, lty=2)
#analisi dei residui
plot(2000:2021,residuals(PM_gdp_real_cap_ita), xlab="", ylab="", main="Residui ITA")
abline(0,0)
#sembra esserci un andamento sistematico...non va bene!
#forse c'è ciclicità nel pil, ma non sappiamo stimarla
#IRLANDA
LM_gdp_real_cap_irl<-lm(gdp_real_cap_irl ~gdp_ttrend)
PM_gdp_real_cap_irl<-lm(gdp_real_cap_irl ~gdp_ttrend + I(gdp_ttrend^2))
summary(LM_gdp_real_cap_ita)$adj.r.squared     
summary(PM_gdp_real_cap_ita)$adj.r.squared 
#NB: da 2 grado in poi viene un r adj sempre =
#Proviamo col modello esponenziale
EXP_gdp_real_cap_irl <- nls(gdp_real_cap_irl ~ I(a*exp(b*gdp_ttrend)),
              start=list(a=100, b=1), trace=TRUE) 
#--->dà errore di gradiente singolare, quindi il modello exp non è adatto

plot(gdp_real_cap_irl,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND GDP REALE PRO CAPITE IRL")
lines(PM_gdp_real_cap_irl$fitted.values, lty=2)
plot(2000:2021,residuals(PM_gdp_real_cap_irl), xlab="", ylab="", main="Residui IRL")
abline(0,0)
#anche qui non benissimo: direi che c'è ciclicità, ma non la sappiamo calcolare
#DANIMARCA
LM_gdp_real_cap_dnk<-lm(gdp_real_cap_dnk ~gdp_ttrend)
PM_gdp_real_cap_dnk<-lm(gdp_real_cap_dnk ~gdp_ttrend + I(gdp_ttrend^2)+I(gdp_ttrend^3))
summary(LM_gdp_real_cap_dnk)$adj.r.squared     
summary(PM_gdp_real_cap_dnk)$adj.r.squared 
#--->confrontando gli R^2 adj il grado del miglior reg è grado 3
plot(gdp_real_cap_dnk,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND GDP REALE PRO CAPITE DNK")
lines(PM_gdp_real_cap_dnk$fitted.values, lty=2)
#Residui
plot(2000:2021,residuals(PM_gdp_real_cap_dnk), xlab="", ylab="", main="Residui DNK")
abline(0,0)
#Qui sembra bene!



#############################################################################
#B) ANALISI SERIE STORICHE DEI PREZZI DELLE CASE
#############################################################################

#-------------------------------------------------------------------------------
#Grafico del prezzo delle case reale annuale
plot(ts(hp_base00_real_y_ita, start=c(2000,1), frequency = 1), ylim=c(80,180),
     col=2, xlab="Time", ylab="REAL HP",
     main="Confronto HP reale")
lines(ts(hp_base00_real_y_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(hp_base00_real_y_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.5, c("ITA", "IRL", "DNK"), col=2:4, lty=1)

#Affordability: divido il pil pro capite reale(ho solo i dati annui) per hp reale
plot(ts(affordability_ita, start=c(2000,1), frequency = 1), ylim=c(1.9,5.2),
     col=2, xlab="Time", ylab="REAL HP/REAL GDP CAPITA",
     main="Confronto AFFORDABILITY")
lines(ts(affordability_irl, start=c(2000,1), frequency = 1), type="l",col=3)
lines(ts(affordability_dnk, start=c(2000,1), frequency = 1), type="l",col=4)
legend("topleft",cex=0.6, c("ITA", "IRL", "DNK"), col=2:4, lty=1)
#Intanto facciamo il trend
hp_ttrend<- 1:length(hp_base00_real_q_ita)
#1) ITALIA
#trend dell'ita
LM_hp_q_ita<-lm(hp_base00_real_q_ita ~ hp_ttrend)
PM_hp_q_ita<-lm(hp_base00_real_q_ita ~ hp_ttrend + I(hp_ttrend^2)+ I(hp_ttrend^3)+ I(hp_ttrend^4)+ I(hp_ttrend^5))
summary(LM_hp_q_ita)$adj.r.squared 
summary(PM_hp_q_ita)$adj.r.squared #il migliore è di grado 5
plot(hp_base00_real_q_ita,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND Hp trimestrale")
lines(PM_hp_q_ita$fitted.values, lty=2)
plot(seq(from=2000.25,to=2022.5,by=0.25),residuals(PM_hp_q_ita), xlab="", ylab="", main="Residui ITA")
abline(0,0)
#residui non al top....magari c'è componente stagionale??
#NB: In realtà dal grafico non si vede nessuna apparente stagionalità
#facciamo un'analisi di  per scoprirlo
#creo il vettore dummies
trimestri<- c(rep(c("Q1","Q2","Q3","Q4"),22 ), "Q1","Q2")
stag_mod_ita<- lm(residuals(PM_hp_q_ita)~trimestri-1)
summary(stag_mod_ita)
plot(residuals(PM_hp_q_ita), main="residui ITA")
lines(fitted.values(stag_mod_ita), col="red")
#linea piatta...dunque verificato che non c'è stagionalità
#NB: se ora facessi i residui dello stag mod sarebbero ~= ai res del PM_mod

#Abbiamo dimostrato che non c'è stagionalità
#In realtà sul sito oecd "indices are seasonally adjusted", quindi tutto torna.
#Quindi come giustifico sti residui schifosi? o c'è ciclo, o servono metodi di ottimizzazione non lineari

#2) IRLANDA
#trend dell'irl
LM_hp_q_irl<-lm(hp_base00_real_q_irl ~ hp_ttrend)
PM_hp_q_irl<-lm(hp_base00_real_q_irl ~ hp_ttrend + I(hp_ttrend^2)+ I(hp_ttrend^3)+ I(hp_ttrend^4)++ I(hp_ttrend^5)++ I(hp_ttrend^6)++ I(hp_ttrend^7)+ I(hp_ttrend^8)+ I(hp_ttrend^9)+ I(hp_ttrend^10))
summary(LM_hp_q_irl)$adj.r.squared 
summary(PM_hp_q_irl)$adj.r.squared #il migliore è di grado 10 :(

plot(hp_base00_real_q_irl,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND Hp trimestrale")
lines(PM_hp_q_irl$fitted.values, lty=2)
plot(seq(from=2000.25,to=2022.5,by=0.25),residuals(PM_hp_q_irl), xlab="", ylab="", main="Residui IRL")
abline(0,0)
trimestri<- c(rep(c("Q1","Q2","Q3","Q4"),22 ), "Q1","Q2")
stag_mod_irl<- lm(residuals(PM_hp_q_irl)~trimestri-1)
summary(stag_mod_irl)
plot(residuals(PM_hp_q_irl))
lines(fitted.values(stag_mod_ita), col="red")
#linea piatta...dunque verificato che non c'è stagionalità
#NB: se ora facessi i residui dello stag mod sarebbero ~= ai res del PM_mod

#3) DANIMARCA
#trend della danimarca
LM_hp_q_dnk<-lm(hp_base00_real_q_dnk ~ hp_ttrend)
PM_hp_q_dnk<-lm(hp_base00_real_q_dnk ~ hp_ttrend + I(hp_ttrend^2)+ I(hp_ttrend^3))
summary(LM_hp_q_dnk)$adj.r.squared 
summary(PM_hp_q_dnk)$adj.r.squared #il migliore è di grado 3

plot(seq(from=2000.25,to=2022.5,by=0.25),hp_base00_real_q_dnk,
     col=2, type="l",xlab="Time", ylab="REAL GDP pro capita",
     main="TREND Hp trimestrale")
lines(seq(from=2000.25,to=2022.5,by=0.25),PM_hp_q_dnk$fitted.values, lty=2)

plot(seq(from=2000.25,to=2022.5,by=0.25),residuals(PM_hp_q_irl), xlab="", ylab="", main="Residui IRL")
abline(0,0)
trimestri<- c(rep(c("Q1","Q2","Q3","Q4"),22 ), "Q1","Q2")
stag_mod_dnk<- lm(residuals(PM_hp_q_irl)~trimestri-1)
summary(stag_mod_dnk)
plot(residuals(PM_hp_q_dnk))
lines(fitted.values(stag_mod_ita), col="red")
#linea piatta...dunque verificato che non c'è stagionalità
#NB: se ora facessi i residui dello stag mod sarebbero ~= ai res del PM_mod



#############################################################################
# C) TASSO DI DISOCCUPAZIONE
#############################################################################
plot(ts(unemp_ita, start=c(2000,1), frequency = 4), 
     main="Tasso disoccupazione ITA", col=2, ylim=c(3,16),
     xlab="Time", ylab="Unemployment rate")
lines(ts(unemp_irl, start=c(2000,1), frequency = 4), type="l",col=3)
lines(ts(unemp_dnk, start=c(2000,1), frequency = 4), type="l",col=4)
legend("topleft",cex=0.55, c("ITA", "IRL", "DNK"), col=2:4, lty=1)

#Essendo tassi ci aspettiamo che siano stazionari
#VERIFICA DELLA STAZIONARIETA'
unemp_ttrend<- 1:length(unemp_ita)
#1)italia
LM_unemp_ita<- lm(unemp_ita~ unemp_ttrend)
PM_unemp_ita<- lm(unemp_ita~ unemp_ttrend+ I(unemp_ttrend^2))
summary(LM_unemp_ita)$adj.r.squared
summary(PM_unemp_ita)$adj.r.squared
#Confrontando gli r2 il mod lineare è il migliore
#Osserviamo i coefficienti peò
coef(LM_unemp_ita)
#--->la pendenza è 0.03...pressochè orizzontale: BENE!
plot(resid(LM_unemp_ita))
#sono brutti...anche qui ha senso considerare componente ciclica (ha senso, il tasso dipende dal ciclo dell'economia)

#2)irlanda
LM_unemp_irl<- lm(unemp_irl~ unemp_ttrend)
PM_unemp_irl<- lm(unemp_ita~ unemp_ttrend+ I(unemp_ttrend^2))
summary(LM_unemp_irl)$adj.r.squared
summary(PM_unemp_irl)$adj.r.squared
#Confrontando gli r2 il mod lineare è il migliore
#Osserviamo i coefficienti pe
coef(LM_unemp_irl)
#--->la pendenza è 0.03...pressochè orizzontale: BENE!
plot(resid(LM_unemp_irl))
#sono brutti...anche qui ha senso considerare componente ciclica (ha senso, il tasso dipende dal ciclo dell'economia)

#3)Danimarca
LM_unemp_dnk<- lm(unemp_dnk~ unemp_ttrend)
PM_unemp_dnk<- lm(unemp_dnk~ unemp_ttrend+ I(unemp_ttrend^2))
summary(LM_unemp_irl)$adj.r.squared
summary(PM_unemp_irl)$adj.r.squared
#Confrontando gli r2 il mod lineare è il migliore
#Osserviamo i coefficienti pe
coef(LM_unemp_dnk)
#--->la pendenza è 0.03...pressochè orizzontale: BENE!
plot(resid(LM_unemp_dnk))
#sono brutti...anche qui ha senso considerare componente ciclica (ha senso, il tasso dipende dal ciclo dell'economia)


