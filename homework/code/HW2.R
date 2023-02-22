#Importo i dati e le librerie
source("import_data.R")
library("forecast")
###############################################
#GDP (solo il reale pro capite)
###############################################
#--------------
#MEDIE MOBILI
#-------------
#1) ITA
plot(ts(gdp_real_cap_ita, start=c(2000,1)), 
     ylab="Real GDP per capita", main="Real GDP per capita ITA")
lines(ma(ts(gdp_real_cap_ita, start=c(2000,1)),3), col="#D35400", lwd=2)
lines(ma(ts(gdp_real_cap_ita, start=c(2000,1)),5), col="#17A589", lwd=2)
legend("topleft",cex=0.8, c("ma(3)", "ma(5)"), col=c("blue", "red"), lty=1)
#ordine <: liscio di meno--->+ aderenza ai dati (e viceversa)
#2) IRL
plot(ts(gdp_real_cap_irl, start=c(2000,1)),
     ylab="Real GDP per capita", main="Real GDP per capita IRL")
lines(ma(ts(gdp_real_cap_irl, start=c(2000,1)),3), col="#D35400", lwd=2)
lines(ma(ts(gdp_real_cap_irl, start=c(2000,1)),5), col="#17A589", lwd=2)
#3) DNK
plot(ts(gdp_real_cap_dnk, start=c(2000,1)),
     ylab="Real GDP per capita", main="Real GDP per capita DNK")
lines(ma(ts(gdp_real_cap_dnk, start=c(2000,1)),3), col="#D35400",lwd=2)
lines(ma(ts(gdp_real_cap_dnk, start=c(2000,1)),5), col="#17A589", lwd=2)

#recap: dati annuali, le medie mobili usate per stimare trend-ciclo
#NB: i nostri dati sono tutti destagionalizzati, quindi le medie mobili non le posso usare per stimare/togliere la stag.

#------------------------
#LISCIAMENTO ESPONENZIALE
#------------------------
#1) ITA
ets_gdp_ita<- ets(ts(gdp_real_cap_ita, start=c(2000,1), frequency = 1))
ets_gdp_ita
plot(ts(gdp_real_cap_ita, start=c(2000,1)), 
     ylab="Real GDP per capita", main="Real GDP per capita ITA")
lines(fitted(ets_gdp_ita), col="#D35400", lwd=3)
plot(forecast(ets_gdp_ita,5), main="GDP forecasts from ETS (ITA)",
     xlab="Time",ylab="Real GDP per capita")
#2) IRL
ets_gdp_irl<- ets(ts(gdp_real_cap_irl, start=c(2000,1)))
ets_gdp_irl
plot(ts(gdp_real_cap_irl, start=c(2000,1)), 
     ylab="Real GDP per capita", main="Real GDP per capita IRL")
lines(fitted(ets_gdp_irl), col="#D35400", lwd=3)
plot(forecast(ets_gdp_irl,5), main="GDP forecasts from ETS (IRL)",
     xlab="Time",ylab="Real GDP per capita")
#3) DNK
ets_gdp_dnk<- ets(ts(gdp_real_cap_dnk, start=c(2000,1), frequency = 1))
ets_gdp_dnk
plot(ts(gdp_real_cap_dnk, start=c(2000,1)), 
     ylab="Real GDP per capita", main="Real GDP per capita DNK")
lines(fitted(ets_gdp_dnk), col="#D35400", lwd=3)
plot(forecast(ets_gdp_dnk,5), main="GDP forecasts from ETS (DNK)",
     xlab="Time",ylab="Real GDP per capita")
#NOTA: essendo dati annuali (pochi) viene schifoso

#------------------------
#MODELLO ARIMA
#------------------------
#1) ITA
arima_gdp_ita<- auto.arima(ts(gdp_real_cap_ita, start=c(2000,1)))
arima_gdp_ita #--->arima(0,1,0), quindi random walk!
plot(forecast(arima_gdp_ita,5), main="GDP forecasts from ARIMA(0,1,0)",
     xlab="Time",ylab="Real GDP per capita")
plot(ts(gdp_real_cap_ita, start=c(2000,1)), 
     ylab="Real GDP per capita", main="Real GDP per capita ITA")
lines(fitted(ets_gdp_ita), col="#D35400", lwd=3)
lines(fitted(arima_gdp_ita), col="#17A589", lwd=2) 

#2) IRL
arima_gdp_irl<- auto.arima(ts(gdp_real_cap_irl, start=c(2000,1)))
arima_gdp_irl #--->arima(0,2,1)
plot(forecast(arima_gdp_irl,5), main="GDP forecasts from ARIMA(0,2,1)",
     xlab="Time",ylab="Real GDP per capita")

plot(ts(gdp_real_cap_irl, start=c(2000,1)), 
     ylab="Real GDP per capita", main="Real GDP per capita IRL")
lines(fitted(ets_gdp_irl), col="#D35400", lwd=3)
lines(fitted(arima_gdp_irl), col="#17A589", lwd=2)

#NB: molto meglio rispetto al lisciamento
#3) DNK
arima_gdp_dnk<- auto.arima(ts(gdp_real_cap_dnk, start=c(2000,1)))
arima_gdp_dnk #--->arima(0,1,0) 
plot(forecast(arima_gdp_dnk,5),main="GDP forecasts from ARIMA(0,1,0)",
     xlab="Time",ylab="Real GDP per capita")

plot(ts(gdp_real_cap_dnk, start=c(2000,1)), 
     ylab="Real GDP per capita", main="Real GDP per capita DNK")
lines(fitted(ets_gdp_dnk), col="#D35400", lwd=3)
lines(fitted(arima_gdp_dnk), col="#17A589", lwd=2)

#NOTA: far vedere il passaggio da ets a arima
#ITA: schifo prima schifo dopo
#IRL: schifo prima bene dopo
#DNK: bene prima bene dopo (~ uguali)


###############################################
#HOUSE PRICING
###############################################
#--------------
#MEDIE MOBILI
#-------------
source("spencer_15_ma.R")
#1) ITA
plot(ts(hp_base00_real_q_ita, start=c(2000,1), frequency = 4),
     ylab="Real hp (base 2000)", main="Real Hp ITA")
lines(ma(ts(hp_base00_real_q_ita, start=c(2000,1), frequency = 4),5), col="#D35400", lwd=2.5)
#NB: il fatto che coincidano dimostra che dati destag!!!
#medie mobili di spencer
lines(ts(spencer_15_ma(hp_base00_real_q_ita),start=c(2001,4), frequency = 4), col="#17A589", lwd=2.5)

#2) IRL
plot(ts(hp_base00_real_q_irl, start=c(2000,1), frequency = 4),
     ylab="Real hp (base 2000)", main="Real Hp IRL")
lines(ma(ts(hp_base00_real_q_irl, start=c(2000,1), frequency = 4),4), col="#D35400", lwd=2.5)
#NB: il fatto che coincidano dimostra che dati destag!!!
#medie mobili di spencer
lines(ts(spencer_15_ma(hp_base00_real_q_irl),start=c(2001,4), frequency = 4), col="#17A589", lwd=2.5)

#3) DNK
plot(ts(hp_base00_real_q_dnk, start=c(2000,1), frequency = 4),
     ylab="Real hp (base 2000)", main="Real Hp DNK")
lines(ma(ts(hp_base00_real_q_dnk, start=c(2000,1), frequency = 4),4), col="#D35400", lwd=2.5)
#NB: il fatto che coincidano dimostra che dati destag!!!
#medie mobili di spencer
#sono praticamente la stessa. Infatti:
lines(ts(spencer_15_ma(hp_base00_real_q_dnk),start=c(2001,4), frequency = 4), col="#17A589", lwd=2.5)

plot(spencer_15_ma(hp_base00_real_q_dnk)- ma(hp_base00_real_q_dnk,5)[-c(1,2,3,4,5,6,7,84,85,86,87,88,89,90)], type="l")

#------------------------
#LISCIAMENTO ESPONENZIALE
#------------------------
#1) ITA
ets_hp_ita<- ets(ts(hp_base00_real_q_ita, start=c(2000,1), frequency = 4))
ets_hp_ita
plot(forecast(ets_hp_ita), main="Hp forecasts from ETS (ITA)",
     xlab="Time",ylab="Real Hp")
#2) IRL
ets_hp_irl<- ets(ts(hp_base00_real_q_irl, start=c(2000,1), frequency = 4))
ets_hp_irl
plot(forecast(ets_hp_irl), main="Hp forecasts from ETS (IRL)",
     xlab="Time",ylab="Real Hp")
#3) DNK
ets_hp_dnk<- ets(ts(hp_base00_real_q_dnk, start=c(2000,1), frequency = 4))
ets_hp_dnk
plot(forecast(ets_hp_dnk), main="Hp forecasts from ETS (DNK)",
     xlab="Time",ylab="Real Hp")
#NOTA: essendo dati trim (tanti) viene decente
#------------------------
#MODELLO ARIMA
#------------------------
#1) ITA
arima_hp_ita<- auto.arima(ts(hp_base00_real_q_ita, start=c(2000,1), frequency = 4))
arima_hp_ita #--->arima(0,2,1), NB: non c'è il 2 (.,.,.) poiche dati destag
plot(forecast(arima_hp_ita), main="Hp forecasts from ARIMA(0,2,1)",
     xlab="Time",ylab="Real Hp")
plot(ts(hp_base00_real_q_ita, start=c(2000,1), frequency = 4), 
     ylab="Real hp", main="Real Hp ITA")
lines(fitted(ets_hp_ita), col="#D35400", lwd=3)
lines(fitted(arima_hp_ita), col="#17A589", lwd=2)

#2) IRL
arima_hp_irl<- auto.arima(ts(hp_base00_real_q_irl, start=c(2000,1), frequency = 4))
arima_hp_irl #--->arima(2,0,2) #NB: utile il cfr con ets
plot(forecast(arima_hp_irl), main="Hp forecasts from ARIMA(2,0,2)",
     xlab="Time",ylab="Real Hp")
plot(ts(hp_base00_real_q_irl, start=c(2000,1), frequency = 4), 
     ylab="Real hp", main="Real Hp IRL")
lines(fitted(ets_hp_irl), col="#D35400", lwd=3)
lines(fitted(arima_hp_irl), col="#17A589", lwd=2)

#3) DNK
arima_hp_dnk<- auto.arima(ts(hp_base00_real_q_dnk, start=c(2000,1), frequency = 4))
arima_hp_dnk #--->arima(1,1,0)(0,0,1)
plot(forecast(arima_hp_dnk), main="Hp forecasts from ARIMA(1,1,0)(0,0,1)",
     xlab="Time",ylab="Real Hp")
plot(ts(hp_base00_real_q_dnk, start=c(2000,1), frequency = 4), 
     ylab="Real hp", main="Real Hp DNK")
lines(fitted(ets_hp_dnk), col="#D35400", lwd=3)
lines(fitted(arima_hp_dnk), col="#17A589", lwd=2)

###############################################
#AFFORDABILITY INDEX
###############################################
#--------------
#MEDIE MOBILI
#-------------
#1)ITA
plot(ts(affordability_ita, start=c(2000,1), frequency = 1), 
     ylab="Affordability Index", main="Affordability ITA")
lines(ma(ts(affordability_ita, start=c(2000,1)),3), col="#D35400", lwd=2)
lines(ma(ts(affordability_ita, start=c(2000,1)),5), col="#17A589", lwd=2)
#2)IRL
plot(ts(affordability_irl, start=c(2000,1), frequency = 1), 
     ylab="Affordability Index", main="Affordability IRL")
lines(ma(ts(affordability_irl, start=c(2000,1)),3),col="#D35400", lwd=2)
lines(ma(ts(affordability_irl, start=c(2000,1)),5), col="#17A589", lwd=2)
#3)DNK
plot(ts(affordability_dnk, start=c(2000,1), frequency = 1), 
     ylab="Affordability Index", main="Affordability DNK")
lines(ma(ts(affordability_dnk, start=c(2000,1)),3),col="#D35400", lwd=2)
lines(ma(ts(affordability_dnk, start=c(2000,1)),5), col="#17A589", lwd=2)
#------------------------
#LISCIAMENTO ESPONENZIALE
#------------------------
#1) ITA
ets_aff_ita<- ets(ts(affordability_ita, start=c(2000,1)))
ets_aff_ita
plot(forecast(ets_aff_ita,5),main="Affordability forecasts from ETS (ITA)",
     xlab="Time",ylab="Affordability index")
#2) IRL
ets_aff_irl<- ets(ts(affordability_irl, start=c(2000,1)))
ets_aff_irl
plot(forecast(ets_aff_irl,5),main="Affordability forecasts from ETS (IRL)",
     xlab="Time",ylab="Affordability index")
#3) DNK
ets_aff_dnk<- ets(ts(affordability_dnk, start=c(2000,1)))
ets_aff_dnk
plot(forecast(ets_aff_dnk,5),main="Affordability forecasts from ETS (DNK)",
     xlab="Time",ylab="Affordability index")

#NOTA: essendo dati annuali (pochi) viene schifoso

#------------------------
#MODELLO ARIMA
#------------------------
#1) ITA
arima_aff_ita<- auto.arima(ts(affordability_ita, start=c(2000,1)))
arima_aff_ita #(0,2,0)
plot(forecast(arima_aff_ita,5), main="Affordability forecasts from ARIMA(0,2,0)",
     xlab="Time",ylab="Affordability")

plot(ts(affordability_ita, start=c(2000,1), frequency = 1), 
     ylab="Affordability", main="Affordability ITA", ylim=c(1.8,3.3))
lines(fitted(ets_aff_ita), col="#D35400", lwd=3)
lines(fitted(arima_aff_ita), col="#17A589", lwd=2)


#2) IRL
arima_aff_irl<- auto.arima(ts(affordability_irl, start=c(2000,1)))
arima_aff_irl #(0,1,0)
plot(forecast(arima_aff_irl,5), main="Affordability forecasts from ARIMA(0,1,0)",
     xlab="Time",ylab="Affordability")

plot(ts(affordability_irl, start=c(2000,1), frequency = 1), 
     ylab="Affordability", main="Affordability IRL")
lines(fitted(ets_aff_irl), col="#D35400", lwd=3)
lines(fitted(arima_aff_irl)-0.03, col="#17A589", lwd=3)

#3) DNK
arima_aff_dnk<- auto.arima(ts(affordability_dnk, start=c(2000,1)))
arima_aff_dnk #(1,0,2)
plot(forecast(arima_aff_dnk,5), main="Affordability forecasts from ARIMA(1,0,2)",
     xlab="Time",ylab="Affordability")

plot(ts(affordability_dnk, start=c(2000,1), frequency = 1), 
     ylab="Affordability", main="Affordability DNK", ylim=c(1.9,3))
lines(fitted(ets_aff_dnk), col="#D35400", lwd=3)
lines(fitted(arima_aff_dnk), col="#17A589", lwd=3)

###############################################
#UNEMPLOYMENT RATE
###############################################
#--------------
#MEDIE MOBILI
#-------------
#1)ITA
plot(ts(unemp_ita, start=c(2000,1), frequency = 4), main="Unemployment Rate ITA")
lines(ma(ts(unemp_ita, start=c(2000,1), frequency=4),4), col="#D35400", lwd=2.5)
#NB: anche qui la ma coincide con i dati poichè destag
#Se vogliamo vedere anche spencer
lines(ts(spencer_15_ma(unemp_ita),start=c(2001,4), frequency = 4), col="#17A589",lwd=2.5)
#Vediamo ora il rapporto di riduzione della varianza
spencer_weights<- c(-3 ,-6, -5 , 3 ,21, 46, 67, 74, 67, 46 ,21 , 3 ,-5 ,-6, -3)
#2)IRL
plot(ts(unemp_irl, start=c(2000,1), frequency = 4),main="Unemployment Rate IRL")
lines(ma(ts(unemp_irl, start=c(2000,1), frequency=4),4), col="#D35400", lwd=2.5)
#NB: anche qui la ma coincide con i dati poichè destag
#Se vogliamo vedere anche spencer
lines(ts(spencer_15_ma(unemp_irl),start=c(2001,4), frequency = 4), col="red",lwd=2)

#3)DNK
plot(ts(unemp_dnk, start=c(2000,1), frequency = 4),main="Unemployment Rate IRL")
lines(ma(ts(unemp_dnk, start=c(2000,1), frequency=4),4), col="#D35400", lwd=2.5)
#NB: anche qui la ma coincide con i dati poichè destag
#Se vogliamo vedere anche spencer
lines(ts(spencer_15_ma(unemp_dnk),start=c(2001,4), frequency = 4), col="red",lwd=2)

#Bonus: RAPPORTO DI RIDUZIONE della varianza
spencer_weights<- c(-3 ,-6, -5 , 3 ,21, 46, 67, 74, 67, 46 ,21 , 3 ,-5 ,-6, -3)
rrv_spencer<-sum((spencer_weights/320)^2)
rrv_spencer #-->~ 19.3%
rrv_ma5= 1/4#-->= 25.0%

#------------------------
#LISCIAMENTO ESPONENZIALE
#------------------------
#1) ITA
ets_unemp_ita<- ets(ts(unemp_ita, start=c(2000,1), frequency = 4))
ets_unemp_ita
plot(forecast(ets_unemp_ita),
     main="Unemployment forecasts from ETS (ITA)")
#2) IRL
ets_unemp_irl<- ets(ts(unemp_irl, start=c(2000,1), frequency = 4))
ets_unemp_irl
plot(forecast(ets_unemp_irl),
     main="Unemployment forecasts from ETS (IRL)")
#3) DNK
ets_unemp_dnk<- ets(ts(unemp_dnk, start=c(2000,1), frequency = 4))
ets_unemp_dnk
plot(forecast(ets_unemp_dnk),
     main="Unemployment forecasts from ETS (DNK)")


#------------------------
#MODELLO ARIMA
#------------------------
#1) ITA
arima_unemp_ita<- auto.arima(ts(unemp_ita, start=c(2000,1), frequency = 4))
arima_unemp_ita #(0,1,3)
plot(forecast(arima_unemp_ita), main="Affordability forecasts from ARIMA(0,1,3)")

plot(ts(unemp_ita, start=c(2000,1), frequency = 4), 
     ylab="Unemployment Rate", main="Unemployment ITA")
lines(fitted(ets_unemp_ita), col="#D35400", lwd=2)
lines(fitted(arima_unemp_ita), col="#17A589", lwd=2)

#2) IRL
arima_unemp_irl<- auto.arima(ts(unemp_irl, start=c(2000,1), frequency = 4))
arima_unemp_irl #(0,2,2)(0,0,2)
plot(forecast(arima_unemp_irl), main="Affordability forecasts from ARIMA(0,2,2)(0,0,2)")

plot(ts(unemp_irl, start=c(2000,1), frequency = 4), 
     ylab="Unemployment Rate", main="Unemployment IRL")
lines(fitted(ets_unemp_irl), col="#D35400", lwd=2)
lines(fitted(arima_unemp_irl), col="#17A589", lwd=2)

#3) DNK
arima_unemp_dnk<- auto.arima(ts(unemp_dnk, start=c(2000,1), frequency = 4))
arima_unemp_dnk #(0,1,0)
plot(forecast(arima_unemp_dnk), main="Affordability forecasts from ARIMA(0,1,0)")

plot(ts(unemp_dnk, start=c(2000,1), frequency = 4), 
     ylab="Unemployment Rate", main="Unemployment DNK")
lines(fitted(ets_unemp_dnk), col="#D35400", lwd=2)
lines(fitted(arima_unemp_dnk), col="#17A589", lwd=2)




#######################################################
#PUNTO BONUS: confronto accuracy dei modelli (training set / test set)
#Considerazione su hp (perchè trim, quindi + dati per il fit, e poi xke sembravano i modelli migliori per previsioni)
# accuracy la facciamo solo per l'arima (che è il modello + completo)
#TEST-SET: ultimi 2 anni (=8 trimestri)
#1)ITA

#----------------------------------------------------------------------------
# 1) Definisco training e test set (80% e 20%)
training_hp_ita<- hp_base00_real_q_ita[1:72]
test_hp_ita <- hp_base00_real_q_ita[73:90]
#----------------------------------------------------------------------------
# 2) Stimo i modelli sul training set
test_arima_ita<- auto.arima(ts(training_hp_ita,start = c(2000,1),frequency =4))
test_ets_ita<- ets(ts(training_hp_ita, start = c(2000,1), frequency =4))
#----------------------------------------------------------------------------
# 3) Previsioni basate sui modelli
pred_arima_ita<- as.numeric(forecast(test_arima_ita,18)$mean)
pred_ets_ita<- as.numeric(forecast(test_ets_ita,18)$mean)
#----------------------------------------------------------------------------
# 4) Confronto tra previsioni e test-set per stimare l'accuracy
accuracy_arima_ita<- 1- mean(abs(pred_arima_ita- test_hp_ita) / test_hp_ita)
accuracy_ets_ita<- 1- mean(abs(pred_ets_ita- test_hp_ita) / test_hp_ita)
#----------------------------------------------------------------------------

plot(ts(hp_base00_real_q_ita,start=2000, frequency = 4),ylab="Real Hp")
lines(ts(c(training_hp_ita[72],pred_arima_ita), start=c(2017,4),frequency = 4), col="#D35400", lwd=2)
lines(ts(c(training_hp_ita[72],pred_ets_ita), start=c(2017,4),frequency = 4), col="#17A589")

plot(test_hp_ita, type="b",ylim=c(98,110), ylab=" ", xlab=" ")
lines(pred_ets_ita, type="b", col="#D35400", lt=2, pch=4, lwd=2)
lines(pred_arima_ita, type="b", pch=2, col="#17A589", lt=3, lwd=1.5)



#2)IRL
training_hp_irl<- hp_base00_real_q_irl[1:72]
test_hp_irl <- hp_base00_real_q_irl[73:90]
test_arima_irl<- auto.arima(ts(training_hp_irl, start = c(2000,1), frequency =4))
test_ets_irl<- ets(ts(training_hp_irl, start = c(2000,1), frequency =4))
pred_arima_irl<- as.numeric(forecast(test_arima_irl,18)$mean)
pred_ets_irl<- as.numeric(forecast(test_ets_irl,18)$mean)
accuracy_arima_irl<- 1- mean(abs(pred_arima_irl- test_hp_irl) / test_hp_irl)
accuracy_ets_irl<- 1- mean(abs(pred_ets_irl- test_hp_irl) / test_hp_irl)
plot(ts(hp_base00_real_q_irl,start=c(2000,1), frequency = 4),ylab="Real Hp")
lines(ts(c(training_hp_irl[72],pred_arima_irl), start=c(2017,4),frequency = 4), col="red")
lines(ts(c(training_hp_irl[72],pred_ets_irl), start=c(2017,4),frequency = 4), col="#D35400", lwd=2.5)

plot(test_hp_irl, type="b",ylim=c(135,200), ylab=" ", xlab=" ")
lines(pred_ets_irl, type="b", col="#D35400", lt=2, pch=4, lwd=2)
lines(pred_arima_irl, type="b", pch=2, col="#17A589", lt=3, lwd=1.5)

#3) DNK
training_hp_dnk<- hp_base00_real_q_dnk[1:72]
test_hp_dnk <- hp_base00_real_q_dnk[73:90]
test_arima_dnk<- auto.arima(ts(training_hp_dnk, start = c(2000,1), frequency =4))
test_ets_dnk<- ets(ts(training_hp_dnk, start = c(2000,1), frequency =4))
pred_arima_dnk<- as.numeric(forecast(test_arima_dnk,18)$mean)
pred_ets_dnk<- as.numeric(forecast(test_ets_dnk,18)$mean)
accuracy_arima_dnk<- 1- mean(abs(pred_arima_dnk- test_hp_dnk) / test_hp_dnk)
accuracy_ets_dnk<- 1- mean(abs(pred_ets_dnk- test_hp_dnk) / test_hp_dnk)
plot(ts(hp_base00_real_q_dnk,start=c(2000,1), frequency = 4),ylab="Real Hp")
lines(ts(c(training_hp_dnk[72],pred_arima_dnk), start=c(2017,4),frequency = 4), col="red", lw=1)
lines(ts(c(training_hp_dnk[72],pred_ets_dnk), start=c(2017,4),frequency = 4), col="#D35400", lw=1)

plot(test_hp_dnk, type="b",ylim=c(160,195), ylab=" ", xlab=" ")
lines(pred_ets_dnk, type="b", col="#D35400", lt=2, pch=4, lwd=2)
lines(pred_arima_dnk, type="b", pch=2, col="#17A589", lt=3, lwd=1.5)
