#------------------------------------------------------------
#IMPORTO I DATASET
GDP_NOM_CAP_Y= read.csv("../data/GDP_NOM_CAP_Y.csv")
GDP_NOM_TOT_Y= read.csv("../data/GDP_NOM_TOT_Y.csv")
GDP_REAL_base15_Q= read.csv("../data/GDP_REAL_base15_Q.csv")
IPC_base15_Q=read.csv("../data/IPC_base15_Q.csv")
IPC_base15_Y= read.csv("../data/IPC_base15_Y.csv")
#HP_base15_NOM_Q= read.csv("../data/HP_base15_NOM_Q.csv")
#HP_base15_NOM_Y= read.csv("../data/HP_base15_NOM_Y.csv")
HP_base15_REAL_Q= read.csv("../data/HP_base15_REAL_Q.csv")
HP_base15_REAL_Y= read.csv("../data/HP_base15_REAL_Y.csv")
UNEMP_Q= read.csv("../data/UNEMP_Q.csv")
#------------------------------------------------------------

#DEFINISCO LE GRANDEZZE MACRO-ECONOMICHE
gdp_nom_ita<- GDP_NOM_TOT_Y[GDP_NOM_TOT_Y$LOCATION == "ITA",]$Value
gdp_nom_irl<- GDP_NOM_TOT_Y[GDP_NOM_TOT_Y$LOCATION == "IRL",]$Value
gdp_nom_dnk<- GDP_NOM_TOT_Y[GDP_NOM_TOT_Y$LOCATION == "DNK",]$Value

gdp_nom_cap_ita<- GDP_NOM_CAP_Y[GDP_NOM_CAP_Y$LOCATION == "ITA",]$Value
gdp_nom_cap_irl<- GDP_NOM_CAP_Y[GDP_NOM_CAP_Y$LOCATION == "IRL",]$Value
gdp_nom_cap_dnk<- GDP_NOM_CAP_Y[GDP_NOM_CAP_Y$LOCATION == "DNK",]$Value

ipc_15_ita<- IPC_base15_Y[IPC_base15_Y$LOCATION == "ITA",]$Value
ipc_00_ita= ipc_15_ita / ipc_15_ita[1]*100
ipc_15_irl<- IPC_base15_Y[IPC_base15_Y$LOCATION == "IRL",]$Value
ipc_00_irl= ipc_15_irl / ipc_15_irl[1]*100
ipc_15_dnk<- IPC_base15_Y[IPC_base15_Y$LOCATION == "DNK",]$Value
ipc_00_dnk= ipc_15_dnk / ipc_15_dnk[1]*100

gdp_real_ita<- gdp_nom_ita / ipc_00_ita
gdp_real_irl<- gdp_nom_irl / ipc_00_irl
gdp_real_dnk<- gdp_nom_dnk / ipc_00_dnk

gdp_real_cap_ita<- gdp_nom_cap_ita / ipc_00_ita
gdp_real_cap_irl<- gdp_nom_cap_irl / ipc_00_irl
gdp_real_cap_dnk<- gdp_nom_cap_dnk / ipc_00_dnk

#1)dati reali annuali base 15
hp_base15_real_y_ita<- HP_base15_REAL_Y[HP_base15_REAL_Y$LOCATION == "ITA",]$Value
hp_base15_real_y_irl<- HP_base15_REAL_Y[HP_base15_REAL_Y$LOCATION == "IRL",]$Value
hp_base15_real_y_dnk<- HP_base15_REAL_Y[HP_base15_REAL_Y$LOCATION == "DNK",]$Value
#Passando a base 2000
hp_base00_real_y_ita<-hp_base15_real_y_ita/hp_base15_real_y_ita[1]*100
hp_base00_real_y_irl<-hp_base15_real_y_irl/hp_base15_real_y_irl[1]*100
hp_base00_real_y_dnk<-hp_base15_real_y_dnk/hp_base15_real_y_dnk[1]*100
#-------------------------------------------------------------------------------
#2)dati reali trimestrali base 15
hp_base15_real_q_ita<- HP_base15_REAL_Q[HP_base15_REAL_Q$LOCATION == "ITA",]$Value
hp_base15_real_q_irl<- HP_base15_REAL_Q[HP_base15_REAL_Q$LOCATION == "IRL",]$Value
hp_base15_real_q_dnk<- HP_base15_REAL_Q[HP_base15_REAL_Q$LOCATION == "DNK",]$Value
#Passando a base 2000
hp_base00_real_q_ita<-hp_base15_real_q_ita/hp_base15_real_q_ita[1]*100
hp_base00_real_q_irl<-hp_base15_real_q_irl/hp_base15_real_q_irl[1]*100
hp_base00_real_q_dnk<-hp_base15_real_q_dnk/hp_base15_real_q_dnk[1]*100

affordability_ita<-gdp_real_cap_ita[-length(gdp_real_cap_ita)]/hp_base00_real_y_ita
affordability_irl<-gdp_real_cap_irl[-length(gdp_real_cap_irl)]/hp_base00_real_y_irl
affordability_dnk<-gdp_real_cap_dnk[-length(gdp_real_cap_dnk)]/hp_base00_real_y_dnk

unemp_ita<- UNEMP_Q[UNEMP_Q$LOCATION == "ITA",]$Value
unemp_irl<- UNEMP_Q[UNEMP_Q$LOCATION == "IRL",]$Value
unemp_dnk<- UNEMP_Q[UNEMP_Q$LOCATION == "DNK",]$Value
