#Esercizio per ricavare stimatori di massima verosimiglianza

#1) prima lo facciamo per il modello dei pantaloni

pant <- read.table("data/fig.2_18.dat", h=F)
plot(ts(pant, start=1971))
#NB: guardando il grafico sembrerebbe appropriato usare o curva logistica o gompertz
#Però prima provo a stimare un trend lineare per vedere quanto il modello farebbe schifo
pant <- pant[,1] #riduco la tabella ad un semplice vettore
ttrend <- 1:length(pant)
#Creo un dataframe con dati vendite e ttrend
dati <- data.frame(pant=pant, ttrend=ttrend)
olslinmod<- lm(pant~ttrend, dati)
plot(dati$pant, type="l")
abline(olslinmod, col="red")

#ottengo i parametri dei minimi quadrati con una stima numerica

mylin<- function(parms, y, ttrend){
    b0=parms[1]
    b1=parms[2]
    yhat<- b0 + b1*ttrend
    uhat<- y-yhat #i residui
    return (sum(uhat^2))
}


myopt<- optim(par=c(0,0), #initial guess
              fn=mylin,
              y=pant,
              ttrend=ttrend)

#I parametri stimati numericamente sono
myopt$par
#I parametri che stima la funzione lm
coef(olslinmod)
#NB: vediamo che sono simili!


#Adesso facciamo un esempio con regressione lineare multipla
#esempio delle macchine con potenza e peso
data(mtcars)
y<- 1/mtcars$mpg*100 #è il consumo in galloni/centomiglia
X<- cbind(1, as.matrix(mtcars[,c("hp","wt")])) #modo per avere matrice con hp(potenza) e wt(peso) ma con prima una colonna di tutti 1

#least square function è quella di prima, ma devo adattarla al caso multi:????'
#mylin<- function(parms,y,X)
mymultilin<- function(beta, y, X){
    #beta è vettore di parametri
    yhat<- X%*%beta
    uhat<- y-yhat #i residui
    return (sum(uhat^2))
}
myoptcar<- optim(par=rep(0,dim(X)[[2]]),
                 fn= mymultilin,
                 y=y,
                 X=X)
myoptcar$par
coef(lm(y ~X -1, mtcars)) 
#grafico 3d???