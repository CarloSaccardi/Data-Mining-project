
retail <- read.csv("SeriesReport-Not Seasonally Adjusted Sales - Monthly (Millions of Dollars).csv", sep=",", dec = ",")
retail <- retail[1:342,]
# le ultime osservazioni sono mancanti

serie <- ts(retail$Value, start=c(1992,1), end=c(2020,5), frequency=12)
plot(serie)
# la varianza della serie non appare costante quindi applichiamo la funzione logaritmo

log_serie <- log(serie)
plot(log_serie)

mat = matrix(log_serie, ncol=12,byrow=TRUE)
means=apply(mat,2,mean)
plot(means,type="b", main="Prezzo medio per mese", xlab="Settimana", ylab="Media")
# notiamo forti differenze tra le medie dei prezzi quindi è presente stagionalità

nsdiffs(log_serie)
# il test KPSS rileva che la componente stagionale non è stazionaria quindi differenziamo la serie

diff_serie <- diff(log_serie,12)
plot(diff_serie)
# dal grafico notiamo un miglioramento verso la situazione di stazionarietà

nsdiffs(diff_serie)
# anche il test KPSS conferma che la componente stagionale non è più stazionaria

# valutiamo la componente non stagionale
mean(diff_serie)
# la media è prossima a 0 e non appare evidente un trend però valutiamo tramite test ADF

summary(ur.df(diff_serie, "trend", lags=36))
# la serie non è stazionaria ma non presente un trend
summary(ur.df(diff_serie, "drift", lags=36))
# la serie rimane non stazionaria e non presenta intercetta
summary(ur.df(diff_serie, "none", lags=36))
# la serie rimane non stazionaria anche senza intercetta ne trend quindi la non stazionarietà non è dovuta
# ad un trend deterministico ma di tipo stocastico

diff_serie2 = diff(diff_serie, 1)
plot(diff_serie2)
# la serie appare ora stazionaria
# rispetto alla differenzazione precedente, questa è di ordine 1 e non più di ordine 12

ndiffs(diff_serie2)
# come ci aspettavamo dal grafico, la nuova serie è stazionaria anche dal risultato del test KPSS

# utilizziamo test ADF per confrontare il risultato
mean(diff_serie2)
# la media è molto prossima a 0
# utilizziamo l'ipotesi alternativa senza trend ne intercetta
library(urca)
summary(ur.df(diff_serie2, "none", lags=36))
# la statistica test tau (-3.7761) è inferiore alle tavole di riferimento (-1.95)
# rifiutiamo l'ipotesi nulla quindi la serie è stazionaria

# valutiamo ora i lag per AR e MA
library(astsa)
acf2(diff_serie2,48)
# gli spike nei valori unitari sono relativi alla componente stagionale
# questa presenta 2 lag rilvanti nel grafico ACF e 1 nel grafico PACF
# un modello possibile potrebbe avere componenti quindi AR(1) e MA(2).
# gli spike tra unità riguardano invece la componente non stagionale
# notiamo 2 lag significativi nel grafico PACF e 3 nel gradico ACF
# proponiamo come componenti anche in questo caso AR(2) e MA(3).

#scegliamo come modello proposto il seguente:
# (1,0,2)(2,0,3)

auto.arima(diff_serie2)
# la funzione auto.arima consiglia un modello ARIMA(2,0,1)(0,0,2)[12] with zero mean 
# notiamo che i coefficienti che rigurdano la stazionarietà sono in accordo con il modello competitivo proposto,
# mentre i coefficienti ARMA risultano essere più contenuti

auto.arima(log_serie)
# la funzione auto.arima valutata sulla serie iniziale invece non varia rispetto al precedente ad eccezione dei
# termini di stazionarietà
# ARIMA(2,1,1)(0,1,2)[12] 

# Valutiamo i 2 modelli competitivi:
# 1
mod1 <- Arima(log_serie,order=c(1, 1, 2), 
           seasonal=list(order=c(2, 1, 3), period=12))
mod1
sarima(log_serie, 1,1,2, 2,1,3,12)
# dal test Ljung-Box notiamo la significatività di tutte le autocorrelazioni
# dal grafico ACF non sono presenti shock oltre la soglia
# i p-value dei termini del modello sono tutti significativi
???? MA

# 2
mod2 <- Arima(log_serie,order=c(2, 1, 1), 
              seasonal=list(order=c(0, 1, 2), period=12))
mod2
sarima(log_serie, 2,1,1, 0,1,2,12)
# i p-value dei termini del modello sono tutti significativi
# dal test Ljung-Box notiamo un lag di poco sotto la soglia, sintomo di lieve autocorrelazione
# dal grafico ACF notiamo uno shock leggermente oltre la soglia considerata


# confrontiamo i due modelli competitivi tramite la statistica BIC:
# mod1
# AIC -4.61785
# BIC -4.517151
# mod2
# AIC -4.522246
# BIC -4.455113

# seleziono mod1 poichè presenta valori inferiori per la statistica considerata
# il modello proposto è il seguente:
# (1,1,2) (2,1,3)[12]

plot(mod1$x, col='blue',lwd=1)
lines(fitted(mod1), col='red',lwd=1.5)
# dal grafico il modello è sovrapponibile alla serie storica osservata

# evoluzione modello
library(dplyr)
cbind("Start" = serie, "Log" = log(serie), "Log diff12" = diff(log(serie),12),
      "Log diff 12 e 1" = diff(diff(log(serie),12),1),
      "Fitted"=fitted(mod1), "Residuals"=log(serie)-fitted(mod1))  %>%
  autoplot(facets=TRUE)

# forecasting
sarima.for(log_serie, 24, 1,1,2, 2,1,3,12)
# il forecast della serie risente della caduta di vendite nella prima del 2020 e per queste notiamo un andamento
# crescente ma di intensità inferiore rispetto agli anni precedenti.
