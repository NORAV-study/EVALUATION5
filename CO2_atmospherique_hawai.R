  ## Importation des données ##

library(tidyverse)
options(max.print=1000000)
hawai <- read.csv(file =  "hawai.csv", header = TRUE)
head(hawai)



## création de la série temporelle ##


library("forecast")
library("fpp2")
hawai_ts <- ts(hawai %>% dplyr::select(-time),
               start = c(hawai$time[1], 1),
               frequency = 12)
head(hawai_ts)
autoplot(hawai_ts)


## Séparation la série en partie d'entraînement (environ 70% des données) et en partie test ##


hawai_train <- head(hawai_ts, round(length(hawai_ts) * 0.7)) ## le premier bloc qui constitue les données d'entrainement (70%)
head(hawai_train)
h <- length(hawai_ts) - length(hawai_train)
hawai_test <- tail(hawai_ts, h) ## les 30% restant qui comprennent les données tests
head(hawai_test)

autoplot(hawai_train) + autolayer(hawai_test)


## Création du modèle prévisionnel ##

#AUTOCORRELATION#
library("cowplot")
plot_grid(ggAcf(hawai_train) + ggtitle("hawai: Autocorrélation"))


##Calcul de la probabilité pour voir si nos données constituent ou non un bruit blanc##
Box.test(hawai_train, lag = 20, type = "Ljung-Box") ## Pour tester si si la série temporelle entière peut être différenciée d’un bruit blanc.


##Modélisation de la série temporelle##

hawai_model <- forecast::ets(hawai_train, model = "ZZZ") ## pour générer le modèle
hawai_model
autoplot(hawai_model)
hawai_forecast <- forecast(hawai_model,h= 12*13 ) ## pour obtenir la prédiction
autoplot(hawai_forecast)

accuracy(hawai_model)


## Comparaison avec les données test

#Le modèle généré sera évalué avec les données *test* de la série temporelle

autoplot(hawai_forecast)+ autolayer(hawai_test)


test_set_accuracy <- hawai_model %>% forecast(h = 12*13) %>%
  accuracy(hawai_test) ## test set
test_set_accuracy[,c("ME","RMSE","MAE","MAPE","MASE")]


## Analyse des résidus##


checkresiduals(hawai_model)


