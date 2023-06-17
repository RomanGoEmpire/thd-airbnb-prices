#################################################
# Prüfungsstudienarbeit
# Roman Gerloff (221008060) 
# Tabea Haas (00815861)
# Dataset: https://www.kaggle.com/datasets/thedevastator/airbnb-prices-in-european-cities
#################################################

# Laden Externer Libraries -----------------------------------------------------
library(tree) # Für Entscheidungsbaum
library(ANN2) # Für Neuronale Netze
library(glmnet) # Für LASSO
library(correlation) # Für Korrelationen
library(vcd) # Für Cramers V


#################################################
# Einlesen, Anpassen und Kontrolle der Daten
#################################################

# Pfad setzen 
WD = getwd()
setwd(WD)

# Einlesen der Daten
data <- read.csv(
  "Dataset/merged_data.csv",
  header = TRUE,
  sep = ",",
  fill = TRUE,
  stringsAsFactors = TRUE
)

# Umwandeln der Daten ----------------------------------------------------------
data$multi <- as.factor(data$multi)
data$biz <- as.factor(data$biz)
data$person_capacity <- as.factor(data$person_capacity)
data$room_shared <- as.factor(data$room_shared)
data$room_private <- as.factor(data$room_private)
data$multi <- as.factor(data$multi)


# Überprüfen der Daten ---------------------------------------------------------

# Augabe der ersten 10 Zeilen der Summary
data[1:10,]
summary(data)

# Zusammenfassung der Werte der Städte-Spalte
summary(data$city)


# Daten Bereinigung ------------------------------------------------------------

# Entfernen von Ausreißern
data <- subset(data, data$realSum <= 1000)
data <- subset(data, data$dist <= 12)
data <- subset(data, data$attr_index <= 1500)
data <- subset(data, data$rest_index <= 3500)

# Zusammenfassen/Gruppieren und Umwandeln 
data$bedrooms <-
  ifelse(data$bedrooms == 0 |
           data$bedrooms == 1 |
           data$bedrooms == 2,
         data$bedrooms,
         '3+')
data$bedrooms <- as.factor(data$bedrooms)

data$cleanliness_rating <-
  ifelse(
    data$cleanliness_rating == 10 |
      data$cleanliness_rating == 9 |
      data$cleanliness_rating == 8,
    data$cleanliness_rating,
    '7-'
  )
data$cleanliness_rating <- as.factor(data$cleanliness_rating)

# Erneutes Überprüfen der Daten
data[1:10,]
summary(data)


#################################################
# Deskriptive Analyse
#################################################

# Boxplots für die Zielvariabele 'realSum' -------------------------------------

# Für die einzelnen Städte
boxplot(realSum ~ city, data, outline = FALSE)

# Ergebnisse: Unterschiedliche Preisniveaus und -variabilität je nach Stadt 
# Amsterdam erscheint besonders teuer, weist aber auch eine große Preisspanne auf
# London, Paris und Barcelona ebenfalls höhere Preisspanne und höhere Preisniveaus
# Vereinzelt erhebliche Ausreißer -> Entfernen dieser

# Für die restlichen kategorialen Variablen 
par(mfrow = c(2, 5))
boxplot(realSum ~ day, data, outline = FALSE)
boxplot(realSum ~ room_type, data, outline = FALSE)
boxplot(realSum ~ room_shared, data, outline = FALSE)
boxplot(realSum ~ room_private, data, outline = FALSE)
boxplot(realSum ~ person_capacity, data, outline = FALSE)
boxplot(realSum ~ host_is_superhost, data, outline = FALSE)
boxplot(realSum ~ multi, data, outline = FALSE)
boxplot(realSum ~ biz, data, outline = FALSE)
boxplot(realSum ~ cleanliness_rating, data, outline = FALSE)
boxplot(realSum ~ bedrooms, data, outline = FALSE)
# Ergebnisse: Wochentage/-end scheint keinen Einfluss auf den Preis zu haben;
# AirBnB's, bei denen man ein ganzes aus/ Apartment hat, scheinen einen höheren 
# Preis zu haben, ebenso tragen mehr Schlafgelegenheiten und Personenkapazität hierzu bei.
# Andere Variablen wie Cleanliness Rating, ob der Host Superhost ist etc. scheinen
# auf den erste Blick eher für sinkende Preise zu sorgen (evtl kommen sie weniger oft vor?)
# AUch hier: Beseitigen von Ausreißern und Gruppieren von 'bedrooms' und 'cleanliness_rating' 


# Punktewolke  -----------------------------------------------------------------

# Kein Mehrwert, da durch monotone Zusammenhänge nur 'Blöcke' geplottet werden.


# Histogramme  -----------------------------------------------------------------

# Für die metrischen Einflussvariablen
par(mfrow = c(2, 4))

hist(data$realSum,
     main = "Real Sum",
     xlab = "Money in Euro",
     ylab = "Number AirBnB's",
)

hist(
  data$guest_satisfaction_overall,
  main = "Guest Satisfaction Overall",
  xlab = "Rating",
  ylab = "Number of Guests",
  breaks = seq(
    min(data$guest_satisfaction_overall),
    max(data$guest_satisfaction_overall),
    by = 1
  )
)

hist(data$dist,
     main = "Distance City Center",
     xlab = "Distance in km",
     ylab = "Number AirBnB's")

hist(data$metro_dist,
     main = "Distance To Metro",
     xlab = "Distance in km",
     ylab = "Number AirBnB's")

hist(data$attr_index,
     main = "Attraction index",
     xlab = "Rating",
     ylab = "Number AirBnB's")

hist(data$attr_index_norm,
     main = "Attraction index normed",
     xlab = "Rating",
     ylab = "Number AirBnB's")

hist(data$rest_index,
     main = "Rest index",
     xlab = "Rating",
     ylab = "Number AirBnB's")

hist(data$rest_index_norm,
     main = "Attraction index normed",
     xlab = "Rating",
     ylab = "Number AirBnB's")

# Ergebnisse: Alle Variablen haben eine extreme Schieflage
# Anpassen der Daten für mehr Aussagekraft und Übersichtlichkeit der Histogramme 

# Korrelationskoeffizienten ----------------------------------------------------

# Implizite Verwendung des Pearson-Korrelationskoeffizienten für zwei metrische Variablen
cor(data$realSum, data$guest_satisfaction_overall)
cor(data$realSum, data$dist)
cor(data$realSum, data$metro_dist)
cor(data$realSum, data$attr_index)
cor(data$realSum, data$attr_index_norm)
cor(data$realSum, data$rest_index)
cor(data$realSum, data$rest_index_norm)

# Ergebnisse: Attraction und Restaurant Index mit den höchsten (positiven) Korrelationen
# diese sind aber immernoch relativ schwach; 
# Mehr Attraktionen/Aktivitäten in der Umgebung könnten also zu (leicht) steigenden Preisen führen

# Verwendung von Cramers V, um die Korrelation zwischen einer metrischen und 
# kategorialen Variable zu berechnen

# Speichern der kategorialen Variablen
kat_var <- c("city", "day", "room_type", "room_shared",
             "room_private", "person_capacity", 
             "host_is_superhost", "multi", "biz")

# for-Schleife zum Berchnen und Ausgabe von Cramers V für alle kat. Variablen
for (var in kat_var) {
  cross_table <- table(data$realSum, data[[var]])
  cramers_v <- assocstats(cross_table)$cramer
  cat("Cramers V für", var, ":", cramers_v, "\n")
}

# Ergebnisse: sehr starke bis moderate Assoziationen zwischen der metr. Zielvariable
# 'realSum' und der jeweiligen kat. Variable -> starker Einfluss der kat. Variablen
# auf die Zielvariable
# Gerade Standorte, Zimmertyp und Art des AirBnS's, scheinen einen starken Einfluss 
# auf den Preis aufzuweisen; Ebenfalls zu sehen bei den entsprechenden Boxplots


#################################################
# Methoden
#################################################

# Funktion zur willkürlichen Änderung der Reihenfolge der Daten
shuffle_data <- function(data) {
  n <- length(data[, 1]) # Anzahl Zeilen im Datensatz bestimmen
  index <- sample(1:n, n, replace = FALSE) # Zufälligen Indizes erstellen
  data <- data[index, ] # Daten neu anordnen
  return(data)
}

# Funktion zur Aufteilung der Daten in Trainings- und Testdaten 
# nach übergebenen Prozentsatz
train_test_divider <- function(data, percentage) {
  n <- round(nrow(data) * percentage) 
  return (list(train = data[1:n,], test = data[(n + 1):nrow(data),]))
}

# Funktion zum Erstellen einer Matrix basierend auf 
# Zielvariable und potentiellen Einflussvariablen
create_model_matrix <- function(target_and_predictors, data) {
  matrix <- model.matrix(target_and_predictors, data = data)
  matrix [, 1] # Entfernen des Intercept
  return(matrix)
}

# Funktion zur Berechnung des Mean Absolute Errors (MAE) 
# zwischen Modell und tatsächlichen Wert
calculate_MAE <- function(model, X, y) {
  prediction <- predict(model, X)$predictions # Vorhersage der Zielvariable
  mean = mean(abs(prediction - y)) # Berechnung MAE
  return (mean)
}

# Funktion zur Berechnung des Mean Absolute Percentage Errors (MAPE)
calculate_MAPE <- function(model, X, y) {
  prediction <- predict(model, X)$predictions # Vorhersage der Zielvariable
  mape <- mean(abs((prediction - y) / y)) * 100 # Berechnung MAPE in Prozent
  return(mape)
}

# Durcheinander 'Würfeln' und Aufteilung des Datensatzes in Trainings- und Testdaten 
# mit 70% Trainings- und 30% Testdaten
data <- shuffle_data(data)
devided_data <- train_test_divider(data, 0.7) 

# Zuweisung von Trainings- und Testdatensatz
train <- devided_data$train
test <- devided_data$test

# Speichern der Ziel- und Einflussvariable(n), um Redundanz zu vermeiden
target_and_predictors <-
  realSum ~ city + day + room_type + room_shared  + room_private  + person_capacity  + multi +
  biz + cleanliness_rating + guest_satisfaction_overall + bedrooms + dist + 
  metro_dist + attr_index + attr_index_norm + rest_index_norm 

# Nich verwendete Variablen für nachfolgenden Berechnungen:
# rest_index + host_is_superhost + lng + lat
# Begründung: LASSO etc. 



#################################################
# Variablenselektion mithilfe von LASSO
#################################################

# Erstellen Matrix und Zuweisung Zielvariable für Trainingsdaten
X <- create_model_matrix(target_and_predictors, train)
X <- X[,-1] # Entfernen Intercept
y <- train$realSum

# Erstellen Matrix und Zuweisung Zielvariable für Testdaten
X_test <- create_model_matrix(target_and_predictors, test)
y_test <- test$realSum

# Ausgabe Summary der Trainingsdaten-Matrix
summary(X)

# Berechnung von LASSO mit der Standardeinstellung s="lambda.min"
model.lasso <- cv.glmnet(X, y)
coef(model.lasso, s = "lambda.min")

# Wiederholung der Berechnung von LASSO mit der restriktiveren Einstellung s="lambda.1se"
model.lasso <- cv.glmnet(X, y)
coef(model.lasso, s = "lambda.1se")

# Wiederholung der Berechnung 100 mal; weitere Verwendung der Variablen, die häufig
# ausgewählt wurden
# Verwendung der restriktiveren Einstellung
m <- length(X[1, ])
total.numbers <- rep(0, m)

# 100 Durchläufe
RUNS <- 100

for (run in 1:RUNS) {
  print(run)
  model.lasso <- cv.glmnet(X, y)
  beta <-
    coef(model.lasso, s = "lambda.1se")[-1, 1]  # Vektor der Koeffizienten (ohne Intercept)
  total.numbers <-
    total.numbers + ifelse(beta != 0, 1, 0)  # Auswahl der Koeffizienten die ungleich Null sind
  
}

total.numbers <- as.matrix(total.numbers)  # wie oft wurde welche Variable gewählt
rownames(total.numbers) <- names(beta)     # die Zeilen sollen die Namen der Variablen haben
total.numbers

# Ergebnisse: 



#################################################
# Lineare Regression
#################################################

# Berechnung der Regression
model <- lm(target_and_predictors, data = data)
model

# Berechnung des mittleren Prognosefehlers (MAD)
y <- data$realSum
prognosis <- model$fitted.values
Error <- mean(abs(y - prognosis))
Error

# Ergebnis: Durchschnittliche absolute Abweichung zwischen den vom Modell vorhergesagten 
# Preisen und den tatsächlichen Preisen beträgt in etwa 72.55€ (gerundet)



#################################################
# Erstellen eines Entscheidungsbaums
#################################################

# Berechnung des Modells
tree <- tree(target_and_predictors, data = train)

# Kreuzvalidierung für die optimale Anzahl an Endknoten
tuning <- cv.tree(tree, K = 5)

# Der Baum mit der optimalen Anzahl an Endknoten
t <- which.min(tuning$dev)
Anzahl.Endknoten <- tuning$size[t]
model <- prune.tree(tree, best = Anzahl.Endknoten)

# Ausgabe des Finalen Baums mit Text
plot(model)
# Anpassung des Textes für bessere Lesbarkeit
text(model, pretty = 0, cex = 0.8)

# Ergebnisse: wichtigstes Merkmal: Stadt (city), Anzahl Bedrooms, Room_Type,
# aber auch Attraction Index und Restaurant Index (genormt)
# Entspricht deskriptiver Analyse und Paper


# Berechnung der Prognoseergebnisse auf den Testdaten
X.test <-
  test[, c(
    "city",
    'day',
    "room_type",
    "room_shared",
    "room_private",
    "person_capacity",
    "multi",
    "biz",
    "cleanliness_rating",
    "guest_satisfaction_overall",
    "bedrooms",
    "dist",
    "metro_dist",
    "attr_index",
    "attr_index_norm",
    "rest_index_norm"
  )]

# Berechnung des mittleren Prognosefehlers (MAD) 
prognosen <- predict(model, X.test)
y.test <- test[, "realSum"]
mean = mean(abs(y.test - prognosen))
print(mean)

# Ergebnis: MAE beträgt in etwa 81.51€  (gerundet)

#################################################
# Neuronale Netze
#################################################

# Erstellen einer Matrix für die Trainingsdaten
X <- create_model_matrix(target_and_predictors, train)
# Zuweisung der Zielvariable für die Trainingsdaten
y <- train$realSum

# Erstellen einer Matrix für die Testdaten
X_test <- create_model_matrix(target_and_predictors, test)
# Zuweisung der Zielvariable für die Testdaten
y_test <- test$realSum

# Trainieren des neuronalen Netzes
# mit 4 Hidden Layer, wobei der 1. Hidden Layer 32 Hidden Units hat, der 2. 16, 
# der 3. 8 und der 4. 4 Hidden Layer

model <- neuralnetwork(
  X,
  y,
  hidden.layers = c(32,16,8,4),
  loss.type = "huber",
  learn.rates = 0.003,
  n.epochs = 500,
  batch.size = 512,
  regression = TRUE,
  verbose = TRUE # Fortschrittsanzeige während des Trainings
)

# Ergebnis:
# Validation loss: 0.124136 -> relativ geringe Fehlerrate auf dem Validierungsdatensatz

# Berechnung der Prognosegüte mittels MAE
mean_train <- calculate_MAE(model, X, y)
mean_train
mean_test <- calculate_MAE(model, X_test, y_test)
mean_test

# Ergebnis: MAE beträgt 56.39€ für den Trainings- und 57.73€ für den Testdatendatensatz (gerundet)
# Geringste Abweichung unter den verwendeten Lernverfahren

# Berechnung der Prognosegüte mittels MAPE
mean_train <- calculate_MAPE(model, X, y)
mean_train
mean_test <- calculate_MAPE(model, X_test, y_test)
mean_test

# Ergebnis: durchschnittliche prozentuale Abweichung der Vorhersagen 
# vom tatsächlichen Preis beträgt in etwa 23.73% bzw. 23.89% 

