#################################################
# Prüfungsstudienarbeit
# Roman Gerloff und Tabea Haas
# Dataset: https://www.kaggle.com/datasets/thedevastator/airbnb-prices-in-european-cities
#################################################

# Externe Libraries ------------------------------------------------------------
library(ggplot2)
library(tree)
library(ANN2)
library(e1071)
library(glmnet)
library(correlation)

#################################################
# Einlesen, Anpassen, Kontrolle der Daten
#################################################

# Formatting -------------------------------------------------------------------
WD = getwd()
setwd(WD)

data <- read.csv(
  "Dataset/merged_data.csv",
  header = TRUE,
  sep = ",",
  fill = TRUE,
  stringsAsFactors = TRUE
)

data$multi <- as.factor(data$multi)
data$biz <- as.factor(data$biz)

data$person_capacity <- as.factor(data$person_capacity)
data$bedrooms <- as.factor(data$bedrooms)
data$cleanliness_rating <- as.factor(data$cleanliness_rating)

data$room_shared <- as.factor(data$room_shared)
data$room_private <- as.factor(data$room_private)
data$multi <- as.factor(data$multi)


# Summary ----------------------------------------------------------------------

summary(data, maxsum = 50)
summary(data$city)

# grouping  --------------------------------------------------------------------

data$cleanliness_rating <-
  ifelse(
    data$cleanliness_rating == 10 |
      data$cleanliness_rating == 9 |
      data$cleanliness_rating == 8 |
      data$cleanliness_rating == 7,
    10,
    data$cleanliness_rating
  )

data$cleanliness_rating <- as.factor(data$cleanliness_rating)

data$bedrooms <-
  ifelse(data$bedrooms == 0 |
           data$bedrooms == 1 |
           data$bedrooms == 2,
         data$bedrooms,
         99)
data$bedrooms <- as.factor(data$bedrooms)

summary(data)

# deskriptive Anylse Werte/Stuff -----------------------------------------------

# Standardabweichungen
sd(data[, "realSum"])
sd(data[, "guest_satisfaction_overall"])
sd(data[, "dist"])
sd(data[, "metro_dist"])
sd(data[, "attr_index"])
sd(data[, "attr_index_norm"])
sd(data[, "rest_index"])
sd(data[, "rest_index_norm"])
sd(data[, "lng"])
sd(data[, "lat"])

# robuste Alternative: Quartile
quantile(data[, "realSum"])
quantile(data[, "guest_satisfaction_overall"])
quantile(data[, "dist"])
quantile(data[, "metro_dist"])
quantile(data[, "attr_index"])
quantile(data[, "attr_index_norm"])
quantile(data[, "rest_index"])
quantile(data[, "rest_index_norm"])
quantile(data[, "lng"])
quantile(data[, "lat"])

# Interquartilsabstände
IQR(data[, "realSum"])
IQR(data[, "guest_satisfaction_overall"])
IQR(data[, "dist"])
IQR(data[, "metro_dist"])
IQR(data[, "attr_index"])
IQR(data[, "attr_index_norm"])
IQR(data[, "rest_index"])
IQR(data[, "rest_index_norm"])
IQR(data[, "lng"])
IQR(data[, "lat"])

# Mean and Medin
mean(data[, "realSum"])
median(data[, "realSum"])

mean(data[, "guest_satisfaction_overall"])
median(data[, "guest_satisfaction_overall"])

mean(data[, "dist"])
median(data[, "dist"])

mean(data[, "metro_dist"])
median(data[, "metro_dist"])

mean(data[, "attr_index"])
median(data[, "attr_index"])

mean(data[, "attr_index_norm"])
median(data[, "attr_index_norm"])

mean(data[, "rest_index"])
median(data[, "rest_index"])

mean(data[, "rest_index_norm"])
median(data[, "rest_index_norm"])

mean(data[, "lng"])
median(data[, "lng"])

mean(data[, "lat"])
median(data[, "lat"])

# realSum plotting -------------------------------------------------------------

par(mfrow = c(2, 6))
boxplot(realSum ~ city, data, outline = FALSE)

par(mfrow = c(1, 10))
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


# Punktewolke plotting ---------------------------------------------------------

# Kein Merwert, da nur Blöcke porträtiert werden

# Korrelationstabelle -----------------------------------------------------------

# histograms plotting ----------------------------------------------------------
# Ausbauen?

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


# Zusammenhänge genauer untersuchen --------------------------------------------

# ???

# Korrelationen ----------------------------------------------------------------

cor(data$realSum, data$guest_satisfaction_overall)
cor(data$realSum, data$dist)
cor(data$realSum, data$metro_dist)
cor(data$realSum, data$attr_index)
cor(data$realSum, data$attr_index_norm)
cor(data$realSum, data$rest_index)
cor(data$realSum, data$rest_index_norm)


# Methods ----------------------------------------------------------------------

shuffle_data <- function(data) {
  n <- length(data[, 1])
  index <- sample(1:n, n, replace = FALSE)
  data <- data[index, ]
  return(data)
}

train_test_divider <- function(data, percentage) {
  n <- round(nrow(data) * percentage)
  return (list(train = data[1:n,], test = data[(n + 1):nrow(data),]))
}


create_model_matrix <- function(target_and_predictors, data) {
  matrix <- model.matrix(target_and_predictors, data = data)
  matrix [, 1] # Remove Intercept
  return(matrix)
}

calculate_mean <- function(model, X, y) {
  prediction <- predict(model, X)$predictions
  mean = mean(abs(prediction - y))
  return (mean)
}


devided_data <- train_test_divider(data, 0.7)
train <- devided_data$train
test <- devided_data$test


target_and_predictors <-
  realSum ~ city  + room_type + room_shared + room_private  + person_capacity +
  host_is_superhost + multi + biz + guest_satisfaction_overall +
  bedrooms + dist + metro_dist + attr_index + attr_index_norm + rest_index +
  rest_index_norm

# Not used:
# cleanliness_rating + day  + lng + lat

#################################################
# Variablenselektion mithilfe von LASSO
#################################################

X.train <- model.matrix(realSum ~. , data.train)
X.train <- X.train[,-1]   # Entferne den Intercept
summary(X.train)
y.train <- data.train[,"realSum"]

# 100-fache Durchführung von LASSO zur Variablenselektion
m <- length(X.train[1,])
total.numbers <- rep(0,m)
RUNS <- 100

for( run in 1:RUNS ){
  
  model.lasso <- cv.glmnet(X.train,y.train)
  beta <- coef(model.lasso,s="lambda.1se")[-1,1]  # Vektor der Koeffizienten
  total.numbers <- total.numbers + ifelse( beta != 0, 1, 0)  
  
}

total.numbers <- as.matrix(total.numbers) 
rownames(total.numbers) <- names(beta)     
total.numbers

Schwelle <- 10
temp <- total.numbers[total.numbers[,1] >= Schwelle, 1]
auswahl <- names(temp)

# Die neuen Datensätze (funktioniert so leider noch nicht)

#data.train.LASSO <- data.train[,c("realSum",auswahl)]
#data.test.LASSO <- data.test[,c("realSum",auswahl)]


#################################################
# Regression: Logistische Regression
#################################################

model <-
  lm(target_and_predictors,
     data = data)
model

y <- data$realSum
prognosis <- model$fitted.values

Error <- mean(abs(y - prognosis))
Error

#################################################
# Erstellen eines Entscheidungsbaums
#################################################

tree <-
  tree(
    target_and_predictors,
    data = train
  )
tuning <- cv.tree(tree, K = 5)
t <- which.min(tuning$dev)
Anzahl.Endknoten <- tuning$size[t]

model <- prune.tree(tree, best = Anzahl.Endknoten)

# Berechnung der Prognoseergebnisse auf den Testdaten:

X.test <-
  test[, c(
    "city",
    "room_type",
    "room_shared",
    "room_private",
    "person_capacity",
    "host_is_superhost",
    "multi",
    "biz",
    "guest_satisfaction_overall",
    "bedrooms",
    "dist",
    "metro_dist",
    "attr_index",
    "attr_index_norm",
    "rest_index",
    "rest_index_norm"
  )]

prognosen <- predict(model, X.test)

# Berechnung des mittleren Prognosefehlers (MAD)

y.test <- test[, "realSum"]
mean = mean(abs(y.test - prognosen))

plot(model)
text(model)

#################################################
# Regression mit SVR (Support Vector Regression)
#################################################

# Definition der Tuning-Parameter

# cc <- seq(-5,10,1)    
# cg <- seq(-4,1,0.5) 

# Berechnung des Modells

# tuning <- tune.svm(realSum ~ xxxx, data=train, scale = TRUE, 
#   type = "eps-regression", kernel = "radial", gamma = 10^cg, cost = 2^cc, 
#   epsilon = 0.1, tunecontrol = tune.control(sampling = "cross",cross=5)))
# print(tuning)
# model <- tuning$best.model 

# Berechnen von Prognosen
# X.test <- test[,c("Geschlecht","Alter","Groesse")]
# prognosis <- predict(model,X.test)

# Berechnung des mittleren Prognosefehlers (MAD)
# y.test <- test$realSum
# mean(abs(y.test-prognosen)



#################################################
# Neuronale Netze
#################################################

X <- create_model_matrix(target_and_predictors, train)
y <- train$realSum

X_test <- create_model_matrix(target_and_predictors, test)
y_test <- test$realSum

model <-
  neuralnetwork(
    X,
    y,
    hidden.layers = c(16,8,4,2),
    loss.type = "absolute",
    learn.rates = 0.003,
    n.epochs =  50,
    batch.size = 32,
    regression = TRUE,
    verbose = TRUE
  )
mean_train <- calculate_mean(model, X, y)
mean_train
mean_test <- calculate_mean(model, X_test, y_test)
mean_test

