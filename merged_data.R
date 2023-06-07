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
data$room_shared <- as.factor(data$room_shared)
data$room_private <- as.factor(data$room_private)
data$multi <- as.factor(data$multi)


# Summary ----------------------------------------------------------------------

summary(data, maxsum = 50)
summary(data$city)

# Data Cleaning ----------------------------------------------------------------

data <- subset(data, data$realSum <= 1000)
data <- subset(data, data$dist <= 12)
data <- subset(data, data$attr_index <= 1500)
data <- subset(data, data$rest_index <= 3500)

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

summary(data)

# deskriptive Analyse Werte ----------------------------------------------------

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

# Mean and Median
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

# Fühlt sich eher irrelevant an

# histograms plotting ----------------------------------------------------------
# Ausbauen?

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

# Zusammenhänge genauer untersuchen --------------------------------------------

# ?????

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

# Data shulffe and devided in test and train

data <- shuffle_data(data)
devided_data <- train_test_divider(data, 0.7)
train <- devided_data$train
test <- devided_data$test

# Zielvariable und Einfluss variablen

target_and_predictors <-
  realSum ~ city + day + room_type  + room_private  + person_capacity  + multi + biz + cleanliness_rating + guest_satisfaction_overall +
  bedrooms + dist + metro_dist   + attr_index_norm + rest_index_norm

# Not used:
# lng + lat + attr_index  + rest_index + room_shared + host_is_superhost

#################################################
# Variablenselektion mithilfe von LASSO
#################################################

library(glmnet)

X <- create_model_matrix(target_and_predictors, train)
y <- train$realSum
X_test <- create_model_matrix(target_and_predictors, test)
y_test <- test$realSum

summary(X)

# Berechnung von LASSO mit der Standardeinstellung s="lambda.min"
model.lasso <- cv.glmnet(X, y)
coef(model.lasso, s = "lambda.min")

# Wiederholung der Berechnung von LASSO mit der restriktiveren Einstellung s="lambda.1se"
model.lasso <- cv.glmnet(X, y)
coef(model.lasso, s = "lambda.1se")

m <- length(X[1, ])
total.numbers <- rep(0, m)

RUNS <- 100

for (run in 1:RUNS) {
  print(run)
  model.lasso <- cv.glmnet(X, y)
  beta <-
    coef(model.lasso, s = "lambda.1se")[-1, 1]  # Vektor der Koeffizienten (ohne Intercept)
  total.numbers <-
    total.numbers + ifelse(beta != 0, 1, 0)  # Auswahl der Koeffizienten die ungleich Null sind
  
}

total.numbers <-
  as.matrix(total.numbers)  # wie oft wurde welche Variable gewählt
rownames(total.numbers) <-
  names(beta)     # die Zeilen sollen die Namen der Variablen haben
total.numbers

Schwelle <- 50
temp <- total.numbers[total.numbers[, 1] >= Schwelle, 1]
auswahl <- names(temp)

#################################################
# Regression: Logistische Regression
#################################################


train_model <- function(city, data) {
  model <-
    lm(
      realSum ~ day + room_type  + room_private  + person_capacity  + multi + biz + cleanliness_rating + guest_satisfaction_overall +
        bedrooms + dist + metro_dist   + attr_index_norm + rest_index_norm
      ,
      data = data
    )
  model
  
  y <- data$realSum
  prognosis <- model$fitted.values
  
  Error <- mean(abs(y - prognosis))
  print(city)
  print(Error)
}

data_amsterdam <- subset(data, data$city == "amsterdam")
train_model("amsterdam", data_amsterdam)
data_athens <- subset(data, data$city == "athens")
train_model("athens", data_athens)
data_barcelona <- subset(data, data$city == "barcelona")
train_model("barcelona", data_barcelona)
data_berlin <- subset(data, data$city == "berlin")
train_model("berlin", data_berlin)
data_budapest <- subset(data, data$city == "budapest")
train_model("budapest", data_budapest)
data_lisbon <- subset(data, data$city == "lisbon")
train_model("lisbon", data_lisbon)
data_london <- subset(data, data$city == "london")
train_model("london", data_london)
data_paris <- subset(data, data$city == "paris")
train_model("paris", data_paris)
data_rome <- subset(data, data$city == "rome")
train_model("rome", data_rome)
data_vienna <- subset(data, data$city == "vienna")
train_model("vienna", data_vienna)


# All Cities

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
  tree(target_and_predictors,
       data = train)
tuning <- cv.tree(tree, K = 5)
t <- which.min(tuning$dev)
Anzahl.Endknoten <- tuning$size[t]

model <- prune.tree(tree, best = Anzahl.Endknoten)

X.test <-
  test[, c(
    "city",
    'day',
    "room_type",
    "room_shared",
    "room_private",
    "person_capacity",
    "host_is_superhost",
    "multi",
    "biz",
    "cleanliness_rating",
    "guest_satisfaction_overall",
    "bedrooms",
    "dist",
    "metro_dist",
    "attr_index",
    "attr_index_norm",
    "rest_index",
    "rest_index_norm",
    "lng",
    "lat"
  )]

prognosen <- predict(model, X.test)

y.test <- test[, "realSum"]
mean = mean(abs(y.test - prognosen))
print(mean)

plot(model)
text(model)

#################################################
# Regression mit SVR (Support Vector Regression)
#################################################

cc <- seq(-5, 10, 1)
cg <- seq(-4, 1, 0.5)

tuning <-
  tune.svm(
    target_and_predictors,
    data = train,
    scale = TRUE,
    type = "eps-regression",
    kernel = "radial",
    gamma = 10 ^ cg,
    cost = 2 ^ cc,
    epsilon = 0.1,
    tunecontrol = tune.control(sampling = "cross", cross = 5000)
  )
print(tuning)
model <- tuning$best.model

X.test <-
  test[, c(
    "city",
    'day',
    "room_type",
    "room_shared",
    "room_private",
    "person_capacity",
    "host_is_superhost",
    "multi",
    "biz",
    "cleanliness_rating",
    "guest_satisfaction_overall",
    "bedrooms",
    "dist",
    "metro_dist",
    "attr_index",
    "attr_index_norm",
    "rest_index",
    "rest_index_norm"
  )]
prognosis <- predict(model, X.test)

y.test <- train$realSum
mean(abs(y.test - prognosen))



#################################################
# Neuronale Netze
#################################################

data <- shuffle_data(data)
devided_data <- train_test_divider(data, 0.7)
train <- devided_data$train
test <- devided_data$test

X <- create_model_matrix(target_and_predictors, train)
y <- train$realSum

X_test <- create_model_matrix(target_and_predictors, test)
y_test <- test$realSum

model <- neuralnetwork(
  X,
  y,
  hidden.layers = c(32, 16, 8, 4),
  loss.type = "huber",
  learn.rates = 0.01,
  n.epochs = 100,
  batch.size = 512,
  regression = TRUE,
  verbose = TRUE
)

mean_train <- calculate_mean(model, X, y)
mean_train
mean_test <- calculate_mean(model, X_test, y_test)
mean_test


# Crazy shit -------------------------------------------------------------------

# Define the combinations
best_combination <- NULL
best_test <- 1000

combinations <- expand.grid(
  layers = list(c(16, 8, 4), c(64, 32), c(16, 8, 8, 4), c(32, 16, 16, 8, 8, 4)),
  loss_types = list("absolute", "squared", "huber", "pseudo-huber"),
  learning_rates = list(0.01, 0.003, 0.001),
  epochs = list(50, 100, 250),
  batch_sizes = list(128, 256, 512)
)

# Define a function to train the neural network and return the performance metric

for (i in (c(1:432))) {
  current_combination <- combinations[i, ]
  
  hidden_layer <- unlist(current_combination$layers)
  loss_type <- unlist(current_combination$loss_types)
  learning_rate <- unlist(current_combination$learning_rates)
  epoch <- unlist(current_combination$epochs)
  batch_size <- unlist(current_combination$batch_sizes)
  
  model <-
    neuralnetwork(
      X,
      y,
      hidden.layers = hidden_layer,
      regression = TRUE,
      loss.type = loss_type,
      learn.rates = learning_rate,
      n.epochs = epoch,
      batch.size = batch_size,
      verbose = TRUE
    )
  
  mean_train <- calculate_mean(model, X, y)
  mean_test <- calculate_mean(model, X_test, y_test)
  
  print(i)
  print(current_combination)
  print(mean_train)
  print(mean_test)
  
  
  if (mean_test < best_test) {
    best_combination <- current_combination
    best_test <- mean_test
  }
}

best_combination
best_test
