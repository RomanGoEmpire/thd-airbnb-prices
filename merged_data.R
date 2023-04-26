#################################################
# Prüfungsstudienarbeit 
# Roman Gerloff und Tabea Haas
#################################################

# Externe Libraries ------------------------------------------------------------
library(ggplot2)


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

summary(data)
summary(data$city)




# grouping  --------------------------------------------------------------------

data$cleanliness_rating <-
  ifelse(
    data$cleanliness_rating == 10 |
      data$cleanliness_rating == 9 |
      data$cleanliness_rating == 8,
    data$cleanliness_rating,
    99
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




# train and test data (70:30, random) ------------------------------------------

n <- length(data[ ,1])
index <- sample(1:n, n, replace = FALSE)
data <- data[index, ]
data.train <- data[1: 36195, ]
data.test <- data[36196: 51707, ]
data[1:10, ]


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

boxplot(realSum ~ city, data, outline = FALSE)
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

# histograms plotting ----------------------------------------------------------

# Zusammenhänge genauer untersuchen --------------------------------------------

# Korrelationen ----------------------------------------------------------------

# Kontingenztafel? -------------------------------------------------------------



#################################################              
# Erstellen eines Entscheidungsbaums
#################################################

#################################################   
# Klassifikation: Logistische Regression
#################################################

#################################################   
# k-nearest Neighbors?
#################################################

#################################################   
# Neuronale Netze
#################################################

























