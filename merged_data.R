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

# realSum plotting -------------------------------------------------------------
boxplot(realSum ~ city, data, outline = FALSE)
par(mfrow = c(1,10))
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
