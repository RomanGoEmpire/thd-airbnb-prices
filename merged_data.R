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


# realSum plotting ----

splitted <- split(data, data$city)

amsterdam <- splitted$amsterdam$realSum
athens <- splitted$athens$realSum
barcelona <- splitted$barcelona$realSum
berlin <- splitted$berlin$realSum
budapest <- splitted$budapest$realSum
lisbon <- splitted$lisbon$realSum
london <- splitted$london$realSum
paris <- splitted$paris$realSum
rome <- splitted$rome$realSum
vienna <- splitted$vienna$realSum

# Boxplot ----
boxplot(
  amsterdam,
  athens,
  barcelona,
  berlin,
  budapest,
  lisbon,
  london,
  paris,
  rome,
  vienna,
  names = c(
    "amsterdam",
    "athens",
    "barcelona",
    "berlin",
    "budapest",
    "lisbon",
    "london",
    "paris",
    "rome",
    "vienna"
  )
)
plot(data$metro_dist,data$realSum)
# ----