WD = getwd()
setwd(WD)

data <- read.csv(
  "Dataset/amsterdam_weekdays.csv",
  header = TRUE,
  sep = ",",
  fill = TRUE,
  stringsAsFactors = TRUE
)

summary(data)