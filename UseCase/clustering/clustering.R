# Load the .csv files
apr14 <- read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/clustering/uber-raw-data-apr14.csv")
may14 <- read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/clustering/uber-raw-data-may14.csv")
jun14 <- read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/clustering/uber-raw-data-jun14.csv")
jul14 <- read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/clustering/uber-raw-data-jul14.csv")
aug14 <- read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/clustering/uber-raw-data-aug14.csv")
sep14 <- read.csv("/home/sathish/Sathish/Sathish/Learning/Big Data/Data Science R/R programming/UseCase/clustering/uber-raw-data-sep14.csv")

#apr14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-apr14.csv")
#may14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-may14.csv")
#jun14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jun14.csv")
#jul14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv")
#aug14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-aug14.csv")
#sep14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-sep14.csv")

library(dplyr)
data14 <- bind_rows(apr14, may14, jun14, jul14, aug14, sep14)

summary(data14)

library(lubridate)

# Separate or mutate the Date/Time columns
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))

head(data14, n=10)

set.seed(20)
clusters <- kmeans(data14[,2:3], 5)

# Save the cluster number in the dataset as column 'Borough'
data14$Borough <- as.factor(clusters$cluster)

str(clusters)


library(ggmap)

NYCMap <- get_map("New York", zoom = 10)
ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(Borough)),data = data14) +
  ggtitle("NYC Boroughs using KMean")

library(DT)

data14$Month <- as.double(data14$Month)
month_borough_14 <- count_(data14, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(month_borough_14)

library(dplyr)
monthly_growth <- month_borough_14 %>%
  mutate(Date = paste("04", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2014")
monthly_growth