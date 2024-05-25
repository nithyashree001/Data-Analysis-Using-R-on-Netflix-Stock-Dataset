getwd()
setwd("C:/Users/n suresh/Downloads")
data <- read.csv("NFLX.csv",header = TRUE ,sep = ",")
print(data)

library(ggplot2)
library(quantmod)
library(TTR)
library(xts)

head(data)
tail(data)
mean(data$High)
mean(data$Low)
mean(data$Upper_Band)
mean(data$Lower_Band)
mean(data$Open)
mean(data$Close)

boxplot(data$AdjClose)

#data analysis
sapply(data, class)
data$High = as.factor(data$High)
data$Low = as.factor(data$Low)
sapply(data , class)
#summary of data
summary(data)
#null values
sum(is.na(data))
str(data$High)
data$High <- as.numeric(data$High)

#variation of High price
ggplot(data, aes(x = High)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "azure4") +
  labs(title = "Distribution of High Prices", 
       x = "High Price", 
       y = "Frequency") +
  theme_minimal()
#correlation test

p1 <-cor.test(as.numeric(data$Open), as.numeric(data$Close),  
              method = "pearson")
print(p1)
p2 <-cor.test(as.numeric(data$Volume),as.numeric(data$RSI),  
              method = "pearson")
print(p2)
p3 <-cor.test(as.numeric(data$Volume),as.numeric(data$MACD),  
              method = "pearson")
print(p3)
p4 <-cor.test(as.numeric(data$Upper_Band),as.numeric(data$Lower_Band),  
              method = "pearson")
print(p4)

p6 <-cor.test(as.numeric(data$MACD),as.numeric(data$MACD_Signal),  
              method = "pearson")
print(p6)
p7 <-cor.test(as.numeric(data$SMA_20),as.numeric(data$Volume),  
              method = "pearson")
print(p7)
data$Date <- as.Date(data$Date)
data$High <- as.numeric(data$High)
data$Low <- as.numeric(data$Low)

# Create the plot with High and Low prices
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = High, color = "High")) +
  geom_line(aes(y = Low, color = "Low")) +
  labs(title = "Stock Prices Over Time", x = "Date", y = "Price") +
  scale_color_manual(values = c("High" = "green", "Low" = "brown1")) +
  theme_minimal()


#Closing Price and 20-Day SMA
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close")) +
  geom_line(aes(y = SMA_20, color = "SMA_20")) +
  labs(title = "Closing Price and 20-Day SMA", x = "Date", y = "Price") +
  scale_color_manual(values = c("Close" = "cyan", "SMA_20" = "deeppink2")) +
  theme_minimal() +
  theme(legend.title = element_blank())


# Calculate 50-day and 200-day moving averages
data$MA_50 <- SMA(data$AdjClose, n = 50)
data$MA_200 <- SMA(data$AdjClose, n = 200)

# Calculate historical volatility
data$Volatility <- runSD(data$RSI, n = 20) * sqrt(252)

# Visualize stock data with moving averages and volatility
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = AdjClose), color = "blue", size = 1) +
  geom_line(aes(y = MA_50), color = "orange", size = 1) +
  geom_line(aes(y = MA_200), color = "red", size = 1) +
  labs(title = "Netflix Inc. Stock Price with Moving Averages",
       x = "Date", y = "Price") +
  theme_minimal()

data_xts <- xts(data$Close, order.by = data$Date)
chartSeries(data_xts, type = "line", name = "Netflix Closing Price")
data_xts <- xts(data$Open, order.by = data$Date)
lineChart(data_xts, line.type = 'h', theme = 'white', TA = NULL)

library(lubridate)

data$Date <- as.character(data$Date) 

splitted <- strsplit(data$Date, "/") 
df <- data.frame( 
  date = data$Date, 
  day = as.integer(sapply(splitted, `[`, 3)), 
  month = as.integer(sapply(splitted, `[`, 2)), 
  year = as.integer(sapply(splitted, `[`, 1)), 
  stringsAsFactors = FALSE
) 

head(df) 

see_the_change <- data.frame( 
  date = data$Date, 
  day = day(data$Date), 
  month = month(data$Date), 
  year = year(data$Date), 
  stringsAsFactors = FALSE
) 

head(see_the_change)


library(dplyr)

# bar plot 
nume_column <- c("Open", "High", 
                 "Low", "Close") 
data_num <- data[, c("Date", nume_column)] 
data_num <- data_num[apply(data_num[, nume_column], 
                           1, function(x) all(is.numeric(x))), ] 

data_num$year <- lubridate::year(data_num$Date) 

data_grouped <- data_num %>% 
  group_by(year) %>% 
  summarise(across(all_of(nume_column), mean)) 

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1)) 

for (i in 1:4) { 
  col <- nume_column[i] 
  barplot(data_grouped[[col]], main = col, 
          xlab = "Year", ylab = "Mean",names.arg = data_grouped$year) 
} 



