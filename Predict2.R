# Importing packages
library("ggplot2")
library("raster")
library("sp")
library("broom")
library("dplyr")
library("forecast")
library("tseries")
library("reshape2")
library("zoo")
library("tidyr")
library("readr")
library("fpp2")
library("TTR")

# Read the data
PredictDf = read.csv(file = "KHIS TB 2021.csv",
                     header = TRUE)
PredictDf2 = PredictDf[PredictDf$Data == "TB deaths", c(3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 21)]
PredictDf2 = PredictDf2[PredictDf2$Orgunit.name == "Kisumu County" , c(seq(from = 1, to = ncol(PredictDf2), by = 1))]

# Test and train data
PredictDf2Train = PredictDf2[, c(seq(1, 10, 1), 14)]
PredictDf2Test = PredictDf2[, c(1, seq(11,14,1) )]

PredictDf2Train = melt(PredictDf2Train,
                       id.vars = c("Orgunit.name"))
PredictDf2Test = melt(PredictDf2Test,
                       id.vars = c("Orgunit.name"))
PredictDf2Train <- PredictDf2Train %>% tidyr::fill(value, .direction = 'up')
PredictDf2Train <- PredictDf2Train %>% tidyr::fill(value, .direction = 'down')
PredictDf2Test <- PredictDf2Test %>% tidyr::fill(value, .direction = 'up')
PredictDf2Test <- PredictDf2Test %>% tidyr::fill(value, .direction = 'down')

# Time Series
PredictDf2Train_ts = ts(PredictDf2Train[,3], start = c(2022, 1), end = c(2022, 9), frequency = 12)

mape <- function(actual, pred){
  mape <- mean(abs((actual - pred) / actual)) * 100
  return(mape)
}

naive_mod = naive(PredictDf2Train_ts, h = 12)
ForecastDf = as.data.frame(naive_mod)
ForecastDfNew = as.data.frame(t(ForecastDf))
ForecastDfNew2 = ForecastDfNew[3, 4:6]
PredictDf2New = ts(PredictDf2[,-c(1, 14)], start = c(2022, 1), end = c(2022, 12), frequency = 12)
PredictDf2New = PredictDf2New[1, ]
PredictDf2New2 = as.data.frame(t(PredictDf2New))
Combined = cbind(PredictDf2New2, ForecastDfNew2)
Combined2 = as.data.frame(t(Combined))
Combined3 = melt(Combined2)
Combined3$variable = names(Combined)

# Filling NAs
Combined3 <- Combined3 %>% tidyr::fill(value, .direction = 'up')
Combined3 <- Combined3 %>% tidyr::fill(value, .direction = 'down')

# Plotting
ggplot(Combined3, aes(x = factor(variable, levels = names(Combined)), y = value, group = 1))+
  geom_line()+
  geom_point()+
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Graph of Deaths in Nakuru County With Time",
       x = "Months",
       y = "Deaths")
