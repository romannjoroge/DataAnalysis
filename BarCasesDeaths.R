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

# Import Data
BarDf = read.csv(file = "KHIS TB 2021.csv",
                 header = TRUE);
# Cleaning Data
BarDf2 = BarDf[BarDf$Data == "TB cases detected"
               | BarDf$Data == "TB deaths", c(3, 21, 4)]
# Removes the word County from the county names
for (i in 1:nrow(BarDf2)) {
  county = substr(strsplit(BarDf2$Orgunit.name[i], " ")[[1]][1], 1, 3);
  BarDf2$Orgunit.name[i] = county;
}


ggplot(data = BarDf2,
       aes(x = Orgunit.name, y = Total.weight, fill = Data)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "A Comparison Between Cases Detected and Deaths For Tuberculosis Per County",
       fill = "Legend",
       x = "Counties",
       y = "Number")

