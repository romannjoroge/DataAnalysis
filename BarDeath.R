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

# Read Data
BarDeathsDf = read.csv(file = "KHIS TB 2021.csv",
                    header = TRUE)
BarDeathsDf2 = BarDeathsDf[BarDeathsDf$Data == "TB deaths", c(3, 21)]

# Rename column name
names(BarDeathsDf2)[names(BarDeathsDf2)=="Orgunit.name"] <- "County" 
names(BarDeathsDf2)[names(BarDeathsDf2)=="Total.weight"] <- "Deaths" 
# Removes the word County from the county names
for (i in 1:nrow(BarDeathsDf2)) {
  county = strsplit(BarDeathsDf2$County[i], " ")[[1]][1];
  BarDeathsDf2$County[i] = county;
}
BarDeathsDf2$County[17] = "Tharaka Nithi"

#Shortens County Names
for (j in 1:nrow(BarDeathsDf2)) {
  county = substr(BarDeathsDf2$County[j], 1, 3)
  BarDeathsDf2$County[j] = county;
}
# Plot
ggplot(data = BarDeathsDf2,
       aes(x = County, y = Deaths)) +
  geom_bar(stat = "identity") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "A Bar Graph Comparing Death to Counties in Kenya",
       x = "Counties",
       y = "Number of Deaths")
