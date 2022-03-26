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
DeathsDf = read.csv(file = "KHIS TB 2021.csv",
                    header = TRUE)
DeathsDf2 = DeathsDf[DeathsDf$Data == "TB deaths", c(3, 21)]
# Rename column name
names(DeathsDf2)[names(DeathsDf2)=="Orgunit.name"] <- "County" 
names(DeathsDf2)[names(DeathsDf2)=="Total.weight"] <- "Total Deaths" 
# Removes the word County from the county names
for (i in 1:nrow(DeathsDf2)) {
  county = strsplit(DeathsDf2$County[i], " ")[[1]][1];
  DeathsDf2$County[i] = county;
}
DeathsDf2$County[17] = "Tharaka Nithi"

FacilitiesDf = read.csv(file = "County Health Facilities.csv",
                        header = TRUE)
FacilitiesDf = FacilitiesDf[, c(1, 6)]
# Rename column name
names(FacilitiesDf)[names(FacilitiesDf)=="X2015"] <- "Number of facilities" 

# Combine Deaths and Facilities
FacilitiesDeathDf = left_join(DeathsDf2, FacilitiesDf, by = "County")
FacilitiesDeathDf = melt(FacilitiesDeathDf,
                       id.vars = c("County"))
#Shortens County Names
for (j in 1:nrow(FacilitiesDeathDf)) {
  county = substr(FacilitiesDeathDf$County[j], 1, 3)
  FacilitiesDeathDf$County[j] = county;
}

# Plot
ggplot(data = FacilitiesDeathDf,
       aes(x = County, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "A Comparison Between County Facilities and Deaths For Tuberculosis Per County",
       fill = "Legend",
       x = "Counties",
       y = "Number of Deaths / Facilities")
