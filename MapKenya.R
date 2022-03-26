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



# Entering data from KHIS TB 2021.csv to TBDf
TBDf = read.csv(file = "KHIS TB 2021.csv",
                header = TRUE);

# Cleaning the data

# Selects all the rows that have TB cases Detected
TBDf = TBDf[TBDf$Data == "TB deaths", ]
# Removes the word County from the county names
for (i in 1:nrow(TBDf)) {
  county = strsplit(TBDf$Orgunit.name[i], " ")[[1]][1];
  TBDf$Orgunit.name[i] = county;
}
# Removes the unneeded columns
TBDf = TBDf[, c(3, 21)];
# Adding an id column to TBDf which is needed when merging it and mapdata
TBDf$id = 1:nrow(TBDf)

# Plotting the heat map

# Gets the map data
Kenya = getData("GADM", country = "KEN", level = "1")

from_list = c()
for (i in 1:(nrow(TBDf)-1)) {
  id_num = Kenya@polygons[[i]]@ID
  from_list = append(from_list, id_num)
}

to_list = seq(1:(nrow(TBDf)-1))
map = setNames(to_list, from_list)


# Formatting Kenya to long frame data format
Kenya = broom::tidy(Kenya);
Kenya$id = map[Kenya$id]
# Merge Kenya and TBDf
Kenya = left_join(Kenya, TBDf, by = "id")
# Plotting the map
ggplot(Kenya, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Total.weight), color = "black") +
  scale_fill_gradient(name = "#TB Deaths", low = "yellow",
                      high = "red", na.value = "grey50") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Concentration of Deaths by Tuberculosis in Kenya",
       fill = "class",
       x = NULL,
       y = NULL)


