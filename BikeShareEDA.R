library(tidyverse)
library(tidymodels)
library(vroom)
library(DataExplorer)
library(patchwork)

#read in data and EDA
data <- vroom("train.csv")
dplyr::glimpse(data) 
DataExplorer::plot_intro(data) #visualize glimpse, 
DataExplorer::plot_correlation(data) #correlation heatmap, 
plot_2 <- DataExplorer::plot_bar(data) #barcharts of discrete, 
plot_3 <- DataExplorer::plot_histogram(data) #histogram of numerical, 
DataExplorer::plot_missing(data) #missing data


#bar plot of weather
plot_bar <- ggplot(data, aes(x=weather)) +
  geom_bar() +
  labs(x = "Weather Type", y = "Number of Users")

#Scatterplot of Temperature with count
plot_temp <- ggplot(data, aes(x= temp, y = count)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Temperature", y = "Total Number of Users")

#Scatterplot of temperature and atemperature to illustrate confounding issues
plot_confound <- ggplot(data, aes(y = atemp, x = temp)) +
  geom_point() +
  geom_smooth() +
  labs(y = "Feels Like Temperature", x = "Temperature")

#A plot of how working days affect who is using the bikes
plot_instinct <- ggplot(data, aes(x = registered, y = casual, color = factor(workingday, labels = c("Holiday", "Working Day")))) +
  geom_point() +
  scale_color_manual(values = c("Holiday" = "Green", "Working Day" = "Blue")) +
  labs(x = "Registered Users", y = "Casual Users", color = "Day Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot all four
(plot_bar + plot_temp) / (plot_confound + plot_instinct)
