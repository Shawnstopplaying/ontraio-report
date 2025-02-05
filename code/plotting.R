# Plotting Lake Ontario Microbial Cell Abundances
#By: Shawn
#Date: January 29th, 2025

#first install the packages
install.packages('tidyverse')
library(tidyverse)

#load in the data
sample_data <- read_csv('sample_data.csv')

Sys.Date() # What is the date?
getwd() # Where  am I?

#what does the data look like
View(sample_data)
str(sample_data)

#Plotting
ggplot(data = sample_data) + 
  aes(x = total_phosphorus, y = cells_per_ml/1000000, color = env_group, size = temperature) +
  labs(y = "Cell Abundance (millions/ml)", x = "Total Phosphorus (µg/L)" , title = "Does total phosphorus affect microbial abundances", color = "Environmental Group", size = "Temperature(℃)") +
  geom_point() + 
  geom_smooth(aes(color = env_group), method = "lm", se = FALSE, size = 1)

SSS#Buoy data
buoy_data <- read_csv(file = 'buoy_data.csv')
dim(buoy_data)
glimpse(buoy_data)
unique(buoy_data$sensor)

ggplot(data = buoy_data) +
  aes(x = day_of_year, y = temperature, group = sensor, color = depth)+
  geom_line()

ggplot(data = buoy_data) +
  aes(x = day_of_year, y = temperature, group = sensor, color = depth)+
  geom_line()+
  facet_grid(rows = vars(buoy))
?facet_wrap

#Cell abundance by environental group
ggplot(data = sample_data)+
  aes(x = env_group, y = cells_per_ml, color = env_group, fill = env_group)+
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(size = chlorophyll))+
  theme_bw()

?ggsave
ggsave("cells_per_ecvGroup.png", width = 6, height = 4)
