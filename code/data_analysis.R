#data analysis

#load packages
library(tidyverse)

#grab the data for our analysis
sample_data <-read_csv("data/sample_data.csv")
glimpse(sample_data)

#sumarize
summarize(sample_data, avg_cells= mean(cells_per_ml))

#syntax/style
sample_data %>%
  #group the data by the envGroup
  group_by(env_group) %>%
  summarize(avg_cells = mean(cells_per_ml))

#Filter: Subset data by rows based on some value
sample_data %>%
  #subset sample only from the deep
  filter
  #calculate the mean cell abundance
  