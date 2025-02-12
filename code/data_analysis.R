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
  #why two eauql signs? wee will subset basen on a logical, true == 1
  filter(temperature < 5) %>%
  #calculate the mean cell abundance
  summarize(avg_cells = mean(cells_per_ml))

#Mutate: Create a new column
sample_data %>%
  #calc new column with the TN:TN ration
  mutate(tn_tp_ration = total_nitrogen / total_phosphorus) %>%
  #visualize it
  view()

#select(): subset by entire column
sample_data%>%
  #pick specific columns
  select(-c(diss_org_carbon,))

#Clean up data
taxon_dirty <- read_csv("data/taxon_abundance.csv", skip = 2)

#only pick to the cyanobacteria
taxon_clean <- taxon_dirty %>%
  select(sample_id: Cyanobacteria) %>%
#what are the wide foemat dimension? 71 rows by 7 column
  taxon_long <- taxon_clean %>%
  pivot_longer(cols = Proteobacteria:Cyanobacteria, 
               names_to = "Phylum",
               values_to = "Abundance")


taxon_long


#Calculate avg abundanve of each phylum
taxon_long %>%
  group_by(Phylum) %>%
  summarize(avg_abund = mean(Abundance))
              
#Plot our data
taxon_long %>%
  ggplot(aes(x = sample_id, y = Abundance, fill = Phylum)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90))

#joining data frames
sample_data %>%

#inner join
sample_data %>%
  inner_join(., taxon_clean, by = "sample_id")

#intuition check on filtering joins
length(unique(taxon_clean$sample_id))
length(unique(sample_data$sample_id))

#anti-join: which rows are not joining?
sample_data%>%
  anti_join(., taxon_clean, by = "sample_id")

#sixing september samples
taxon_clean %>%
  mutate(sample_id = str_replace(sample_id,
                                 pattern = "Sep",
                                 replacement = "September")

         
#test
stopifnot(nrow(sample_and_taxon) == nrow(sample_data))


#write out our clean data into a new file
write_csv(sample_and_taxon, "data/sample_and_taxon.csv")

#Quick Plot of Chloroflexi
sample_and_Taxon %>%
  ggplot(aes(x = depth, y = Chloroflwxi)) +
  geom_point() +
  #add a statistical model
  geom_smooth()