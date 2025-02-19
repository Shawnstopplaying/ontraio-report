#data analysis

#load packages
library(tidyverse)

#grab the data for our analysis
sample_data <-read_csv("data/sample_data.csv")
glimpse(sample_data)

#summarize
summarize(sample_data, avg_cells=mean(cells_per_ml))

#syntax/style
sample_data %>%
  #group data by environmental group
  group_by(env_group)%>%
  #calculate mean
  summarize(avg_cells=mean(cells_per_ml))

#filter: subset data by rows based on some value
sample_data %>%
  #subset samples only from the deep
  #subset based on a logical, TRUE==1
  filter(env_group =="Deep") %>%
  #calculate mean cell abundances
  summarize(avg_cells = mean(cells_per_ml))

sample_data %>%
  #subset samples only from the deep
  #subset based on a logical, TRUE==1
  filter(temperature<5) %>%
  #calculate mean cell abundances
  summarize(avg_cells = mean(cells_per_ml))

#mutate: create new column 
sample_data %>%
  #calculate new column with the TN:TP ratio
  mutate(tn_tp_ratio=total_nitrogen/total_phosphorus) %>%
  #visualize it
  view()

#select():subset by entire columns
sample_data %>%
  #pick specific columns
  select(sample_id, depth)

#select():subset by entire columns
sample_data %>%
  #pick specific columns in between sample id and temperature
  select(sample_id:depth)

#select():subset by entire columns
sample_data %>%
  #pick all columns except for
  select(-diss_org_carbon)

#select():subset by entire columns
sample_data %>%
  #pick all columns except for
  select(-c(diss_org_carbon,chlorophyll))

#clean up data
taxon_dirty<-read_csv("data/taxon_abundance.csv", skip =2)
head(taxon_dirty)

#only pick cyanobacteria
taxon_clean<-
  taxon_dirty %>%
  select(sample_id: Cyanobacteria)
#what are the wide format dimensions?
dim(taxon_clean)

#shape the data from wide into long format
taxon_long<-
  taxon_clean%>%
  #shap into long-formatted data frame
  pivot_longer(cols=Proteobacteria:Cyanobacteria,
               names_to="phylum",
               values_to="abundance")
#check the new dimensions:42
dim(taxon_long)

#calculate abundance by each phylum
taxon_long%>%
  group_by(phylum)%>%
  summarise(avg_abund=mean(abundance))

#plot our data
taxon_long%>%
  ggplot(aes(x=sample_id, y=abundance, fill=phylum))+
  geom_col()+
  theme(axis.text.x=element_text(angle=90))

#joining data frames
sample_data %>%
  head(6)

taxon_clean%>%
  head(6)

#inner join
sample_data%>%
  #. represents sample data
  inner_join(.,taxon_clean, by="sample_id")%>%
  #this gives 32 instead of 71, only a subset, why?
  dim()

#intuition check on filtering joins
length(unique(taxon_clean$sample_id))
length(unique(sample_data$sample_id))

#anti-join: which rows are not joining
sample_data%>%
  anti_join(., taxon_clean, by ="sample_id")

#fixing september samples
taxon_clean_goodSep<-
  taxon_clean%>%
  #replace sample_id column with fixed september names
  mutate(sample_id= str_replace(sample_id, pattern="Sep", replacement = "September"))

#check dimensions
dim(taxon_clean_goodSep)

#inner join
sample_and_taxon<-
  sample_data%>%
  inner_join(.,taxon_clean_goodSep, by="sample_id")
dim(sample_and_taxon)

#test
stopifnot(nrow(sample_and_taxon)== nrow(sample_data))

#write out clean data into a new file
write_csv(sample_and_taxon, "data/sample_and_taxon.csv")

#quickplot of chloroflexi
sample_and_taxon%>%
  ggplot(aes(x=depth, y=Chloroflexi))+
  geom_point()+
  #add a statostical model
  geom_smooth()
