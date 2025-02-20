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

colnames(sample_taxon_long)

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

# Homework
taxon_clean_goodSep_long <- taxon_clean_goodSep %>%
  pivot_longer(cols = Proteobacteria:Cyanobacteria, 
               names_to = "phylum", 
               values_to = "abundance")

# Check the structure of the new long-format data
glimpse(taxon_clean_goodSep_long)
colnames(sample_taxon_long)

sample_taxon_long <- taxon_clean_goodSep_long %>%
  inner_join(taxon_clean_goodSep, by = "sample_id")

sample_taxon_long <- sample_taxon_long %>%
  inner_join(sample_data, by = "sample_id")

glimpse(sample_taxon_long)
colnames(sample_taxon_long)


filtered_data <- sample_taxon_long %>%
  filter(phylum %in% c("Chloroflexi", "Cyanobacteria", "Bacteroidota"))


# Create the faceted boxplot
ggplot(filtered_data, aes(x = env_group, y = abundance, fill = env_group, color = env_group)) +
  geom_boxplot(alpha = 0.5) +  
  geom_jitter(width = 0.2, size = 2) + 
  facet_grid(. ~ phylum) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Phylum Abundance Changes by Depth and Season",
    x = "Depth and Season",
    y = "Phylum Abundance"
  )

#ggsave("phylum_abundance_boxplot.png", width = 8, height = 4, dpi = 300)

# Load necessary libraries
library(tidyverse)
if (!requireNamespace("ggpubr", quietly = TRUE)) install.packages("ggpubr")
library(ggpubr)  # For statistical annotation

# Perform Two-Way ANOVA
anova_result <- aov(abundance ~ env_group * phylum, data = filtered_data)
anova_summary <- summary(anova_result)

# Convert ANOVA summary to dataframe for easier extraction
anova_df <- as.data.frame(anova_summary[[1]])

# Extract p-values and ensure proper formatting
p_env_group <- formatC(anova_df$`Pr(>F)`[1], format = "e", digits = 2)  # p-value for env_group
p_phylum <- formatC(anova_df$`Pr(>F)`[2], format = "e", digits = 2)  # p-value for phylum
p_interaction <- formatC(anova_df$`Pr(>F)`[3], format = "e", digits = 2)  # p-value for interaction

# Create the faceted boxplot with ANOVA results and store it in `p`
p <- ggplot(filtered_data, aes(x = env_group, y = abundance, fill = env_group, color = env_group)) +
  geom_boxplot(alpha = 0.5) +  
  geom_jitter(width = 0.2, size = 2) + 
  facet_grid(. ~ phylum) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Phylum Abundance Changes by Depth and Season\nTwo-Way ANOVA Results",
    x = "Depth and Season",
    y = "Phylum Abundance"
  ) +
  annotate("text", x = 1.5, y = max(filtered_data$abundance), 
           label = paste0("p (env_group) = ", p_env_group), size = 5, color = "black") +
  annotate("text", x = 2, y = max(filtered_data$abundance) * 0.9, 
           label = paste0("p (phylum) = ", p_phylum), size = 5, color = "black") +
  annotate("text", x = 2.5, y = max(filtered_data$abundance) * 0.8, 
           label = paste0("p (interaction) = ", p_interaction), size = 5, color = "black")

# Explicitly print the plot (this enables zooming in RStudio)
print(p)
# Save as a high-quality PNG file
ggsave("phylum_abundance_ANOVA.png", plot = p, width = 10, height = 6, dpi = 300)


