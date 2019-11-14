.libPaths(c("C:/Users/DAI035/data_school/Packages"))

library(tidyverse) 


sessionInfo()


#read csv data ("data/field_horses_raw_counts_modified.csv")
field_horses_raw_counts_modified <- read_csv("data/field_horses_raw_counts_modified.csv")
field_horses_raw_counts_modified


#read csv data ("data/field_horses_raw_counts_modified.csv")
field_horses_metadata_modified <- read_csv("data/field_horses_metadata_modified.csv")
field_horses_metadata_modified



tidy_field_horses_raw_counts <-  field_horses_raw_counts_modified %>%  
  gather(key = horse_id, value = counts, -gene) 
tidy_field_horses_raw_counts

full_data <- full_join(tidy_field_horses_raw_counts, field_horses_metadata_modified)

full_data 
write.csv(full_data,"results/field_horses_full_data.csv")


# raw counts of microRNA of 6 horses with geom_plot
full_data_plot1 <- ggplot (data = full_data,
                           aes(x = horse_id,
                               y = counts, 
                               color = infection, 
                               group = horse_id))+
  geom_point()+
  scale_y_log10()+
  scale_color_manual(values = c(mock = "grey", infected = "red")) +
  theme_bw() +
  labs(title = "microRNA raw counts of horses together")

full_data_plot1
ggsave(filename = "results/Figure 1.png", plot = full_data_plot1, width = 10, height = 8, dpi = 300, units = "cm")



# raw counts of microRNA of ech horse with geom_boxplot and geom_jitter
full_data_plot2 <- ggplot (data = full_data,
                           aes(x = horse_id,
                               y = counts, 
                               color = infection, 
                               group = horse_id))+
  geom_boxplot() + 
  geom_jitter()+
  scale_y_log10()+
  scale_color_manual(values = c(mock = "grey", infected = "red")) +
  theme_bw() +
  labs(title = "microRNA raw counts of 6 horses")


full_data_plot2
ggsave(filename = "results/Figure 2.png", plot = full_data_plot2, width = 12, height = 8, dpi = 300, units = "cm")


# plot figure with raw count > 1000 for all horses with geom_boxplot() + geom_jitter()
raw_count_more_than_1000 <- full_data %>% 
  filter(counts > 1000)

raw_count_more_than_1000

plot3 <- ggplot(data = raw_count_more_than_1000,
                aes(x = horse_id,
                    y = counts, 
                    color = infection, 
                    group = horse_id))+
  geom_boxplot() + 
  geom_jitter()+
  scale_y_log10()+
  scale_color_manual(values = c(mock = "grey", infected = "red")) +
  theme_bw() +
  labs(title = "microRNA raw counts more than 1000 of 6 horses")

plot3
ggsave(filename = "results/Figure 3.png", plot = plot3, width = 12, height = 8, dpi = 300, units = "cm")



# plot figure with raw count < 1000 for all horses with geom_boxplot() + geom_jitter()
raw_count2 <- full_data %>% 
  filter(counts < 1000)

raw_count2

plot4 <- ggplot(data = raw_count2,
                aes(x = horse_id,
                    y = counts, 
                    color = infection, 
                    group = horse_id))+
  geom_boxplot() + 
  geom_jitter()+
  scale_y_log10()+
  scale_color_manual(values = c(mock = "grey", infected = "red")) +
  theme_bw() +
  labs(title = "microRNA raw counts < 1000 of 6 horses")

plot4
ggsave(filename = "results/Figure 4.png", plot = plot4, width = 12, height = 8, dpi = 300, units = "cm")


# Do some PCA analysis with data, but then I need to generate new data set. 
# Following RNAseq tutorial 
# Create a matrix with only counts and gene names as column names 
# read csv data ("data/field_horses_raw_counts.csv")
field_horses_raw_counts <- read_csv("data/field_horses_raw_counts.csv")
field_horses_raw_counts

data_1 <- field_horses_raw_counts %>% 
  filter(gene > 0) %>% 
  column_to_rownames("gene") 
  data_1
  
   

 # Transpose the rows and columns
data_2 <- t(data_1) 
data_2
head(data_2)



# Remove first column from data

data_without_gene <- field_horses_raw_counts[,-(1)]
data_without_gene 

# Store GeneID as rownames
rownames(data_without_gene) <- field_horses_raw_counts$gene

View(data_without_gene)
head(data_without_gene)



#PCA analysis 
field_horses_raw_counts_PCA <- field_horses_raw_counts %>% 
  column_to_rownames("gene") %>% 
   scale()

# Peform the PCA on the already scaled heat_stress data
PCA <- prcomp(field_horses_raw_counts_PCA)

# Check the summary
summary(PCA)

plot(PCA)



# plot PC1 and PC2

# check Marina's github: https://github.com/Marina0729/Rmarkdown-summary-of-hendra-microRNAs

# have stephen's R markdown template: https://github.com/sparce/DSreport.
 # creat a new repository in my github, and then copy stephen's repository to my github repository and go to RStudio, file-new file-Rmarkdown.



