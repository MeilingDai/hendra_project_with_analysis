.libPaths(c("C:/Users/DAI035/data_school/Packages"))

library(tidyverse) 


sessionInfo()



#read_csv("data/field_horses_metadata_modified.csv")
field_horses_metadata_modified <- read_csv("data/field_horses_metadata_modified.csv")
field_horses_metadata_modified 

#read csv data ("data/field_horses_raw_counts_modified.csv")
field_horses_raw_counts_modified <- read_csv("data/field_horses_raw_counts_modified.csv")
field_horses_raw_counts_modified 


tidy_field_horses_raw_counts <-  field_horses_raw_counts_modified %>%  
  gather(key = horse_id, value = counts, -gene) 
tidy_field_horses_raw_counts

full_data <- full_join(tidy_field_horses_raw_counts, field_horses_metadata_modified)

full_data 


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
