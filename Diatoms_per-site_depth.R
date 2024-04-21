setwd("/Users/anton/Downloads/Diatoms")
df <- read.table(file = "diatom_mapped_id80_sorted_depth.txt",
                 header = F,
                 col.names = c("plastome","position","depth"))
head(df)
plastome <- "NC_077461.1"
depth_plastome <- df[df$plastome == plastome,]
hist(depth_plastome$depth)
median(depth_plastome$depth)
unique(df$plastome)


medians <- vector()
averages <- vector()
sd <- vector()
plastomes <- unique(df$plastome)

for(i in 1:63) {
  depth_plastome <- df[df$plastome == plastomes[i],]
  medians[i] <- median(depth_plastome$depth)}
medians


for(i in 1:63) {
  depth_plastome <- df[df$plastome == plastomes[i],]
  averages[i] <- mean(depth_plastome$depth)}
averages

for(i in 1:63) {
  depth_plastome <- df[df$plastome == plastomes[i],]
  sd[i] <- sd(depth_plastome$depth)}
sd
tabel <- cbind(plastomes,medians, averages, sd)
tabel


str(tabel)
str(df)
tabel

library(dplyr)
library(ggplot2)

# Assuming your data frame is named df
# Convert plastome to a factor to ensure proper grouping
df$plastome <- factor(df$plastome)

# Filter and mutate data for the desired plastomes
df_filtered <- df %>%
  mutate(plastome = case_when(
    plastome == "NC_016731.1" ~ "Synedra acus",
    plastome == 'NC_008589.1' ~ "Thalassiosira pseudonana",
    plastome == 'NC_077461.1' ~ 'Cyclotella cryptica'
    # Add more conditions if needed for other plastomes
  )) %>%
  filter(plastome %in% c('Skeletonema costatum', 'Synedra acus', 'Thalassiosira pseudonana','Cyclotella cryptica' ))

# Calculate median and mean
summary_stats <- df_filtered %>%
  group_by(plastome) %>%
  summarise(median_depth = median(depth), mean_depth = mean(depth))

# Merge summary stats with the original dataframe
df_filtered <- left_join(df_filtered, summary_stats, by = "plastome")

# Create separate plots for each plastome with smaller line size and x-axis labels without scientific notation
p <- ggplot(df_filtered, aes(x = position, y = depth, group = plastome, color = plastome)) +
  geom_line(size = 0.3) +
  geom_hline(aes(yintercept = mean_depth), color = "black", linetype = "dashed") +
  geom_hline(aes(yintercept = median_depth), color = "black", linetype = "dotted") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +  # Remove scientific notation
  labs(x = "Position", y = "Per-site read depth of plastome") +  # Updated y-axis label
  theme_minimal() +
  facet_wrap(~ plastome, scales = "fixed", ncol = 1, strip.position = "bottom") +  # Place strip labels at the bottom
  theme(strip.text = element_text(size = 10, face = "bold")) +  # Adjust strip label appearance
  scale_color_manual(values = c("Skeletonema costatum" = "royalblue", "Synedra acus" = "indianred", "Thalassiosira pseudonana" = "forestgreen", "Cyclotella cryptica" = "cornflowerblue"))  # Set line colors

# Print the plot
print(p)







df_filtered_1 <- df %>%
  mutate(plastome = case_when(
    plastome == 'NC_077461.1' ~ 'Cyclotella cryptica'
    # Add more conditions if needed for other plastomes
  )) %>%
  filter(plastome %in% c('Cyclotella cryptica' ))

# Calculate median and mean
summary_stats_1 <- df_filtered_1 %>%
  group_by(plastome) %>%
  summarise(median_depth = median(depth), mean_depth = mean(depth))

# Merge summary stats with the original dataframe
df_filtered_1 <- left_join(df_filtered, summary_stats, by = "plastome")

# Create separate plots for each plastome with smaller line size and x-axis labels without scientific notation
p <- ggplot(df_filtered_1, aes(x = position, y = depth, group = plastome, color = plastome)) +
  geom_line(size = 0.3) +
  geom_hline(aes(yintercept = mean_depth), color = "black", linetype = "dashed") +
  geom_hline(aes(yintercept = median_depth), color = "black", linetype = "dotted") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +  # Remove scientific notation
  labs(x = "Position", y = "Per-site read depth of plastome") +  # Updated y-axis label
  theme_minimal() +
  facet_wrap(~ plastome, scales = "fixed", ncol = 1, strip.position = "bottom") +  # Place strip labels at the bottom
  theme(strip.text = element_text(size = 10, face = "bold")) +  # Adjust strip label appearance
  scale_color_manual(values = c("Cyclotella cryptica" = "cornflowerblue"))  # Set line colors

# Print the plot
print(p)
