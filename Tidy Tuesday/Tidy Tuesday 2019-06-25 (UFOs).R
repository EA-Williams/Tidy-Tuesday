library(tidyverse)

# load data
ufo_sightings <- read_csv("https://bit.ly/3bQTJAV")

# inspect first six rows
head(ufo_sightings)

# plot 1: frequency of sightings in each month

ggplot(ufo_sightings, aes(x=date_time)) + 
  geom_histogram(stat="count")

ggplot(ufo_sightings, aes(x=ufo_shape)) + geom_histogram()



# plot 2: average encounter duration by ufo shape

duration_by_shape <- ufo_sightings %>%
  group_by(ufo_shape) %>%
  summarise(mean_duration = mean(encounter_length), n = n()) %>%
  arrange(desc(mean_duration)) %>%
  top_n(5,mean_duration) %>%
  mutate(mean_hours = mean_duration / 3600)

ggplot(duration_by_shape, aes(x = reorder(ufo_shape, -mean_hours), y = mean_hours)) +
  geom_col() + ggtitle("Mean Duration of UFO Encounters by UFO Shape") +
  geom_text(aes(label = paste("n =", prettyNum(n,big.mark=",")), vjust=-0.25), size = 3) +
  theme_minimal()


