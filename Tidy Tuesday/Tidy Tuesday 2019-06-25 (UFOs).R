# The core of this was written during the first virtual meeting of the R Users Manchester group,
# 18/03/2020, run by Andrew Stewart: https://github.com/ajstewartlang/RUM_TidyTuesday_2020

library(tidyverse)
library(viridis)  # for colour palette (colourblind and greyscale friendly)

# load data
ufo_sightings <- read_csv("https://bit.ly/3bQTJAV")

# inspect first six rows
head(ufo_sightings)

# plot 1: frequency of sightings in each month (not finished)

ggplot(ufo_sightings, aes(x=date_time)) + 
  geom_histogram(stat="count")

ggplot(ufo_sightings, aes(x=ufo_shape)) + geom_histogram()



# plot 2: average encounter duration by ufo shape

duration_by_shape <- ufo_sightings %>%
  filter(ufo_shape != "other", ufo_shape != "unknown") %>%
  group_by(ufo_shape) %>%
  summarise(mean_duration = mean(encounter_length),
            n = n()) %>%
  arrange(desc(mean_duration)) %>%
  top_n(5,mean_duration) %>%
  mutate(mean_hours = mean_duration / 3600, ufo_shape = str_to_title(ufo_shape))

ggplot(duration_by_shape, aes(x = reorder(ufo_shape, -mean_hours), y = mean_hours,
                              fill = reorder(ufo_shape, -mean_hours))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste("n =", prettyNum(n,big.mark=",")), vjust=-0.25), size = 2.5) +
  theme_minimal() + scale_fill_viridis(discrete = TRUE) +
  labs(title="Duration of UFO Encounters by UFO Shape",
       x ="Shape of UFO", y = "Mean Duration (hours)")

table(ufo_sightings$ufo_shape)

