# The core of this was written during the first virtual meeting of the R Users Manchester group,
# 18/03/2020, run by Andrew Stewart: https://github.com/ajstewartlang/RUM_TidyTuesday_2020

library(tidyverse)
library(viridis)  # for colour palette (colourblind and greyscale friendly)
library(lubridate)

# load data
ufo_sightings <- read_csv("https://bit.ly/3bQTJAV")

# inspect first six rows
head(ufo_sightings)

# plot 1: average encounter duration by ufo shape

# create the dataframe
duration_by_shape <- ufo_sightings %>%
  # remove the unknown or non-recorded shapes
  filter(ufo_shape != "other", ufo_shape != "unknown") %>%
  group_by(ufo_shape) %>%
  # find out the mean durations and number of reports of each UFO shape
  summarise(mean_duration = mean(encounter_length), n = n()) %>%
  # sort the shapes by encounter duration, largest to smallest
  arrange(desc(mean_duration)) %>%
  # keep just the top 5
  top_n(5,mean_duration) %>%
  # convert duration to hours (was seconds) and change the shapes to title case
  mutate(mean_hours = mean_duration / 3600, ufo_shape = str_to_title(ufo_shape))

# plot the shapes on the x axis (ordered by hours, descending) and hours on the y axis
ggplot(duration_by_shape, aes(x = reorder(ufo_shape, -mean_hours), y = mean_hours,
                              # fill according to UFO shape
                              fill = reorder(ufo_shape, -mean_hours))) +
  # add the columns for each shape, with no legend
  geom_col(show.legend = FALSE) +
  # add labels on top of each UFO shape column with the number of reports (sample size)
  # use prettyNum to include commas in numbers in the thousands. Align the labels above the columns.
  geom_text(aes(label = paste("n =", prettyNum(n,big.mark=",")), vjust=-0.25), size = 2.5) +
  # use a minimal theme and the viridis colour palette (the best)
  theme_minimal() + scale_fill_viridis(discrete = TRUE) +
  # label the plot and the axes
  labs(title="Duration of UFO Encounters by UFO Shape",
       x ="Shape of UFO", y = "Mean Duration (hours)")

# plot 2: frequency of sightings in each month (not finished)
# learnt how to plot frequency month vs year from Jalapic: https://gist.github.com/jalapic/9a1c069aa8cee4089c1e
library(gridExtra)

tile_theme = theme(
  panel.background = element_rect(fill="black"),
  panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks = element_blank(), 
  axis.text = element_text(color="black", size=8),
  axis.text.y  = element_text(hjust=1),
  legend.text = element_text(color="black", size=8),
  legend.position = "bottom",
  legend.title=element_text())

year_month <- ufo_sightings %>%
  mutate(month = month(mdy_hm(date_time)),
         year = year(mdy_hm(date_time))) %>%
 group_by(year, month) %>%
 summarise(n = n()) %>%
  filter(between(year, 1960, 2013))

letter_months = substring(month.abb[1:12], 1, 1)

ggplot(year_month, aes(x = month, y = reorder(year, -year), fill = n)) + 
  geom_tile(colour="black", size=0.5, stat="identity") + 
  scale_fill_viridis(name = "Number of UFO Sightings") +
  scale_x_continuous(breaks=1:12, labels= letter_months, position= "top", expand = c(0,0)) +
  scale_y_discrete(breaks = seq(1960, 2010, by = 10)) +
  labs(title="UFO Sightings since 1960 by Month and Year",
       x ="Month", y = "Year") + tile_theme

