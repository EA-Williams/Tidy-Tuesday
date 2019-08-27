library(tidyverse)

# Load the data
simpsons <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv")

# Filter by Fat Tony appearances
simpsons_tony <- filter(simpsons, role == "Fat Tony")

# Separate the 'roles' column into separate columns for each sole
simpsons_roles <- separate(simpsons, role, into = c("role 1", "role 2", "role 3", "role 4", "role 5"), sep = ";")

# Filter by 'self' guest appearances
simpsons_self <- filter(simpsons, str_detect(role, "self"))

# Want to plot the number of guest stars playing themselves per season...
ggplot(data = simpsons_self) +
  geom_bar(mapping = aes(x = cut))
