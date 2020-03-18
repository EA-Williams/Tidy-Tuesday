library(tidyverse)
# library(purrrlyr)
# library(extrafont)
# library(extrafontdb)
#
# devtools::install_github("Ryo-N7/tvthemes")
# library(tvthemes)
#
# import_simpsons()
# loadfonts()

# Load the data
simpsons <-
  readr::read_delim(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv",
    delim = "|",
    quote = ""
  )

# Add Episode column
simpsons <- simpsons %>%
  mutate(episode_number = str_sub(number, -2)) %>%
  select(1, episode_number, everything()) # move it to the second column

# Separate the 'roles' column into separate columns for each role
simpsons_roles <-
  separate(
    simpsons,
    role,
    into = c("role_1", "role_2", "role_3", "role_4", "role_5"),
    sep = ";"
  ) #There will be a warning here

# New column for 'roles this episode'
simpsons_roles <- simpsons_roles %>%
  mutate(roles_this_episode =
  select(simpsons_roles, starts_with("role_")) %>%
  is.na %>%
  `!` %>%
  rowSums())

# Filter by multiple roles
simpsons_multi_roles <- filter(simpsons_roles, roles_this_episode > 1)

# Number of unique roles by each person

main_matches %>% filter(loser == "Andy Murray") %>% 
  group_by(winner) %>% arrange(year) %>%
  summarise(years  = paste(year, collapse =","), times = length(year))  %>%
  arrange(desc(times), years)

simpsons2 <- simpsons %>% 
  group_by(guest_star) %>% arrange(role) %>%
  summarise(roles = paste(role, collapse = ";", total_roles = length(role))) %>%
  arrange(desc(total_roles), roles)

simpsons3 <- simpsons %>%
  group_by(guest_star) %>%
  summarise(roles = paste(role, collapse = ";"))

simpsons4 <- simpsons_roles %>%
  group_by(guest_star) %>%
  summarise_at(c("role_1", "role_2", "role_3", "role_4", "role_5"), n())


##THIS PART DOESN'T WORK YET
# Filter by 'self' guest appearances
simpsons_self <- filter(simpsons, str_detect(role, "self"))

# Want to plot the number of guest stars playing themselves per season...
ggplot(data = simpsons_self) +
  geom_bar(mapping = aes(x = as.integer(season))) +
  labs(y = "Guest Stars Appearing as Themselves", x = "Season")
#+theme_simpsons()

# # Want to plot the number of guest stars playing themselves per season...
# ggplot(data = simpsons_self) +
#   geom_bar(mapping = aes(x = as.integer(season), fill = str_sub(number,-2))) +
#   labs(y = "Guest Stars Appearing as themselves", x = "Season")
# #+theme_simpsons()

#




#from Tony Galvan, https://github.com/gdatascience/tidytuesday/blob/master/simpsons.Rmd

roles <- simpsons %>%
  unnest(role = strsplit(role, ";")) %>%
  mutate(role_name = if_else(role %in% c("Himself", "Herself", "Themselves"), guest_star, role))
glimpse(roles)



#Adapted from https://ryo-n7.github.io/2019-05-16-introducing-tvthemes-package/
theme_simpsons(title.font = "Akbar",
               text.font = "Akbar",
               axis.text.size = 8)
