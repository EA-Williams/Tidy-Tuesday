# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-27
library(tidyverse)

# load dataset
simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

# general tidying:

# create the sdata dataframe
sdata <- simpsons %>%
  # filter out the movie
  filter(season != "Movie") %>%
  # create a column for the episode number, and convert this and season to integers
  mutate(episode = as.integer(str_sub(number, -2)),
         season = as.integer(season)) %>%
  # rearrange the columns and get rid of the production code
  select(season, episode, episode_title, guest_star, role) %>%
  # separate out the roles into different rows
  separate_rows(role, sep = "; ")

## my question: what is the gender distrubution of celebrity cameos and revisits in The Simpsons?

# create a dataframe called cameos for celebrity cameos
cameos <- sdata %>%
  # filter to people playing themselves
  filter(str_detect(role, "Herself|Himself|Themselves")) %>%
  # create a new column for gender of cameos - female, male, and group
  mutate(gender = case_when(str_detect(role, "Herself") ~ "Female",
                            str_detect(role, "Himself") ~ "Male",
                            str_detect(role, "Themselves") ~ "Group")) %>%
  # create a new column for whether this is someone's first cameo or a revisit
  mutate(unique = case_when(duplicated(guest_star) ~ "Revisit",
                   TRUE ~ "First"))
  
ggplot(data = cameos, aes(x = gender, fill = unique)) + geom_bar()

ggplot(data = cameos_summary, aes(x = gender, y = n)) + geom_col()

cameos_summary <- cameos %>%
  filter(gender != "Group") %>%
  group_by(gender, unique) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  add_row(gender = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Male"),
          unique = c("First", "Revisit", "First", "Revisit", "First", "Revisit", "First", "Revisit"),
          n = c(58, 0 , 306, 0, 0, 0, 0, 0)) %>%
  add_column(slide = c(3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1))

# learning gganimate from https://www.r-graph-gallery.com/288-animated-barplot-transition.html

# Make a ggplot, but add frame=year: one image per year
ggplot(cameos_summary, aes(x=gender, y = n, fill = reorder(unique, -unique))) + 
  geom_col() +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    slide,
    transition_length = 2,
    state_length = 1) + 
  ease_aes('sine-in-out')
# Save at gif:
anim_save("288-animated-barplot-transition.gif")