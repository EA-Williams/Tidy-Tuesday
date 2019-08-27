library(tidyverse)

simpsons <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv")

simpsons_tony <- filter(simpsons, role == "Fat Tony")

simpsons_roles <- separate(simpsons, role, into = c("role 1", "role 2", "role 3", "role 4", "role 5"), sep = ";")

simpsons_self <- filter(simpsons, str_detect(role, "self"))

ggplot(data = simpsons_self)
