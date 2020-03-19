library(tidyverse)
library(lubridate)
library(viridis)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

raw_df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/raw_anime.csv")


anime <- tidy_anime %>%
  select(-title_english, -title_japanese, -title_synonyms, -synopsis, -background, -related,
         -producers, -studio) %>%
  filter(type == "TV") %>%
  group_by(animeID) %>%
  mutate(genre = paste0(genre, collapse = ", ")) %>%
  distinct()

  table(anime$source)

  # question 1: how does broadcast day / time affect the score of a series?
  
  
  # question 2: how does the source of an anime  affect the score?
  # source : original / manga(4-koma manga, Digital manga, Manga, Web manga) / novel (Light novel, 
  # Novel, Visual novel) /  Other
  
  # question 3: what's the disparity between my rankings of 10 anime and their rankings on MAL?
  
  
  # Q1: how does broadcast day / time affect the score of a series?
  
  broadcasting <- raw_df %>%
    select(animeID, name, score, broadcast) %>%
    mutate(bc_day = str_extract(broadcast, '[A-Za-z]+'),
           bc_day = substr(bc_day, 1, nchar(bc_day)-1),
           bc_time = str_extract(broadcast, '[:digit:][:digit:][:punct:][:digit:][:digit:]'),
           bc_hour = hour(strptime(bc_time, format = "%H:%M"))) %>%
    filter(!is.na(bc_day), !is.na(bc_hour))
    
  ggplot(broadcasting, aes(x = bc_day, y = bc_hour, fill = score)) + 
    geom_tile(colour="black", size=0.5, stat="identity") + 
    scale_fill_viridis(name = "Anime Score") #+
   # scale_x_discrete(breaks=1:7, labels= c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"), position= "top", expand = c(0,0))# +
    #scale_y_discrete(breaks = seq(1960, 2010, by = 10)) +
    labs(title="UFO Sightings since 1960 by Month and Year",
         x ="Month", y = "Year") + tile_theme
  day("Monday")
  
  df$wkdaynum <- format("Monday","%u")  
  
      
      # Q3: 
      # 1.	Dragon Ball Z
      # 2.	K-On!
      # 3.	Boku No Hero Academia
      # 4.	Bakuman
      # 5.	Ouran Koukou Host Club
      # 6.	Fullmetal Alchemist: Brotherhood
      # 7.	One Piece
      # 8.	Tiger and Bunny
      # 9.	One Punch Man
      # 10.	New Game!
        
  