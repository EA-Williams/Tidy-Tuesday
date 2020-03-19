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
  
  # function to convert "broadcast" to numbered day ('bcd' is broadcast day)
  
 bcd_to_num <- function(bcd) {
    day_num <- str_extract(bcd, '[A-Za-z]+') %>%
      recode("Mondays" = 1,
             "Tuesdays" = 2,
             "Wednesdays" = 3,
             "Thursdays" = 4,
             "Fridays" = 5,
             "Saturdays" = 6,
             "Sundays" = 7)
    return(day_num)
  }
  
  broadcasting <- raw_df %>%
    select(animeID, name, score, broadcast) %>%
    mutate(bc_day_num = bcd_to_num(broadcast),
           bc_time = str_extract(broadcast, '[:digit:][:digit:][:punct:][:digit:][:digit:]'),
           bc_hour = hour(strptime(bc_time, format = "%H:%M"))) %>%
    filter(!is.na(bc_day_num), !is.na(bc_hour))
    
  ggplot(broadcasting, aes(x = bc_day_num, y = bc_hour, fill = score)) + 
    geom_tile(colour="black", size=0.5, stat="identity") + 
    scale_fill_viridis(name = "Anime Score") +
    scale_x_continuous(breaks=1:7, labels= c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"), position= "top",
                                             expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0, 23, by = 2), expand = c(0,0)) +
    tile_theme +
    labs(title="Anime Scores by Broadcast Day and Hour",
         x ="Broadcast Day", y = "Broadcast Hour") + tile_theme
    
  # Hmm, quite few blank tiles - dark hours where no anime is aired!
  
  # Let's simplify it and look at just days of the week...
  
  # Q1b: how does broadcast day effect the score of a series?
  
  broadcasting2 <- broadcasting %>%
    group_by(bc_day_num) %>%
    mutate(n_day = n())
  
  # plot the days on the x axis and scores on the y axis
  ggplot(broadcasting2, aes(x = bc_day_num, y = score, fill = n_day)) +
    # add the columns for each day, use a minimal theme and the viridis colour palette (the best)
    geom_col() + scale_fill_viridis() + theme_minimal()
    # label the plot and the axes
    labs(title="Scores of Animes by Broadcast Day",
         x ="Broadcast Day", y = "Score")
 
      
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
        
  