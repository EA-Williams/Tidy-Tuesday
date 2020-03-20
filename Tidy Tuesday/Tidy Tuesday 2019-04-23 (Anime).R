library(tidyverse)
library(lubridate)
library(viridis)

# load the tidy and raw data
#tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
raw_df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/raw_anime.csv")

# my questions

# 1) how does broadcast day / time affect the score of a series?
# 2) how does the source of an anime  affect the score?
# 3) what's the disparity between my rankings of 10 anime and their rankings on MAL?
  
  
## Q1: how does broadcast day / time affect the score of a series?

# function to convert "broadcast" to numbered day ('bcd' is broadcast day)
  
 bcd_to_num <- function(bcd) {
   # take the first word
    day_num <- str_extract(bcd, '[A-Za-z]+') %>%
      # convert to a number
      recode("Mondays" = 1,
             "Tuesdays" = 2,
             "Wednesdays" = 3,
             "Thursdays" = 4,
             "Fridays" = 5,
             "Saturdays" = 6,
             "Sundays" = 7)
    return(day_num)
  }
  
 # create the 'broadcasting' dataframe from 'raw_df'
 # there will be a warning for the animes where the broadcast time/day was "unknown"
  broadcasting <- raw_df %>%
    # keep only these four columns
    select(animeID, name, score, broadcast) %>%
    # create new columns for broadcast day (bc_day_num), broadcast time, and broadcast hour (e.g. 12:30 -> 12)
    mutate(bc_day_num = bcd_to_num(broadcast),
           bc_time = str_extract(broadcast, '[:digit:][:digit:][:punct:][:digit:][:digit:]'),
           bc_hour = hour(strptime(bc_time, format = "%H:%M"))) %>%
    # remove animes with missing data for day, hour, or score
    filter(!is.na(bc_day_num), !is.na(bc_hour), !is.na(score)) %>%
    # group by day and hour
    group_by(bc_day_num, bc_hour) %>%
    # calculate the mean score for each day and hour combination
    summarise("dh_score" = mean(score)) %>% # day-hour-score
    ungroup()
  
  # create a theme for the upcoming plot
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
    
  # plot using the broadcasting dataframe, with day on the x axis, hour on the y axis, filled according to score
  ggplot(broadcasting, aes(x = bc_day_num, y = bc_hour, fill = dh_score)) + 
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
    # group by just the day
    group_by(bc_day_num) %>%
    # count how many anime there are for each day, and calculate the mean score for each day
    mutate(n_day = n(), day_score = mean(score)) %>%
    ungroup()
  
  bcg <- broadcasting %>%
    # group by just the day
    group_by(bc_day_num) %>%
    # count how many anime there are for each day, and calculate the mean score for each day
    summarise(n_day = n(), day_score = mean(score)) %>%
    ungroup()
  
  # plot the days on the x axis and scores on the y axis
  ggplot(bcg, aes(x = bc_day_num, y = day_score)) + ##, fill = n_day
    # add the columns for each day, use a minimal theme and the viridis colour palette (the best)
    geom_col() + scale_fill_viridis() + theme_minimal() %>%
    # label the plot and the axes
    labs(title="Scores of Animes by Broadcast Day",
         x ="Broadcast Day", y = "Score")
  
  # why is day_score above 2000??? (max should be around 7)
  # no difference between "y = score" and "y = day_score"
  # even "y = n_day" isn't working correctly...
 
  table(broadcasting$day_score) 
  plot(broadcasting$bc_day_num, broadcasting$day_score)
  
  
    #Q2: not finished - also problem with plotting means
  
    # source : original / manga(4-koma manga, Digital manga, Manga, Web manga) / novel (Light novel, 
    # Novel, Visual novel) /  Other
    
  sources <- raw_df %>%
    select(animeID, name, score, source) %>%
    mutate(source_group = recode(source, "4-koma manga" = "Manga",
                                 "Digital manga" = "Manga",
                                 "Web manga" = "Manga",
                                 "Light novel" = "Novel",
                                 "Book" = "Novel",
                                 "Visual novel" = "Game",
                                 "Card game" = "Game",
                                 .default = "Other")) %>%
    group_by(source_group) %>%
    mutate(n_source = n(), mean_score = mean(score, na.rm = TRUE)) %>%
    ungroup()
    
  table(sources$source_group)
  
  ggplot(sources, aes(x = source_group, y = mean_score))+ #fill = n_source)) +
    # add the columns for each source group
    geom_col() +
    # add labels on top of each source group column with the number of anime (sample size)
    # use prettyNum to include commas in numbers in the thousands. Align the labels above the columns.
    geom_text(aes(label = paste("n =", prettyNum(n,big.mark=",")), vjust=-0.25), size = 2.5) +
    # use a minimal theme and the viridis colour palette (the best)
    theme_minimal() + scale_fill_viridis(discrete = TRUE) +
    # label the plot and the axes
    labs(title="Anime Scources by Source",
         x ="Anime Source", y = "Score")
  
  
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
        
  