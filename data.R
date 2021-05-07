
#############

#Jose Fernandez
#May 2021


#############

#EPV
edat <- feather::read_feather("epv.feather")
epva <- read.csv("epva.csv") %>%
  mutate(Player = sprintf(paste0("<img src='", Photo, "' width=70px><div class='jhr'>%s</div></img>"), Player)) %>%
  mutate(Team = ifelse(Team == "Away",  
                          sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/miami-heat-logo.png' width=90px><div class='jhr'>\r</div></img>", "Heat"), 
                          sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/brooklyn-nets-logo.png' width=90px><div class='jhr'>\r</div></img>", "Nets"))) %>%
  select(Team, Player, EPVA)

#PBP
pbp <- read.csv("pbp.csv")

#players
players <- read.csv("players2013.csv")

#prepare to replace player id with player name

players <- players %>%
  mutate(Player = paste(firstname, lastname, sep = " ")) %>%
  select(player_id, Player)

a1 <- players %>% select(PA1 = Player, a1_id = player_id)
a2 <- players %>% select(PA2 = Player, a2_id = player_id)
a3 <- players %>% select(PA3 = Player, a3_id = player_id)
a4 <- players %>% select(PA4 = Player, a4_id = player_id)
a5 <- players %>% select(PA5 = Player, a5_id = player_id)
h1 <- players %>% select(PH1 = Player, h1_id = player_id)
h2 <- players %>% select(PH2 = Player, h2_id = player_id)
h3 <- players %>% select(PH3 = Player, h3_id = player_id)
h4 <- players %>% select(PH4 = Player, h4_id = player_id)
h5 <- players %>% select(PH5 = Player, h5_id = player_id)

#join all player tables
edat1 <- edat %>%
  
  full_join(a1, by = "a1_id") %>%
  full_join(a2, by = "a2_id") %>%
  full_join(a3, by = "a3_id") %>%
  full_join(a4, by = "a4_id") %>%
  full_join(a5, by = "a5_id") %>%
  full_join(h1, by = "h1_id") %>%
  full_join(h2, by = "h2_id") %>%
  full_join(h3, by = "h3_id") %>%
  full_join(h4, by = "h4_id") %>%
  full_join(h5, by = "h5_id") %>%
  
  # select relevant metrics  
  select(game, time, quarter, game_clock, shot_clock, 
         ball_x, ball_y, ball_z,  
         PA1, a1_x, a1_y,
         PA2, a2_x, a2_y,
         PA3, a3_x, a3_y,
         PA4, a4_x, a4_y,
         PA5, a5_x, a5_y,
         PH1, h1_x, h1_y,
         PH2, h2_x, h2_y,
         PH3, h3_x, h3_y,
         PH4, h4_x, h4_y,
         PH5, h5_x, h5_y,
         everything()) %>%
  filter(possID != 0) %>%
  
  #translate events (source https://github.com/howardbaek/nba-animation/blob/master/final-result.Rmd)
  mutate_at(
    vars(one_of('a1_event', 'a2_event', 'a3_event', 'a4_event', 'a5_event', 'h1_event', 'h2_event', 'h3_event', 'h4_event', 'h5_event')),
    funs(case_when(
      . == 1 ~ "Free Throw Made",
      . == 2 ~ "Free Throw Missed",
      . == 3 ~ "Shot Made",
      . == 4 ~ "Shot Missed",
      . == 5 ~ "Offensive Rebound",
      . == 6 ~ "Defensive Rebound",
      . == 7 ~ "Turnover",
      . == 8 ~ "Foul",
      . == 9 ~ "Violation",
      . == 10 ~ "Substitution",
      . == 11 ~ "Timeout",
      . == 12 ~ "Jump Ball",
      . == 13 ~ "Ejection",
      . == 14 ~ "Start Period",
      . == 15 ~ "End Period",
      . == 16 ~ "Clock Sync",
      . == 17 ~ "Instant Replay",
      . == 18 ~ "Replay Ruling",
      . == 19 ~ "Game Over",
      . == 20 ~ "Stoppage",
      . == 21 ~ "Dribble",
      . == 22 ~ "Pass",
      . == 23 ~ "",
      . == 24 ~ "Shot Block",
      . == 25 ~ "Assist",
      TRUE ~ ""
    ))) %>%
  
  #fill next x rows with events to help reading on gif
  mutate_at(
    vars(one_of('a1_event', 'a2_event', 'a3_event', 'a4_event', 'a5_event', 'h1_event', 'h2_event', 'h3_event', 'h4_event', 'h5_event')),
    
    funs(case_when(
      
      is.na(lag(.)) ~ .,
      lag(.) != "" ~ lag(.),
      lag(.,2) != "" ~ lag(.,2),
      lag(.,3) != "" ~ lag(.,3),
      lag(.,4) != "" ~ lag(.,4),
      lag(.,5) != "" ~ lag(.,5),
      lag(.,6) != "" ~ lag(.,6),
      lag(.,7) != "" ~ lag(.,7),
      lag(.,8) != "" ~ lag(.,8),
      lag(.,9) != "" ~ lag(.,9),
      lag(.,10) != "" ~ lag(.,10),
      lag(.,11) != "" ~ lag(.,11),
      lag(.,12) != "" ~ lag(.,12),
      lag(.,13) != "" ~ lag(.,13),
      lag(.,14) != "" ~ lag(.,14),
      lag(.,15) != "" ~ lag(.,15),
      lag(.,16) != "" ~ lag(.,16),
      TRUE ~ .
      
    )))

#prepare players table to identify which team is in offense
players_1 <- players %>% select(poss = player_id, Player)

edat1 <- edat1 %>%
  full_join(players_1, by = "poss") %>%
  group_by(possID) %>%
  mutate(Possession = ifelse(Player == PA1, "Away",
                             ifelse(Player == PA2, "Away",
                                    ifelse(Player == PA3, "Away",
                                           ifelse(Player == PA4, "Away",
                                                  ifelse(Player == PA5, "Away", "Home")))))) %>%
  fill(Possession, .direction = "downup") %>%
  select(-Player)


library(sparkline)
library(DT)

a <- edat1 %>%
  select(Period = quarter, game_clock, possID, epv.smooth, Offense1 = Possession) %>%
  filter(!is.na(epv.smooth)) %>%
  mutate(epv.smooth = round(epv.smooth,3)) %>%
  group_by(Period, Offense1, possID) %>%
  mutate(game_clock = last(game_clock)) %>%
  group_by(Period, game_clock, Offense1, possID) %>%
  
  summarise(
    
    `EPV Difference` = round(last(epv.smooth)-first(epv.smooth),3),
    
    EPV_Trend = spk_chr(epv.smooth, 
                        type = "line", 
                        height = 50,
                        width = 200,
                        lineColor = ifelse( `EPV Difference` > 0, "springgreen", "tomato"), 
                        lineWidth = 1,
                        fillColor = "transparent", 
                        minSpotColor = "red",
                        maxSpotColor = "green",
                        spotColor = F,
                        chartRangeClip = T,
                        tooltipChartTitle = "EPV Trend")) %>% 
  
  full_join(pbp) %>%
  
  na.omit() %>%
  
  select(Period, game_clock, Poss = possID, Play_by_Play, Score, EPV_Trend, `EPV Difference`, Offense1) %>%
  
  mutate(Period = case_when(
    Period == 1 ~ "1st",
    Period == 2 ~ "2nd",
    Period == 3 ~ "3rd",
    Period == 4 ~ "4th",
    TRUE ~ "NA"
  )) %>% 
  mutate(minutes = game_clock %/% 60) %>% 
  mutate(seconds = round(game_clock %% 60,1)) %>%
  mutate(game_clock = paste(minutes,"m ", seconds, "s", sep = "")) %>%
  select(-minutes, -seconds) %>%
  arrange(Period, Poss) %>%
  mutate(Offense = ifelse(Offense1 == "Away",  
                          sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/miami-heat-logo.png' width=60px><div class='jhr'>\r</div></img>", "Heat"), 
                          sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/brooklyn-nets-logo.png' width=60px><div class='jhr'>\r</div></img>", "Nets"))) %>%
  ungroup()





library(htmltools)
library(reactable)


#function to create color grades for background
orange_pal <- function(x) rgb(colorRamp(c("#cc3232", "#db7b2b", "#e7b416", "#99c140", "#2dc937"))(x), maxColorValue = 255)



EPVA_tab <- reactable(epva,
 
          pagination = FALSE,
          highlight = TRUE,
          showSortIcon = FALSE,
          borderless = TRUE,
          filterable = F,
          theme = reactable::reactableTheme(backgroundColor = "transparent", color = "white"),
          groupBy = c("Team"),
          defaultSorted = c("EPVA", "Player", "Team"),
          
          columns = list(
            
            
            Team = colDef(html = T,
                          style = list(fontFamily = "monospace", whiteSpace = "pre", fontWeight = "bold"),
                          class = "image",
                          maxWidth = 150,
                          header = JS("function(colInfo) {
                                      return colInfo.column.name + '<div style=\"color: #999\">Heat @ Nets (2013)</div>'}")
                          ),
            
            Player = colDef(html = T,
                            style = list(fontFamily = "monospace", whiteSpace = "pre", fontWeight = "bold"),
                            align = "center",
                            class = "imageText",
                            maxWidth = 200),
            
            EPVA = colDef(format = colFormat(digits = 3), 
                           defaultSortOrder = "desc",
                           maxWidth = 80,
                           align = "center",
                           class = "row1",
                           style = function(value) {
                             normalized <- (value - min(epva$EPVA)) / (max(epva$EPVA) - min(epva$EPVA))
                             color <- orange_pal(normalized)
                             list(fontFamily = "monospace", whiteSpace = "pre", background = color, color = "black", fontWeight = "bold")}
            )
            
          )
       )
                   
