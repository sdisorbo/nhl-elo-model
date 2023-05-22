#load necessary packaged

library(hockeyR)
library(dplyr)
library(tidyverse)
library(reactable)
library(reactablefmtr)
library(ggimage)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(gt)
library(hrbrthemes)
library(gtExtras)
library(extrafont)
library(showtext)
library(magick)
library(webshot)
library(sportyR)
library(slider)
#####load the season play by play data
hbp <- hockeyR::load_pbp('2022-2023')

#clean/add to the data, handle shootouts, shot types, score diff
  hbp$game_date <- as.Date(hbp$game_date)
  hbp$home_score_diff <- hbp$home_final - hbp$away_final
  hbp$shot <-  ifelse(hbp$event == "Shot" | hbp$event == "Blocked Shot" | hbp$event == "Goal" | hbp$event == "Missed Shot", 1, 0)
  hbp$goal <-  ifelse(hbp$event == "Goal", 1, 0)
  hbp$save <-  ifelse(hbp$shot & !hbp$goal, 1, 0)
  hbp$shootout_counter <- 0

#get game winners
  winners <- hbp %>%
    group_by(game_id) %>% 
    reframe(
      date = first(game_date),
      sd = last(home_score_diff),
      home_won = ifelse(last(home_final) < last(away_final), 0, 1),
      home = last(home_abbreviation),
      away = last(away_abbreviation),
      winner = ifelse(last(home_final) < last(away_final), last(away_abbreviation), last(home_abbreviation)),
      loser = ifelse(last(home_final) <= last(away_final), last(home_abbreviation), last(away_abbreviation)),
      ot = (ifelse(last(period > 3), 1, 0)),
      winner_diff = last(abs(home_score_diff)),
      loser_diff = ifelse(sd < 0, sd, -1*sd),
      shootout_counter = sum(ifelse(period == 5 & goal == 1, ifelse(event_team_type == "home", 1, -1), 0)),
    ) 
  winners <- unique(winners)
  
  #adjust winners based on shootout results
  winners$winner <- ifelse(winners$shootout_counter != 0, ifelse(winners$shootout_counter > 0, winners$home, winners$away), winners$winner)
  winners$loser <- ifelse(winners$shootout_counter != 0, ifelse(winners$shootout_counter < 0, winners$away, winners$home), winners$loser)
  
  winners <- winners %>% select(-home, -away, -shootout_counter)
  
  #get away teams data by game--want expected goals, shot quality, turnovers, etc. 
  away <- hbp %>% 
    mutate(away_save = ifelse(event_goalie_name == away_goalie, 1, 0)) %>% 
    mutate(home_save = ifelse(event_goalie_name == home_goalie, 1, 0)) %>% 
    group_by(game_id, away_abbreviation) %>% 
    reframe(
      gSAX = sum(ifelse(away_save, save-xg, 0), na.rm=T),
      gAX = sum(ifelse(home_save, goal-xg, 0), na.rm=T),
      xGa = sum(ifelse(away_save, xg, 0), na.rm=T),
      xG = sum(ifelse(home_save, xg, 0), na.rm=T),
      shots = sum(ifelse(home_save, 1, 0), na.rm=T),
      qshots = sum(ifelse(home_save & xg > .1, 1, 0), na.rm=T),
      shots_ag = sum(ifelse(away_save, 1, 0), na.rm=T),
      qshots_ag = sum(ifelse(away_save & xg > .1, 1, 0), na.rm=T),
      takes = sum(ifelse(event_type == "TAKEAWAY" & event_team_type == "away" | event_type == "GIVEAWAY" & event_team_type == "home", 1, 0)),
      gives = sum(ifelse(event_type == "TAKEAWAY" & event_team_type == "home" | event_type == "GIVEAWAY" & event_team_type == "away", 1, 0))
    ) %>% 
    left_join(hockeyR::team_logos_colors, by = c("away_abbreviation" = "team_abbr"))
    
    
  #get home data
  home <- hbp %>% 
    mutate(away_save = ifelse(event_goalie_name == away_goalie, 1, 0)) %>% 
    mutate(home_save = ifelse(event_goalie_name == home_goalie, 1, 0)) %>% 
    group_by(game_id, home_abbreviation) %>% 
    reframe(
      gSAX = sum(ifelse(home_save, save-xg, 0), na.rm=T),
      gAX = sum(ifelse(away_save, goal-xg, 0), na.rm=T),
      xGa = sum(ifelse(home_save, xg, 0), na.rm=T),
      xG = sum(ifelse(away_save, xg, 0), na.rm=T),
      shots = sum(ifelse(away_save, 1, 0), na.rm=T),
      qshots = sum(ifelse(away_save & xg > .1, 1, 0), na.rm=T),
      shots_ag = sum(ifelse(home_save, 1, 0), na.rm=T),
      qshots_ag = sum(ifelse(home_save & xg > .1, 1, 0), na.rm=T),
      gives = sum(ifelse(event_type == "TAKEAWAY" & event_team_type == "away" | event_type == "GIVEAWAY" & event_team_type == "home", 1, 0)),
      takes = sum(ifelse(event_type == "TAKEAWAY" & event_team_type == "home" | event_type == "GIVEAWAY" & event_team_type == "away", 1, 0))
    ) %>%  left_join(hockeyR::team_logos_colors, by = c("home_abbreviation" = "team_abbr"))
  
  #additional calculations
  home$qsp <- home$qshots/home$shots
  home$qspa <- home$qshots_ag/home$shots_ag
  away$qsp <- away$qshots/away$shots
  away$qspa <- away$qshots_ag/away$shots_ag
  home$net_turn <- home$takes+home$gives
  away$net_turn <- away$takes+away$gives
  names(home)[names(home) == "home_abbreviation"] <- "team"
  names(away)[names(away) == "away_abbreviation"] <- "team"
  #make one table
  teams <- rbind(away, home)
  teams <- left_join(teams, winners, by = c("game_id"="game_id"))
  
  #get cumulative averages for the important statistics
  teams$csum_gSAX <- ave(teams$gSAX, teams$full_team_name, FUN=cummean)
  teams$csum_gAX <- ave(teams$gAX, teams$full_team_name, FUN=cummean)
  teams$csum_xGa <- ave(teams$xGa, teams$full_team_name, FUN=cummean)
  teams$csum_xG <- ave(teams$xG, teams$full_team_name, FUN=cummean)
  teams$csum_qsp <- ave(teams$qsp, teams$full_team_name, FUN=cummean)
  teams$csum_qspa <- ave(teams$qspa, teams$full_team_name, FUN=cummean)
  teams$csum_turns <- ave(teams$net_turn, teams$full_team_name, FUN=cummean)
  

  #initialize team elo ratings at each game based on stats accumulated
  teams$season_score <- ((teams$csum_gSAX/10) + teams$csum_gAX - teams$csum_xGa + teams$csum_xG + 30*teams$qsp - 30*teams$qspa + teams$csum_turns/10)*13
  teams$opp <- ifelse(teams$winner == teams$full_team_name, teams$loser, teams$winner)
  #names(teams)[names(final) == "away"] <- " "
  
  full_names <- team_logos_colors %>% select(full_team_name, team_abbr)
  names(full_names)[names(full_names) == "full_team_name"] <- "opp_full_name"
  teams <- left_join(teams, full_names, by = c("opp"="team_abbr"))
  
  #get their opponent elo score
  scores <- teams %>% 
    group_by(full_team_name, game_id) %>% 
    reframe(
      opp_score = season_score
    )
  
  teams$opp_score <- teams$season_score
 # teams <- teams %>% 
  #  left_join(scores, by = c("opp_full_name" = "full_team_name", "game_id" = "game_id")) 
  teams$season_score[is.na(teams$season_score)] <- 0
  
  #initialize ratings +1000 baseline along with the opponent scores
  #initialize win and loss counters
  teams$rating <- (teams$season_score)+1000
  teams$opp_score <- (teams$opp_score)+1000
  teams$change <- 0
  teams$win <- 0
  teams$loss <- 1
  teams$ot_loss <- 0
  teams$before <- 0
  teams$last_score <- teams$season_score
  teams$rating[is.na(teams$rating)] <- mean(teams$rating, na.rm=T)
  teams$opp_score[is.na(teams$opp_score)] <- mean(teams$opp_score, na.rm=T)
  teams <- teams %>% drop_na(opp_full_name)
  
  teams <- left_join(teams, full_names, by = c("full_team_name"="opp_full_name"))
  teams$Points <- 0
  teams <- teams %>% drop_na(winner_diff)

#loop through each game, determine winner, adjust elo ratings accordingly and change win, loss, and other placeholders
#loop through remaining data to adjust each rating to be current rating O(nlogn)
for(i in 1:nrow(teams)){
  r1 = 10^(teams$rating[i]/400)
  r2 = 10^(teams$opp_score[i]/400)
  e1 = r1/(r1+r2)
  e2 = r2/(r1+r2)
  s1 = ifelse(teams$team_abbr[i] == teams$winner[i], 1, 0)
  if(teams$team_abbr[i] == teams$winner[i]){
    teams$win[i] = 1
    teams$Points[i] = 2}
  if(teams$team_abbr[i] == teams$winner[i]){teams$loss[i] = 0}
  else if(teams$team_abbr[i] != teams$winner[i] & teams$ot[i] == 1){
    teams$Points[i] = 1
    teams$ot_loss[i] = 1
  }
  s2= ifelse(teams$team_abbr[i] == teams$winner[i] & teams$ot[i] == 1, .5, ifelse(teams$team_abbr[i] == teams$winner[i], 0,  1))      
  diff = ifelse(teams$team_abbr[i] == teams$winner[i], teams$winner_diff[i], teams$loser_diff[i])
  teams$change[i] <- 50*(s1-e1) + diff + (teams$season_score[i]-teams$last_score[i])
  teams$before[i] <- teams$rating[i]
  teams$last_score[i] <- teams$season_score[i]
  teams$rating[i] <- teams$rating[i] + teams$change[i]
  teams$opp_score[i] <- teams$opp_score[i] + (100*(s2-e2) + (teams$loser_diff[i])) + (teams$season_score[i]-teams$last_score[i])
  
  k = i+1
  for(j in k:nrow(teams)){
    if(i != nrow(teams)){ #length of the df
      if(teams$team_abbr[j] == teams$team_abbr[i]){teams$rating[j] = teams$rating[i]}
      if(teams$opp_full_name[j] == teams$opp_full_name[i]){teams$opp_score[j] = teams$opp_score[i]}
      if(teams$team_abbr[j] == teams$team_abbr[i]){teams$last_score[j] = teams$season_score[i]}
    }
  }
}
  
#summing totals for wins and losses and points
teams$wins <- ave(teams$win, teams$full_team_name, FUN=cumsum)
teams$losses <- ave(teams$loss, teams$full_team_name, FUN=cumsum)
teams$ot_losses <- ave(teams$ot_loss, teams$full_team_name, FUN=cumsum)
teams$Points <- ave(teams$Points, teams$full_team_name, FUN=cumsum)
teams$losses <- teams$losses - teams$ot_losses

teams$Team <- teams$full_team_name

#get new df with displayable data 
final <- teams %>% 
  group_by(full_team_name) %>% 
  reframe(
    ` ` = first(team_logo_espn),
    Rating = round((last(rating))),
    Record = paste(last(wins), last(losses), last(ot_losses), sep="-"),
    Change = round(last(change)),
    Points = last(Points)
  )

#rankings 
final$Rank <- NA
order.scores<-order(-final$Rating,final$full_team_name)
final$Rank[order.scores] <- 1:nrow(final)

#rearrange cols
final <- final[c(7,1,2,4,5,6,3)]

#display the results in a reactable table
reactable(final,
          showSortIcon = FALSE,
          theme = fivethirtyeight(),
          searchable = TRUE,
          language = reactableLang(
            searchPlaceholder = "SEARCH FOR A TEAM..."),
          columns = list(
            Rank = colDef(show = T, maxWidth = 55),
            ` ` = colDef(
              name = " ",
              align = "center",
              maxWidth = 50,
              cell = embed_img(height = "30", width = "30")
            ),
            full_team_name = colDef(show = T, maxWidth = 700, name = "Team"),
            Record = colDef(show = T, maxWidth = 85),
            Change = colDef(
              show = T,
              maxWidth = 70,
              style = color_scales(final, colors = c("#fd84a9", "white", "#42c2ca")),
            ),
            Rating = colDef(
              show = T,
              maxWidth = 70,
              style = color_scales(final, colors = c("#fd84a9", "white", "#42c2ca"))
            ),
            Points = colDef(
              show = T,
              maxWidth = 70,
              style = color_scales(final, colors = c("#fd84a9", "white", "#42c2ca"))
            )
          ),
          fullWidth = F,
          defaultPageSize = 32,
) %>% add_title("DRAGON Model NHL Ratings 2021", font_size = 30) %>% 
  add_source("By: Samuel DiSorbo @analytacist with {reactablefmtr} •  Data: hockeyR", font_size = 12)


#progression chart
teams$Day <- seq.int(nrow(teams)) 

transparent <- function(img) {
  image_fx(img, expression = "0.35*a", channel = "alpha")
}

teams$rating[is.na(teams$rating)] <- mean(teams$rating, na.rm=T)

#get mins and max for the best teams to compare any given team to the league extremes
max <- base::max
min <- base::min
teams$high <- slide_dbl(teams$rating, max, .before = 600, .complete = FALSE)
teams$low <- slide_dbl(teams$rating, min, .before = 600, .complete = FALSE)

maxx <- teams %>% 
  group_by(Day) %>% 
  reframe(
    max = high,
  ) 

minn <- teams %>% 
  group_by(Day) %>% 
  reframe(
    min = low
  ) 
#minn$min <- ifelse(minn$min > 880, 840, minn$min)


team <- teams %>% 
  filter(full_team_name == "Dallas Stars")%>% 
  arrange(Day)
  
#progression table
ggplot() +
  #ylim(800, 1520)+
  geom_hline(yintercept = mean(teams$rating), color = "black")+
  geom_ribbon(aes(as.numeric(maxx$Day), ymin = minn$min, ymax = maxx$max), alpha = 0.4) +
  geom_line(aes(x=as.numeric(maxx$Day), y=maxx$max), color="black", size=.5) + 
  geom_image(aes(x = 1350, y = mean(teams$rating)), image = team$team_logo_espn[1], 
             size =.2, asp = 16/8, image_fun = transparent) +
  geom_line(aes(x=as.numeric(minn$Day), y=minn$min), color="black", size=.5) +
  geom_line(aes(x=as.numeric(team$Day), y=team$rating), color=team$team_color1, size=1,) +
  geom_point(aes(x = last(as.numeric(team$Day)), y=last(team$rating)), shape=21, size=3, 
             color=team$team_color1[1], fill="white",) +
  theme_bw()+
  xlim(1, 2700/1)+
  labs(
    x = "Day",
    y = "Rating",
    title = glue::glue("{unique(team$full_team_name)} Team Rating"),
  )+
  geom_text(mapping = aes(x = first(as.numeric(teams$Day)), y = maxx$max[1]+280), label = "NHL Best", size=1.8, color = "black")+
  geom_label(mapping = aes(x = first(as.numeric(teams$Day)), y = mean(teams$rating)), label = "Average", size=1.8)+
  geom_text(mapping = aes(x = first(as.numeric(teams$Day)), y = minn$min[1]-20), label = "NHL Worst", size=1.8, color="black")+
  theme(plot.title = element_text(size = 15, hjust = 0.47, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text = element_text(size = 9, hjust=.5),
        axis.title = element_text(size = 10, hjust=0.47),
        panel.background = element_rect(fill = "gray96"),
        plot.background = element_rect(fill = "gray96"),
        panel.border = element_blank(),
        text = element_text(color = "black", family = "Palatino"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
  )#+



  
