#get libraries
library(hash)

#create empty dictionaries for later use
second_round <- hash()
third_round <- hash()
finals <- hash()
champion <- hash()

#record playoff teams and their seeding for East and West
Team <- c("Boston Bruins", "Florida Panthers", "Toronto Maple Leafs", "Tampa Bay Lightning", "New Jersey Devils", "New York Rangers", "Carolina Hurricanes", "New York Islanders")
Seed <- c(1, 8, 4, 6, 3, 5, 2, 7)
east_teams <- data.frame(Team, Seed)

Team <- c("Vegas Golden Knights", "Winnipeg Jets", "Edmonton Oilers", "Los Angeles Kings", "Dallas Stars", "Minnesota Wild", "Seattle Kraken", "Colorado Avalanche")
Seed <- c(1, 8, 2, 5, 3, 6, 7, 4)
west_teams <- data.frame(Team, Seed)

#combining your df with the East and West seedings with the final df created by the nhl_elo_model script...now you have the playoff teams and their respective elo rating for east and west
east <- final %>% 
  left_join(east_teams, by=c("Team" = "Team")) %>% 
  drop_na(Seed)

west <- final %>% 
  left_join(west_teams, by=c("Team" = "Team")) %>% 
  drop_na(Seed)

#function to simulate the first round for the eastern side, 7 game series, adjust the elo ratings after each matchup
first_round_sim_east <- function(east, second_round){
  #1 vs 8
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
   # print("hello")
    r1 = 10^(east$Rating[east$Seed==1]/400)
    r2 = 10^(east$Rating[east$Seed==8]/400)
    e1 = r1/(r1+r2)
    e2 = r2/(r1+r2)
    prob = 1/(1+10^((east$Rating[east$Seed==8]-east$Rating[east$Seed==1])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
      #home_wins = home_wins+1
      east$Rating[east$Seed==1] <- 50*(1-e1) + east$Rating[east$Seed==1]
      east$Rating[east$Seed==8] <- 50*(0-e1) + east$Rating[east$Seed==8]
      home_wins = home_wins+1
    }
    else{
      #away_wins = away_wins+1
      east$Rating[east$Seed==1] <- 50*(0-e1) + east$Rating[east$Seed==1]
      east$Rating[east$Seed==8] <- 50*(1-e1) + east$Rating[east$Seed==8]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      second_round[[east$Team[east$Seed==1]]] = second_round[[east$Team[east$Seed==1]]]+1
      one = 1
      break
    }
    if(away_wins == 4){
      second_round[[east$Team[east$Seed==8]]] =  second_round[[east$Team[east$Seed==8]]]+1
      one = 8
      break
    }
  }
  #2 vs 7
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    r1 = 10^(east$Rating[east$Seed==2]/400)
    r2 = 10^(east$Rating[east$Seed==7]/400)
    e1 = r1/(r1+r2)
    e2 = r2/(r1+r2)
    prob = 1/(1+10^((east$Rating[east$Seed==7]-east$Rating[east$Seed==2])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
     # home_wins = home_wins+1
      east$Rating[east$Seed==2] <- 50*(1-e1) + east$Rating[east$Seed==2]
      east$Rating[east$Seed==7] <- 50*(0-e1) + east$Rating[east$Seed==7]
      home_wins = home_wins+1
    }
    else{
     # away_wins = away_wins+1
      east$Rating[east$Seed==2] <- 50*(0-e1) + east$Rating[east$Seed==2]
      east$Rating[east$Seed==7] <- 50*(1-e1) + east$Rating[east$Seed==7]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      second_round[[east$Team[east$Seed==2]]] = second_round[[east$Team[east$Seed==2]]]+1
      two = 2
      break
    }
    if(away_wins == 4){
      second_round[[east$Team[east$Seed==7]]] =  second_round[[east$Team[east$Seed==7]]]+1
      two = 7
      break
    }
  }
  #3 vs 5
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    r1 = 10^(east$Rating[east$Seed==3]/400)
    r2 = 10^(east$Rating[east$Seed==5]/400)
    e1 = r1/(r1+r2)
    e2 = r2/(r1+r2)
    prob = 1/(1+10^((east$Rating[east$Seed==5]-east$Rating[east$Seed==3])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
      home_wins = home_wins+1
      east$Rating[east$Seed==3] <- 50*(1-e1) + east$Rating[east$Seed==3]
      east$Rating[east$Seed==5] <- 50*(0-e1) + east$Rating[east$Seed==5]
     # home_wins = home_wins+1
    }
    else{
      away_wins = away_wins+1
      east$Rating[east$Seed==3] <- 50*(0-e1) + east$Rating[east$Seed==3]
      east$Rating[east$Seed==5] <- 50*(1-e1) + east$Rating[east$Seed==5]
      #away_wins = away_wins+1
    }
    if(home_wins == 4){
      second_round[[east$Team[east$Seed==3]]] = second_round[[east$Team[east$Seed==3]]]+1
      three = 3
      break
    }
    if(away_wins == 4){
      second_round[[east$Team[east$Seed==5]]] =  second_round[[east$Team[east$Seed==5]]]+1
      three = 5
      break
    }
  }
  #4 vs 6
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    r1 = 10^(east$Rating[east$Seed==4]/400)
    r2 = 10^(east$Rating[east$Seed==6]/400)
    e1 = r1/(r1+r2)
    e2 = r2/(r1+r2)
    prob = 1/(1+10^((east$Rating[east$Seed==6]-east$Rating[east$Seed==4])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
      #home_wins = home_wins+1
      east$Rating[east$Seed==4] <- 50*(1-e1) + east$Rating[east$Seed==4]
      east$Rating[east$Seed==6] <- 50*(0-e1) + east$Rating[east$Seed==6]
      home_wins = home_wins+1
    }
    else{
     # away_wins = away_wins+1
      east$Rating[east$Seed==4] <- 50*(0-e1) + east$Rating[east$Seed==4]
      east$Rating[east$Seed==6] <- 50*(1-e1) + east$Rating[east$Seed==6]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      second_round[[east$Team[east$Seed==4]]] = second_round[[east$Team[east$Seed==4]]]+1
      four = 4
      break
    }
    if(away_wins == 4){
      second_round[[east$Team[east$Seed==6]]] =  second_round[[east$Team[east$Seed==6]]]+1
      four = 6
      break
    }
  }
  advance <- list(two, three, one, four)
  return(advance)
} 

#first round simulator in the west 
first_round_sim_west <- function(west, second_round){
  #1 vs 8
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    # print("hello")
    r1 = 10^(west$Rating[west$Seed==1]/400)
    r2 = 10^(west$Rating[west$Seed==8]/400)
    e1 = r1/(r1+r2)
    e2 = r2/(r1+r2)
    prob = 1/(1+10^((west$Rating[west$Seed==8]-west$Rating[west$Seed==1])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
      #home_wins = home_wins+1
      west$Rating[west$Seed==1] <- 50*(1-e1) + west$Rating[west$Seed==1]
      west$Rating[west$Seed==8] <- 50*(0-e1) + west$Rating[west$Seed==8]
      home_wins = home_wins+1
    }
    else{
     # away_wins = away_wins+1
      west$Rating[west$Seed==1] <- 50*(0-e1) + west$Rating[west$Seed==1]
      west$Rating[west$Seed==8] <- 50*(1-e1) + west$Rating[west$Seed==8]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      second_round[[west$Team[west$Seed==1]]] = second_round[[west$Team[west$Seed==1]]]+1
      one = 1
      break
    }
    if(away_wins == 4){
      second_round[[west$Team[west$Seed==8]]] =  second_round[[west$Team[west$Seed==8]]]+1
      one = 8
      break
    }
  }
  #2 vs 7
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    r1 = 10^(west$Rating[west$Seed==2]/400)
    r2 = 10^(west$Rating[west$Seed==7]/400)
    e1 = r1/(r1+r2)
    e2 = r2/(r1+r2)
    prob = 1/(1+10^((west$Rating[west$Seed==7]-west$Rating[west$Seed==2])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
      home_wins = home_wins+1
      west$Rating[west$Seed==2] <- 50*(1-e1) + west$Rating[west$Seed==2]
      west$Rating[west$Seed==7] <- 50*(0-e1) + west$Rating[west$Seed==7]
     # home_wins = home_wins+1
    }
    else{
      away_wins = away_wins+1
      west$Rating[west$Seed==2] <- 50*(0-e1) + west$Rating[west$Seed==2]
      west$Rating[west$Seed==7] <- 50*(1-e1) + west$Rating[west$Seed==7]
     # away_wins = away_wins+1
    }
    if(home_wins == 4){
      second_round[[west$Team[west$Seed==2]]] = second_round[[west$Team[west$Seed==2]]]+1
      two = 2
      break
    }
    if(away_wins == 4){
      second_round[[west$Team[west$Seed==7]]] =  second_round[[west$Team[west$Seed==7]]]+1
      two = 7
      break
    }
  }
  #3 vs 5
 
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    r1 = 10^(west$Rating[west$Seed==3]/400)
    r2 = 10^(west$Rating[west$Seed==5]/400)
    e1 = r1/(r1+r2)
    e2 = r2/(r1+r2)
    prob = 1/(1+10^((west$Rating[west$Seed==5]-west$Rating[west$Seed==3])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
     # home_wins = home_wins+1
      west$Rating[west$Seed==3] <- 50*(1-e1) + west$Rating[west$Seed==3]
      west$Rating[west$Seed==5] <- 50*(0-e1) + west$Rating[west$Seed==5]
      home_wins = home_wins+1
    }
    else{
     # away_wins = away_wins+1
      west$Rating[west$Seed==3] <- 50*(0-e1) + west$Rating[west$Seed==3]
      west$Rating[west$Seed==5] <- 50*(1-e1) + west$Rating[west$Seed==5]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      second_round[[west$Team[west$Seed==3]]] = second_round[[west$Team[west$Seed==3]]]+1
      three = 3
      break
    }
    if(away_wins == 4){
      second_round[[west$Team[west$Seed==5]]] =  second_round[[west$Team[west$Seed==5]]]+1
      three = 5
      break
    }
  }
  #4 vs 6
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    r1 = 10^(west$Rating[west$Seed==4]/400)
    r2 = 10^(west$Rating[west$Seed==6]/400)
    e1 = r1/(r1+r2)
    e2 = r2/(r1+r2)
    prob = 1/(1+10^((west$Rating[west$Seed==6]-west$Rating[west$Seed==4])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
      #home_wins = home_wins+1
      west$Rating[west$Seed==4] <- 50*(1-e1) + west$Rating[west$Seed==4]
      west$Rating[west$Seed==6] <- 50*(0-e1) + west$Rating[west$Seed==6]
      home_wins = home_wins+1
    }
    else{
     # away_wins = away_wins+1
      west$Rating[west$Seed==4] <- 50*(0-e1) + west$Rating[west$Seed==4]
      west$Rating[west$Seed==6] <- 50*(1-e1) + west$Rating[west$Seed==6]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      second_round[[west$Team[west$Seed==4]]] = second_round[[west$Team[west$Seed==4]]]+1
      four = 4
      break
    }
    if(away_wins == 4){
      second_round[[west$Team[west$Seed==6]]] =  second_round[[west$Team[west$Seed==6]]]+1
      four = 6
      break
    }
  }
  advance <- list(two, three, one, four)
  return(advance)
} 

#second round east sim using winning teams from the first round sim 
second_round_sim_east <- function(east, third_round, r1){
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    # print("hello")
    r3 = 10^(east$Rating[east$Seed==r1[[1]]]/400)
    r2 = 10^(east$Rating[east$Seed==r1[[2]]]/400)
    e1 = r3/(r3+r2)
    e2 = r2/(r3+r2)
    prob = 1/(1+10^((east$Rating[east$Seed==r1[[2]]]-east$Rating[east$Seed==r1[[1]]])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
     # home_wins = home_wins+1
      east$Rating[east$Seed==r1[[1]]] <- 50*(1-e1) + east$Rating[east$Seed==r1[[1]]]
      east$Rating[east$Seed==r1[[2]]] <- 50*(0-e1) + east$Rating[east$Seed==r1[[2]]]
      home_wins = home_wins+1
    }
    else{
      #away_wins = away_wins+1
      east$Rating[east$Seed==r1[[1]]] <- 50*(0-e1) + east$Rating[east$Seed==r1[[1]]]
      east$Rating[east$Seed==r1[[2]]] <- 50*(1-e1) + east$Rating[east$Seed==r1[[2]]]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      third_round[[east$Team[east$Seed==r1[[1]]]]] = third_round[[east$Team[east$Seed==r1[[1]]]]]+1
      one = r1[[1]]
      break
    }
    if(away_wins == 4){
      third_round[[east$Team[east$Seed==r1[[2]]]]] =  third_round[[east$Team[east$Seed==r1[[2]]]]]+1
      one = r1[[2]]
      break
    }
  }
  #2 vs 7
  
  home_wins = 0
  away_wins = 0
  for(i in 1:5){
    #set params
    r3 = 10^(east$Rating[east$Seed==r1[[3]]]/400)
    r2 = 10^(east$Rating[east$Seed==r1[[4]]]/400)
    e1 = r3/(r3+r2)
    e2 = r2/(r3+r2)
    prob = 1/(1+10^((east$Rating[east$Seed==r1[[4]]]-east$Rating[east$Seed==r1[[3]]])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
    #  home_wins = home_wins+1
      east$Rating[east$Seed==2] <- 50*(1-e1) + east$Rating[east$Seed==r1[[3]]]
      east$Rating[east$Seed==7] <- 50*(0-e1) + east$Rating[east$Seed==r1[[4]]]
      home_wins = home_wins+1
    }
    else{
     # away_wins = away_wins+1
      east$Rating[east$Seed==2] <- 50*(0-e1) + east$Rating[east$Seed==r1[[3]]]
      east$Rating[east$Seed==7] <- 50*(1-e1) + east$Rating[east$Seed==r1[[4]]]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      third_round[[east$Team[east$Seed==r1[[3]]]]] = third_round[[east$Team[east$Seed==r1[[3]]]]]+1
      two = r1[[3]]
      break
    }
    if(away_wins == 4){
      third_round[[east$Team[east$Seed==r1[[4]]]]] =  third_round[[east$Team[east$Seed==r1[[4]]]]]+1
      two = r1[[4]]
      break
    }
  }
  
  advance = list(one, two)
  return(advance)
}

#second round west sim using winning teams from the first round sim 
second_round_sim_west <- function(west, third_round, r1a){
  home_wins = 2
  away_wins = 4
  for(i in 1:7){
    #set params
    # print("hello")
    r3 = 10^(west$Rating[west$Seed==r1a[[1]]]/400)
    r2 = 10^(west$Rating[west$Seed==r1a[[2]]]/400)
    e1 = r3/(r3+r2)
    e2 = r2/(r3+r2)
    prob = 1/(1+10^((west$Rating[west$Seed==r1a[[2]]]-west$Rating[west$Seed==r1a[[1]]])/400))
    #sim single game
    outcome = runif(1)
    
    if(outcome <= prob){
      home_wins = home_wins+1
      west$Rating[west$Seed==r1a[[1]]] <- 50*(1-e1) + west$Rating[west$Seed==r1a[[1]]]
      west$Rating[west$Seed==r1a[[2]]] <- 50*(0-e1) + west$Rating[west$Seed==r1a[[2]]]
     # home_wins = home_wins+1
    }
    else{
      away_wins = away_wins+1
      west$Rating[west$Seed==r1a[[1]]] <- 50*(0-e1) + west$Rating[west$Seed==r1a[[1]]]
      west$Rating[west$Seed==r1a[[2]]] <- 50*(1-e1) + west$Rating[west$Seed==r1a[[2]]]
      
      #away_wins = away_wins+1
    }
    if(home_wins == 4){
      third_round[[west$Team[west$Seed==r1a[[1]]]]] = third_round[[west$Team[west$Seed==r1a[[1]]]]]+1
      one = r1a[[1]]
      break
    }
    if(away_wins == 4){
      third_round[[west$Team[west$Seed==r1a[[2]]]]] =  third_round[[west$Team[west$Seed==r1a[[2]]]]]+1
      one = r1a[[2]]
      break
    }
  }
  #2 vs 7
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    r3 = 10^(west$Rating[west$Seed==r1a[[3]]]/400)
    r2 = 10^(west$Rating[west$Seed==r1a[[4]]]/400)
    e1 = r3/(r3+r2)
    e2 = r2/(r3+r2)
    prob = 1/(1+10^((west$Rating[west$Seed==r1a[[4]]]-west$Rating[west$Seed==r1a[[3]]])/400))
    #sim single game
    outcome = runif(1)

    if(outcome <= prob){
      home_wins = home_wins+1
      west$Rating[west$Seed==2] <- 50*(1-e1) + west$Rating[west$Seed==r1a[[3]]]
      west$Rating[west$Seed==7] <- 50*(0-e1) + west$Rating[west$Seed==r1a[[4]]]
     # home_wins = home_wins+1
    }
    else{
      away_wins = away_wins+1
      west$Rating[west$Seed==2] <- 50*(0-e1) + west$Rating[west$Seed==r1a[[3]]]
      west$Rating[west$Seed==7] <- 50*(1-e1) + west$Rating[west$Seed==r1a[[4]]]
     # away_wins = away_wins+1
    }
    if(home_wins == 4){
      third_round[[west$Team[west$Seed==r1a[[3]]]]] = third_round[[west$Team[west$Seed==r1a[[3]]]]]+1
      two = r1a[[3]]
      break
    }
    if(away_wins == 4){
      third_round[[west$Team[west$Seed==r1a[[4]]]]] =  third_round[[west$Team[west$Seed==r1a[[4]]]]]+1
      two = r1a[[4]]
      break
    }
  }
  
  advance = list(one, two)
  return(advance)
}

#conference final round sim for the east using teams from second round sim 
conf_round_sim_east <- function(east, finals, conf1){
 
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    # print("hello")
    r3 = 10^(east$Rating[east$Seed==conf1[[1]]]/400)
    r2 = 10^(east$Rating[east$Seed==conf1[[2]]]/400)
    e1 = r3/(r3+r2)
    e2 = r2/(r3+r2)
    prob = 1/(1+10^((east$Rating[east$Seed==conf1[[2]]]-east$Rating[east$Seed==conf1[[1]]])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
     # home_wins = home_wins+1
      east$Rating[east$Seed==conf1[[1]]] <- 50*(1-e1) + east$Rating[east$Seed==conf1[[1]]]
      east$Rating[east$Seed==conf1[[2]]] <- 50*(0-e1) + east$Rating[east$Seed==conf1[[2]]]
      home_wins = home_wins+1
    }
    else{
      away_wins = away_wins+1
      east$Rating[east$Seed==conf1[[1]]] <- 50*(0-e1) + east$Rating[east$Seed==conf1[[1]]]
      east$Rating[east$Seed==conf1[[2]]] <- 50*(1-e1) + east$Rating[east$Seed==conf1[[2]]]
     # away_wins = away_wins+1
    }
    if(home_wins == 4){
      finals[[east$Team[east$Seed==conf1[[1]]]]] = finals[[east$Team[east$Seed==conf1[[1]]]]]+1
      one = conf1[[1]]
      break
    }
    if(away_wins == 4){
      finals[[east$Team[east$Seed==conf1[[2]]]]] =  finals[[east$Team[east$Seed==conf1[[2]]]]]+1
      one = conf1[[2]]
      break
    }
  }
  advance = list(one)
  return(advance)
}

#conference final round sim for the west using teams from second round sim 
conf_round_sim_west <- function(west, finals, conf1a){
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    # print("hello")
    r3 = 10^(west$Rating[west$Seed==conf1a[[1]]]/400)
    r2 = 10^(west$Rating[west$Seed==conf1a[[2]]]/400)
    e1 = r3/(r3+r2)
    e2 = r2/(r3+r2)
    prob = 1/(1+10^((west$Rating[west$Seed==conf1a[[2]]]-west$Rating[west$Seed==conf1a[[1]]])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
    #  home_wins = home_wins+1
      west$Rating[west$Seed==conf1a[[1]]] <- 50*(1-e1) + west$Rating[west$Seed==conf1a[[1]]]
      west$Rating[west$Seed==conf1a[[2]]] <- 50*(0-e1) + west$Rating[west$Seed==conf1a[[2]]]
      home_wins = home_wins+1
    }
    else{
    #  away_wins = away_wins+1
      west$Rating[west$Seed==conf1a[[1]]] <- 50*(0-e1) + west$Rating[west$Seed==conf1a[[1]]]
      west$Rating[west$Seed==conf1a[[2]]] <- 50*(1-e1) + west$Rating[west$Seed==conf1a[[2]]]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      finals[[west$Team[west$Seed==conf1a[[1]]]]] = finals[[west$Team[west$Seed==conf1a[[1]]]]]+1
      one = conf1a[[1]]
      break
    }
    if(away_wins == 4){
      finals[[west$Team[west$Seed==conf1a[[2]]]]] =  finals[[west$Team[west$Seed==conf1a[[2]]]]]+1
      one = conf1a[[2]]
      break
    }
  }
  advance = list(one)
  return(advance)
}

#finals sim using teams from east and west conf final sim 
finals_sim <- function(east, west, champion, e, w){
  
  home_wins = 0
  away_wins = 0
  for(i in 1:7){
    #set params
    # print("hello")
    r3 = 10^(east$Rating[east$Seed==e[[1]]]/400)
    r2 = 10^(west$Rating[west$Seed==w[[1]]]/400)
    e1 = r3/(r3+r2)
    e2 = r2/(r3+r2)
    prob = 1/(1+10^((west$Rating[west$Seed==w[[1]]]-east$Rating[east$Seed==e[[1]]])/400))
    #sim single game
    outcome = runif(1)
    if(outcome <= prob){
      home_wins = home_wins+1
      east$Rating[east$Seed==e[[1]]] <- 50*(1-e1) + east$Rating[east$Seed==e[[1]]]
      west$Rating[west$Seed==w[[1]]] <- 50*(0-e1) + west$Rating[west$Seed==w[[1]]]
      home_wins = home_wins+1
    }
    else{
      away_wins = away_wins+1
      east$Rating[east$Seed==e[[1]]] <- 50*(0-e1) + east$Rating[east$Seed==e[[1]]]
      west$Rating[west$Seed==w[[1]]] <- 50*(1-e1) + west$Rating[west$Seed==w[[1]]]
      away_wins = away_wins+1
    }
    if(home_wins == 4){
      champion[[east$Team[east$Seed==e[[1]]]]] = champion[[east$Team[east$Seed==e[[1]]]]]+1
      break
    }
    if(away_wins == 4){
      champion[[west$Team[west$Seed==w[[1]]]]] =  champion[[west$Team[west$Seed==w[[1]]]]]+1
      break
    }
  }
}



#add each team to the dictionaries for each round and set their value to 0. Each time a team makes a round in the sumlation the value is increased by 1 to gauge their chances to make the given round
second_round[[east$Team[east$Seed==1]]] <- 0
second_round[[east$Team[east$Seed==2]]] <- 0
second_round[[east$Team[east$Seed==3]]] <- 0
second_round[[east$Team[east$Seed==4]]] <- 0
second_round[[east$Team[east$Seed==5]]] <- 0
second_round[[east$Team[east$Seed==6]]] <- 0
second_round[[east$Team[east$Seed==7]]] <- 0
second_round[[east$Team[east$Seed==8]]] <- 0
third_round[[east$Team[east$Seed==1]]] <- 0
third_round[[east$Team[east$Seed==3]]] <- 0
third_round[[east$Team[east$Seed==2]]] <- 0
third_round[[east$Team[east$Seed==4]]] <- 0
third_round[[east$Team[east$Seed==5]]] <- 0
third_round[[east$Team[east$Seed==6]]] <- 0
third_round[[east$Team[east$Seed==7]]] <- 0
third_round[[east$Team[east$Seed==8]]] <- 0
finals[[east$Team[east$Seed==1]]] <- 0
finals[[east$Team[east$Seed==3]]] <- 0
finals[[east$Team[east$Seed==2]]] <- 0
finals[[east$Team[east$Seed==4]]] <- 0
finals[[east$Team[east$Seed==5]]] <- 0
finals[[east$Team[east$Seed==6]]] <- 0
finals[[east$Team[east$Seed==7]]] <- 0
finals[[east$Team[east$Seed==8]]] <- 0
champion[[east$Team[east$Seed==1]]] <- 0
champion[[east$Team[east$Seed==2]]] <- 0
champion[[east$Team[east$Seed==3]]] <- 0
champion[[east$Team[east$Seed==4]]] <- 0
champion[[east$Team[east$Seed==5]]] <- 0
champion[[east$Team[east$Seed==6]]] <- 0
champion[[east$Team[east$Seed==7]]] <- 0
champion[[east$Team[east$Seed==8]]] <- 0
second_round[[west$Team[west$Seed==1]]] <- 0
second_round[[west$Team[west$Seed==2]]] <- 0
second_round[[west$Team[west$Seed==3]]] <- 0
second_round[[west$Team[west$Seed==4]]] <- 0
second_round[[west$Team[west$Seed==5]]] <- 0
second_round[[west$Team[west$Seed==6]]] <- 0
second_round[[west$Team[west$Seed==7]]] <- 0
second_round[[west$Team[west$Seed==8]]] <- 0
third_round[[west$Team[west$Seed==1]]] <- 0
third_round[[west$Team[west$Seed==3]]] <- 0
third_round[[west$Team[west$Seed==2]]] <- 0
third_round[[west$Team[west$Seed==4]]] <- 0
third_round[[west$Team[west$Seed==5]]] <- 0
third_round[[west$Team[west$Seed==6]]] <- 0
third_round[[west$Team[west$Seed==7]]] <- 0
third_round[[west$Team[west$Seed==8]]] <- 0
finals[[west$Team[west$Seed==1]]] <- 0
finals[[west$Team[west$Seed==3]]] <- 0
finals[[west$Team[west$Seed==2]]] <- 0
finals[[west$Team[west$Seed==4]]] <- 0
finals[[west$Team[west$Seed==5]]] <- 0
finals[[west$Team[west$Seed==6]]] <- 0
finals[[west$Team[west$Seed==7]]] <- 0
finals[[west$Team[west$Seed==8]]] <- 0
champion[[west$Team[west$Seed==1]]] <- 0
champion[[west$Team[west$Seed==2]]] <- 0
champion[[west$Team[west$Seed==3]]] <- 0
champion[[west$Team[west$Seed==4]]] <- 0
champion[[west$Team[west$Seed==5]]] <- 0
champion[[west$Team[west$Seed==6]]] <- 0
champion[[west$Team[west$Seed==7]]] <- 0
champion[[west$Team[west$Seed==8]]] <- 0


#simulate the playoffs 10,000 times, using the functions written, passing the return each time to the next
for(i in 1:10000){
  r1 = first_round_sim_east(east, second_round)
  r1a = first_round_sim_west(west, second_round)
  conf1 = second_round_sim_east(east, third_round, r1)
  conf1a = second_round_sim_west(west, third_round, r1a)
  e = conf_round_sim_east(east, finals, conf1)
  w = conf_round_sim_west(west, finals, conf1a)
  finals_sim(east, west, champion, e, w)
}


#make the hash tables into dfs 
second=data.frame("Team" = c(keys(second_round)), 
                `Make 2nd Round` = c(values(second_round, keys=NULL)))
third=data.frame("Team" = c(keys(third_round)), 
                  `Make 3rd Round` = c(values(third_round, keys=NULL)))
finalss=data.frame("Team" = c(keys(finals)), 
                  `Make Conf. Finals` = c(values(finals, keys=NULL)))
champ=data.frame("Team" = c(keys(champion)), 
                  `Win Cup` = c(values(champion, keys=NULL)))

#make a new df with the new data telling each team's total makes in the 10,000 sims for each round
new <- final %>%
  left_join(second, by = c("Team" = "Team")) %>% 
  left_join(third, by = c("Team" = "Team")) %>% 
  left_join(finalss, by = c("Team" = "Team")) %>% 
  left_join(champ, by = c("Team" = "Team")) %>% 
  filter(Team == "Dallas Stars" | Team == "Vegas Golden Knights"| Team == "Carolina Hurricanes" | Team == "Florida Panthers")
  
#caluclate probabilities for each round
new$Make.2nd.Round <- round((new$Make.2nd.Round/10000),3)*100
new$Make.3rd.Round <- round((new$Make.3rd.Round/10000),3)*100
new$Make.Conf..Finals <- round((new$Make.Conf..Finals/10000),3)*100
new$Win.Cup <- round((new$Win.Cup/10000),3)*100

#change col names
colnames(new)[which(names(new) == "Make.2nd.Round")] <- "2nd Round"
colnames(new)[which(names(new) == "Make.3rd.Round")] <- "Conf. Finals"
colnames(new)[which(names(new) == "Make.Conf..Finals")] <- "Finals"
colnames(new)[which(names(new) == "Win.Cup")] <- "Win Cup"
new <- new %>% select(-`2nd Round`)

#plot the table
tab <- new %>%
  arrange(-`Win Cup`) %>% 
  #filter(Team == "Toronto Maple Leafs"| Team == "Florida Panthers") %>% 
  #filter(Team == "Edmonton Oilers"| Team == "Dallas Stars" | Team == "Vegas Golden Knights"| Team == "Carolina Hurricanes" | Team == "Toronto Maple Leafs"| Team == "Florida Panthers" | Team == "Seattle Kraken"| Team == "New Jersey Devils") %>% 
  gt() %>%
  gt_img_rows(columns = ` `) %>% 
  #gt_color_box(columns = Change, domain = -50:50, palette = c("darkred", "seashell", "darkgreen")) %>% 
  gt_theme_538() %>% 
  cols_align("center", contains("scale")) %>%
  data_color( # Update cell colors...
    columns = vars(`Points`), # ...for mean_len column
    colors = scales::col_numeric(
      palette = c("brown4", "white", "cadetblue4"), # Overboard colors! 
      domain = c(90,140), # Column scale endpoints
      alpha = .8
    )) %>% 
  data_color( # Update cell colors...
    columns = vars(Rating), # ...for mean_len column
    colors = scales::col_numeric(
      palette = c("brown4", "white", "cadetblue4"), # Overboard colors! 
      domain = c(1000,1609) # Column scale endpoints
    )) %>% 
  data_color( # Update cell colors...
    columns = vars(`Last 10 Gain`), # ...for mean_len column
    colors = scales::col_numeric(
      palette = c("brown4", "white", "cadetblue4"), # Overboard colors! 
      domain = c(57,332) # Column scale endpoints
    )) %>% 
  data_color( # Update cell colors...
    columns = vars(`Win Cup`), # ...for mean_len column
    colors = scales::col_numeric(
      palette = c("brown4", "white", "cadetblue4"), # Overboard colors! 
      domain = c(0,50) # Column scale endpoints
    )) %>% 
  data_color( # Update cell colors...
    columns = vars( `Finals`), # ...for mean_len column
    colors = scales::col_numeric(
      palette = c("brown4", "white", "cadetblue4"), # Overboard colors! 
      domain = c(0,93) # Column scale endpoints
    )) %>% 
  gt_plt_sparkline(`Trend`, palette = c("black", "black", "purple", "green", "lightgrey"), same_limit = T,) %>%
  cols_width(4 ~ px(85)) %>% 
  cols_hide(columns = c(Record, Points, `Conf. Finals`)) %>% 
  tab_header(title = "Stanley Cup Playoff Elo Rankings", subtitle = "via DRAGON Model") %>% 
  tab_source_note(
    source_note = "Data: hockeyR, By: Samuel DiSorbo @analytacist "
  ) %>% 
  tab_spanner(
    label = "Chances for...",
    columns = c(Finals, `Win Cup`)
  )





