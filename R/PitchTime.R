library(DBI)
library(tidyverse)
library(baseballr)
library(pitchRx)
library(lme4)
library(mgcv)
library(lubridate)
library(dbConnect)
library(broom)
#scrape(start = "2017-01-01", end = "2017-12-01", connect = db$con)
db <- src_sqlite("/Users/Sam/Desktop/Baseball Data/pitchRx.sqlite3")

setwd("~/Desktop/Projects/PitchTime")
#Retrosheet event files
dbRetro=dbConnect(MySQL(),user="root",
                  host="localhost",
                  dbname="retrosheet",
                  password="root",
                  unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")

#Retrosheet Game Logs
game_logs = fetch(dbSendQuery(dbRetro, paste0("select * from games_bck where YEAR_ID=2015 OR YEAR_ID=2016 OR YEAR_ID=2017")),n = -1) %>%
    mutate(gameday_link = paste0('gid_',substr(GAME_ID, 4,7),'_',substr(GAME_ID, 8,9),'_',substr(GAME_ID, 10,11),'_',
                                 tolower(AWAY_TEAM_ID),'mlb', '_',tolower(substr(GAME_ID, 1,3)),'mlb','_', pmax(as.integer(substr(GAME_ID,12,12)),1)))

#IDS: MLB to Name
ids =select(read.csv('https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv'),
            key_mlbam, key_retro, name_last ,name_first ) %>%
    mutate(Name = paste(name_first, name_last)) %>%
    select(-name_last, -name_first)%>%
    filter(!(is.na(key_mlbam)|key_mlbam==''|is.na(key_retro)|key_retro==''))

# Gather Data from Pitchf/x
PITCH <- collect(filter(tbl(db, 'pitch'),gameday_link %in% game_logs$gameday_link),n=Inf) %>%
    mutate(event_num = as.integer(event_num), num = as.integer(num), season = as.integer(substr(gameday_link, 5, 8)))

ATBAT <- collect(filter(tbl(db, 'atbat'),gameday_link %in% game_logs$gameday_link),n=Inf)%>%
    mutate(event_num = as.integer(event_num), num = as.integer(num),season = as.integer(substr(gameday_link, 5, 8)))

ACTION<- collect(filter(tbl(db, 'action'),gameday_link %in% game_logs$gameday_link),n=Inf)%>%
    mutate(event_num = as.integer(event_num), num = as.integer(num), season = as.integer(substr(gameday_link, 5, 8)))

PO <- collect(filter(tbl(db, 'po'),gameday_link %in% game_logs$gameday_link),n=Inf)%>%
    mutate(event_num = as.integer(event_num), num = as.integer(num), season = as.integer(substr(gameday_link, 5, 8)))

# RUNNER <- collect(filter(tbl(db, 'runner'),gameday_link %in% game_logs$gameday_link),n=Inf)%>%
#     mutate(event_num = as.integer(event_num),num = as.integer(num))


AllEvents = bind_rows(
    #All actions (Injuries, Defensive Subs, Reviews, Ejections, etc)
    select(ACTION, url, gameday_link, num, event_num, des, a_event=event, tfs_zulu) %>% 
                          mutate(type = 'aciton',
                                 time = as_datetime(gsub('T|Z',' ',tfs_zulu))),
    #All Pick off attampts
    select(PO, url, gameday_link, num, event_num, des) %>% mutate(type= 'po'),
    #All pitches
    PITCH %>% mutate(type = 'pitch') %>%
        mutate(time = as_datetime(gsub('T|Z',' ',tfs_zulu))) %>%
        #For each PA calculate pace between pitches
        group_by(url, num) %>%
        arrange(event_num) %>%
        mutate(pace = as.numeric(time-lag(time))) %>%
        ungroup
    ) %>%
    #Arrange by game, play number, type of play
    arrange(url, gameday_link, num, event_num, desc(type)) %>%
    group_by(url, gameday_link,num) %>%
    #Get the previous event and description
    mutate(prev_event = lag(a_event), prev_des = lag(des)) %>%
    ungroup



EligiblePitch = filter(AllEvents, 
                       #dont include events without pace
                       !is.na(pace), 
                       #take out plays without prev_event (i.e., event was not a pitch)
                       is.na(prev_event),
                       #take out remaining pickoffs
                       !grepl('Pickoff', prev_des),
                       #only pace above 0 (some data errors)
                       pace>0,
                       #some really long pace (rain delays etc)
                       #180 seconds is the 99.98 percentile
                       pace<180)%>%
    #Join AB data
    left_join(ATBAT %>% 
                  rename(event_num_b = event_num)%>%
                  group_by(url, inning, inning_side) %>% 
                  arrange(num)%>%
                  mutate(outs = lag(o, default = 0)), by = c('url','num','inning_side','inning','gameday_link')) %>%
    #A few incorrect plays labeled w/ 3 outs
    filter(outs!=3)%>%
    #Calculate a few necessary columns
    mutate(inning =ifelse(as.integer(inning)<9, inning, "9+"),
           home_team_runs = as.integer(home_team_runs),
           away_team_runs = as.integer(away_team_runs),
           bases = gsub("0", "_", paste0(as.integer(!is.na(on_1b)),as.integer(!is.na(on_2b))*2,as.integer(!is.na(on_3b))*3)),
           score_diff = ifelse(inning_side=='top', home_team_runs-away_team_runs, away_team_runs-home_team_runs),
           h_score_diff = home_team_runs-away_team_runs, 
           #standardize the previous pitch into similar categories
           prev_des = recode(prev_des, 'Automatic Ball'='Ball',
                             'Missed Bunt' = 'Swinging Strike',
                             'Pitchout'='Ball',
                             'Foul Tip' = 'Swinging Strike',
                             'Swinging Strike (Blocked)'='In Dirt',
                             'Ball In Dirt'='In Dirt'),
           season = as.integer(substr(gameday_link,5,8))) %>%
    select(-batter_name, -pitcher_name) %>%
    #Get player names
    left_join(ids %>% select(batter = key_mlbam, batter_name = Name, -key_retro)) %>%
    left_join(ids %>% select(pitcher= key_mlbam, pitcher_name = Name, -key_retro))

#Umpires?
#EligiblePitch = left_join(EligiblePitch, select(game_logs, gameday_link, BASE4_UMP_ID))

#Current pace model
pace_model = function(df){
    lmer(pace ~ factor(outs)+bases + abs(score_diff) + abs(score_diff):(score_diff>=0)+ 
             inning +inning_side + prev_des + count + 
             (1|batter) + (1 |pitcher) , data=df)
}

#Map pace_model to data for each season
Results = group_by(EligiblePitch, season)%>%
    nest()%>%
    mutate(model = map(data,pace_model))

#Function to apply to lme4 model to extract rand effects
get_rand_effects = function(mod){
    rand =  ranef(mod)
    
    return(list(batter = data.frame(batter = as.integer(rownames(rand$batter)),
                                    cpace = rand$batter[,1]),
                pitcher = data.frame(pitcher = as.integer(rownames(rand$pitcher)),
                                     cpace = rand$pitcher[,1])))
}

#Extract random effects
Results = mutate(Results, rand_eff = map(model, get_rand_effects))

#Expected pitches per PA
ex_pitch_pa = left_join(group_by(PITCH, season) %>% summarise(p = n()),
                        group_by(ATBAT, season)%>% summarise(ab = n())) %>%
    mutate(pppa = p/ab)

#Ex PA per game
ex_pa_game = group_by(ATBAT, season) %>% summarise(pa_game = n()/n_distinct(url)/18) 


#Batter Results
batter_pace = group_by(EligiblePitch, season, batter_name, batter) %>% 
    #Pace and times between pitches
    summarise(pace = mean(pace),between_pitches = n())%>%
    #join approx PA and games
    left_join(ATBAT %>% 
                  group_by(season, batter) %>% 
                  summarise(PA = n_distinct(url, num), 
                            games = n_distinct(url)))%>%
    #join random effects
    left_join( do.call(rbind, lapply(1:3, function(i) Results$rand_eff[[i]]$batter%>%mutate(season = Results$season[i])))) %>%
    #join expected measures
    left_join(ex_pitch_pa)%>%
    left_join(ex_pa_game)%>%
    ungroup%>%
    #calculate min added per plate appearance
    mutate(min_added_pa = between_pitches*cpace/60/PA,
           #expected min added per plate appearance (control for Pitches/PA)
           exp_min_added_pa = cpace*(pppa-1)/60,
           #expected min added per game (control for # of PA)
           exp_min_added_game = pa_game*exp_min_added_pa) 

pitcher_pace =  group_by(EligiblePitch, season, pitcher_name, pitcher) %>% 
    #Pace and times between pitches
    summarise(pace = mean(pace),between_pitches = n())%>%
    #join approx PA and games
    left_join(ATBAT %>% 
                  group_by(season, pitcher) %>% 
                  summarise(PA = n_distinct(url, num), 
                            games = n_distinct(url)))%>%
    #join random effects
    left_join( do.call(rbind, lapply(1:3, function(i) Results$rand_eff[[i]]$pitcher%>%mutate(season = Results$season[i])))) %>%
    #join expected measures
    left_join(ex_pitch_pa)%>%
    left_join(ex_pa_game)%>%
    ungroup%>%
    #calculate min added per plate appearance
    mutate(min_added_pa = between_pitches*cpace/60/PA,
           #expected min added per plate appearance (control for Pitches/PA)
           exp_min_added_pa = cpace*(pppa-1)/60,
           #expected min added per game (control for # of PA)
           exp_min_added_game = pa_game*exp_min_added_pa) 

#Save Data
saveRDS(pitcher_pace,'data/pitcher_pace.RDS')
saveRDS(batter_pace,'data/batter_pace.RDS')
saveRDS(EligiblePitch,'data/pace_granular.RDS')
saveRDS(Results, 'data/result_tibble.RDS')


#### Get Game Time #####
game_logs = fetch(dbSendQuery(dbRetro, paste0("select * from games_bck where YEAR_ID IN (",paste(2010:2017, collapse=',') ,")")),n = -1) %>%
    mutate(gameday_link = paste0('gid_',substr(GAME_ID, 4,7),'_',substr(GAME_ID, 8,9),'_',substr(GAME_ID, 10,11),'_',
                                 tolower(AWAY_TEAM_ID),'mlb', '_',tolower(substr(GAME_ID, 1,3)),'mlb','_', pmax(as.integer(substr(GAME_ID,12,12)),1)))

PITCH2 <- collect(filter(select(tbl(db, 'pitch'), tfs_zulu, gameday_link),gameday_link %in% game_logs$gameday_link),n=Inf) %>%
    mutate( season = as.integer(substr(gameday_link, 5, 8)))

#Calculate game time: time between first pitch and last
game_time = filter(PITCH2, !is.na(time)) %>%
    group_by(gameday_link, season) %>% 
    summarise(time = max(time, na.rm=T)-min(time, na.rm=T)) 

saveRDS(game_time, 'data/game_time.RDS')