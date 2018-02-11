library(tidyverse)
library(ggthemes)
library(scales)

#Load data
batter_pace=readRDS('data/batter_pace.RDS')
pitcher_pace = readRDS('data/pitcher_pace.RDS')
pace_data = readRDS('data/pace_granular.RDS')
pitches_total = read.csv('data/fg_pitchinfo.csv') %>%
    filter(Season>=2008)


#Time function
to_time = function(x) paste0(floor(x),':',str_pad(round(60*(x-floor(x))),width = 2, pad = '0'))
to_sec =  function(x) paste(x, 'sec')

#Pitches over time
ggplot(pitches_total)+
    geom_line(aes(x = as.factor(Season), y = TBF, group = 1), size = 1.5)+
    theme_fivethirtyeight()+
    scale_y_continuous(labels  = comma)

#Pitches per plate
ggplot(pitches_total %>% mutate(PpBF = Pitches/TBF))+
    geom_line(aes(x = as.factor(Season), y = PpBF, group = 1), size = 1.5)+
    theme_fivethirtyeight()+
    scale_y_continuous(labels  = comma)

#Overall metrics
pitches_total %>% 
    mutate(Pace_ = Pace*first(TBF)*(first(Pitches/TBF)-1)/3600/G,
           PpBF = first(Pace)*first(TBF)*(Pitches/TBF-1)/3600/G,
           BF= first(Pace)*TBF*(first(Pitches/TBF)-1)/3600/G,
           Raw = Pace*TBF*(Pitches/TBF-1)/3600/G) %>%
    gather(Metric, Value,-Season,-G, -IP, -Pace, -Pitches, -TBF, -Raw) %>% 
    mutate(Value = Value- first(Value)) %>% spread(Metric, Value)%>%View()


#Batter and pitcher pace
ggplot(batter_pace %>% filter(PA>400))+
    geom_density(aes(exp_min_added_game , fill = factor(season)))


game_time = readRDS('data/game_time.RDS')

plot_time = game_time %>% filter(!is.na(time))%>%
    group_by(season)%>%
    mutate(Avg = mean(time), Med = median(time))%>%
    ungroup%>%
    filter(time<5, time>1.5)%>%
    mutate( Season = factor(season, levels = 2017:2010, ordered = T ),
            abs_diff = abs(time-Avg))

ggplot(plot_time)+
    geom_density_ridges_gradient(aes(x=time, y =Season, fill = Med))+
    scale_fill_distiller("Median \nGame Time\n", label =to_time, palette='RdBu')+
    theme_fivethirtyeight()+
    scale_x_continuous("\nGame Time", labels = to_time)+
    scale_y_discrete("Season\n")+
    theme(legend.position = 'right',legend.direction = 'vertical',
          text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14))+
    ggtitle("Distribution of Game Times (2010-2017)")


#### State Pace Graphs ####
state_order = c('___','1__','_2_','12_', '__3','1_3','_23','123')

state_pace = filter(pace_data, season==2017)%>% 
    group_by(bases = factor(bases, levels = state_order, ordered = T),
             outs= factor(outs))%>% 
    summarise(pace=mean(pace))

#Pie charts
ggplot(state_pace)+
    geom_bar(aes(x =1, y = pace, fill = pace), stat ='identity')+
    facet_grid(bases~outs)+
    coord_polar(theta = "y")+
    scale_y_continuous("", minor_breaks = c(0,1))+
    scale_x_continuous("")+
    scale_fill_distiller("Pace\n", palette='RdBu', labels = to_sec,
                         breaks= c(20,22,24,26,28,30))+
    theme(axis.text  = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 16),
          legend.box.margin=margin(0,0,150,80),
          legend.key.width = unit(.75, 'cm'),
          legend.key.height = unit(1,'cm'),
          panel.grid.minor = element_line(color='gray'),
          panel.grid.major = element_line(color='gray'))+
    ylim(0, 30)

#Pie legend
filter(pace_data, season==2017)%>%summarise(pace=mean(pace))%>% ggplot()+ geom_bar(aes(x =1, y = pace), fill = 'black', alpha = .3, stat ='identity')+coord_polar(theta = "y")+
    scale_y_continuous("", minor_breaks = c(0,1))+
    scale_x_continuous("")+
    theme(axis.text  = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 16),
          legend.box.margin=margin(0,0,150,80),
          legend.key.width = unit(.75, 'cm'),
          legend.key.height = unit(1,'cm'),
          panel.grid.minor = element_line(color='gray'),
          panel.grid.major = element_line(color='gray'))+
    ylim(0, 30)


##### Pace by inning and bottom top #####
inning_pace= filter(pace_data, season==2017) %>%
    group_by(inning, inning_side )%>%
    summarise(pace = mean(pace))

#Line graph
ggplot(inning_pace)+
    geom_line(aes(x = inning, y = pace, colour = inning_side, group = inning_side), size = 1)+
    theme_fivethirtyeight()+
    scale_x_discrete("\nInning")+
    scale_y_continuous("Pace\n", label = to_sec)+
    scale_color_discrete("")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14)) +
    ggtitle("Average Pace by Inning")


#### Pace by pitch type #####
pitch_pace = filter(pace_data, season==2017) %>%
    group_by(prev_des)%>%
    summarise(pace = mean(pace))%>%
    mutate(prev_des = ifelse(prev_des=='Foul (Runner Going)','Foul\n(Runner Going)', prev_des))

#Bar graph
ggplot(pitch_pace)+
    geom_bar(aes(x = prev_des, y = pace), fill = "#014d64", stat = 'identity')+
    theme_fivethirtyeight()+
    scale_fill_economist()+
    scale_y_continuous("Pace\n")+
    scale_x_discrete("\n Previous Pitch")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14)) +
    guides(fill=FALSE)+
    ggtitle("Pace Given Previous Pitch Result")


#### Pace by count #####
count_pace = filter(pace_data, season==2017) %>%
    group_by(count)%>%
    summarise(pace = mean(pace)) %>%
    separate(count, c('Balls', 'Strikes'), '-')

#Bar graph
ggplot(count_pace)+
    geom_tile(aes(x = Strikes, y = Balls, fill = pace))+
    geom_text(aes(x = Strikes, y = Balls, label = to_sec(round(pace,1))))+
    theme_fivethirtyeight()+
    scale_fill_distiller('Pace  ', palette = 'RdBu')+
    scale_y_discrete("Balls\n")+
    scale_x_discrete("\n Strikes")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14),
          legend.key.width = unit(1, "cm")) +
    ggtitle("Pace by Count")


#### Pace by Score ####
score_pace = filter(pace_data, season == 2017) %>%
    group_by(score_diff )%>%
    summarise(pace = mean(pace))

filter(pace_data, season == 2017) %>%
    ggplot()+
    stat_smooth(aes(x = score_diff, y = pace), method = 'lm', formula = 'y~poly(x, 3)')+
    geom_point(data = score_pace , aes(x = score_diff, y = pace, color = 'Average Pace\n(at each score)'))+
    theme_fivethirtyeight()+
    scale_color_manual("", values = c('black','black'))+
    scale_y_continuous("Pace\n")+
    scale_x_continuous("\nHome Score Difference")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14)) +
    ggtitle("Pace by Count (2017)")
