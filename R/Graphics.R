library(tidyverse)
library(ggthemes)
library(ggridges)
library(scales)
library(gridExtra)

#Load data
batter_pace=readRDS('data/batter_pace.RDS')
pitcher_pace = readRDS('data/pitcher_pace.RDS')
pace_data = readRDS('data/pace_granular.RDS')
pitches_total = read.csv('data/fg_pitchinfo.csv') %>%
    filter(Season>=2008)
Results = readRDS('data/result_tibble.RDS')

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


#Game time distribution
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
    ggtitle("Distribution of Game Times (2010-2017)")+
    labs(caption = 'Source: PITCHf/x Data')


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
    ylim(0, 30)+
    labs(caption = "Source: PITCHF/x Data")

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
    geom_line(aes(x = inning, y = pace, colour = inning_side, group = inning_side), size = 1.5)+
    theme_fivethirtyeight()+
    scale_x_discrete("\nInning")+
    scale_y_continuous("Pace\n", label = to_sec)+
    scale_color_discrete("")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14)) +
    ggtitle("Average Pace by Inning (2017)")+
    labs(caption = "Source: PITCHF/x Data")


#### Pace by pitch type #####
pitch_pace = filter(pace_data, season==2017) %>%
    group_by(prev_des)%>%
    summarise(pace = mean(pace))%>%
    mutate(prev_des = ifelse(prev_des=='Foul (Runner Going)','Foul\n(Runner Going)', prev_des))%>%
    arrange(pace)%>%
    mutate(prev_des = factor(prev_des, levels = prev_des, ordered = T))

#Bar graph
ggplot(pitch_pace)+
    geom_bar(aes(x = prev_des, y = pace), fill = "#014d64", stat = 'identity')+
    theme_fivethirtyeight()+
    scale_fill_economist()+
    scale_y_continuous("Pace\n", labels = to_sec)+
    scale_x_discrete("\n Previous Pitch")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14)) +
    guides(fill=FALSE)+
    ggtitle("Pace Given Previous Pitch Result (2017)")+
    labs(caption = "Source: PITCHF/x Data")


#### Pace by count #####
count_pace = filter(pace_data, season==2017) %>%
    group_by(count)%>%
    summarise(pace = mean(pace)) %>%
    separate(count, c('Balls', 'Strikes'), '-')

#Bar graph
ggplot(count_pace)+
    geom_tile(aes(x = Strikes, y = Balls, fill = pace))+
    #geom_text(aes(x = Strikes, y = Balls, label = to_sec(round(pace,1))))+
    theme_fivethirtyeight()+
    scale_fill_distiller('Pace     ', palette = 'RdBu', labels = to_sec, breaks = c(20,23,26))+
    scale_y_discrete("Balls\n")+
    scale_x_discrete("\n Strikes")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14),
          legend.key.width = unit(1, "cm")) +
    ggtitle("Pace by Count (2017)")+
    labs(caption = "Source: PITCHF/x Data")


#### Pace by Score ####
score_pace = filter(pace_data, season == 2017) %>%
    group_by(score_diff )%>%
    summarise(pace = mean(pace), n = n())

filter(pace_data, season == 2017) %>%
    ggplot()+
    stat_smooth(aes(x = score_diff, y = pace ), method = 'lm', formula = 'y~poly(x, 3)')+
    geom_point(data = score_pace , aes(x = score_diff, y = pace, size =n))+
    theme_fivethirtyeight()+
    scale_color_manual("", values = c('black','black'))+
    scale_size_continuous("     Pitches\n(at each score diff.)", labels = function(x) paste0(floor(x/1000),'k'))+
    scale_y_continuous("Pace\n", labels = to_sec)+
    scale_x_continuous("\nHome Score Difference")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 14)) +
    ggtitle("Pace by Score Differential (2017)")+
    labs(caption = "Source: PITCHF/x Data")


#### Year to Year Correlation cPace vs Pace ####

# Batters
qual_batters = filter(batter_pace, PA>100)

year_to_year_b = bind_rows(left_join(filter(qual_batters, season==2015)  %>%
              select(batter_name, cpace1=cpace, pace1=pace), 
          filter(qual_batters, season==2016)  %>%
              select(batter_name, cpace2=cpace, pace2=pace)),
          left_join(filter(qual_batters, season==2016)  %>%
                        select(batter_name, cpace1=cpace, pace1=pace), 
                    filter(qual_batters, season==2017)  %>%
                        select(batter_name, cpace2=cpace, pace2=pace)))%>%
    filter(!is.na(cpace2))

#plot pace         
pace_corr_b = ggplot(year_to_year_b,aes(x = pace1, y = pace2))+
    geom_point()+
    geom_smooth(method = 'lm')+
    geom_text(x =22, y = 30, label = paste('Rsq = ',round(summary(lm(pace2~pace1, year_to_year_b))$r.squared,3)), size = 5)+
    scale_x_continuous("\nPace in Year 1",limits = c(18, 32), breaks =seq(18,32,2))+
    scale_y_continuous("Pace in Year 2\n",limits = c(18, 32),breaks =seq(18,32,2))+
    ggtitle('Batter Pace Year 1 vs. Year 2', subtitle = 'At Least 100 PA (2015-2017)')+
    theme_fivethirtyeight()+
    theme(text = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 12))

#plot cpace
cpace_corr_b = ggplot(year_to_year_b,aes(x = cpace1, y = cpace2))+
    geom_point()+
    geom_smooth(method = 'lm')+
    geom_text(x =-1, y =6, label = paste('Rsq = ',round(summary(lm(cpace2~cpace1, year_to_year_b))$r.squared,3)), size = 5)+
    scale_x_continuous("\ncPace in Year 1",limits = c(-4, 7), breaks =seq(-4,6,2))+
    scale_y_continuous("cPace in Year 2\n",limits = c(-4, 7),breaks =seq(-4,6,2))+
    ggtitle('Batter cPace Year 1 vs. Year 2', subtitle = 'At Least 100 PA (2015-2017)')+
    theme_fivethirtyeight()+
    theme(text = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 12))
          

# Pitchers
qual_pitchers = filter(pitcher_pace, PA>100)

year_to_year_p = bind_rows(left_join(filter(qual_pitchers, season==2015)  %>%
                                       select(pitcher_name, cpace1=cpace, pace1=pace), 
                                   filter(qual_pitchers, season==2016)  %>%
                                       select(pitcher_name, cpace2=cpace, pace2=pace)),
                         left_join(filter(qual_pitchers, season==2016)  %>%
                                       select(pitcher_name, cpace1=cpace, pace1=pace), 
                                   filter(qual_pitchers, season==2017)  %>%
                                       select(pitcher_name, cpace2=cpace, pace2=pace)))%>%
    filter(!is.na(cpace2))

#plot pace
pace_corr_p = ggplot(year_to_year_p,aes(x = pace1, y = pace2))+
    geom_point()+
    geom_smooth(method = 'lm')+
    geom_text(x =22, y = 30, label = paste('Rsq = ',round(summary(lm(pace2~pace1, year_to_year_p))$r.squared,3)), size = 5)+
    scale_x_continuous("\nPace in Year 1",limits = c(18, 32), breaks =seq(18,32,2))+
    scale_y_continuous("Pace in Year 2\n",limits = c(18, 32),breaks =seq(18,32,2))+
    ggtitle('Pitcher Pace Year 1 vs. Year 2', subtitle = 'At Least 100 Batters Faced (2015-2017)')+
    theme_fivethirtyeight()+
    theme(text = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 12))

#plot cpace
cpace_corr_p = ggplot(year_to_year_p,aes(x = cpace1, y = cpace2))+
    geom_point()+
    geom_smooth(method = 'lm')+
    geom_text(x =-1, y =6, label = paste('Rsq = ',round(summary(lm(cpace2~cpace1, year_to_year_p))$r.squared,3)), size = 5)+
    scale_x_continuous("\ncPace in Year 1",limits = c(-5, 7), breaks =seq(-4,7,2))+
    scale_y_continuous("cPace in Year 2\n",limits = c(-5, 7),breaks =seq(-4,7,2))+
    ggtitle('Pitcher cPace Year 1 vs. Year 2', subtitle = 'At Least 100 Batters Faced (2015-2017)')+
    theme_fivethirtyeight()+
    theme(text = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 12))

#combine
grid.arrange(pace_corr_b, cpace_corr_b,pace_corr_p,cpace_corr_p, nrow = 2) 


year_to_year = bind_rows(year_to_year_b%>%mutate(type='Batter'),
                         year_to_year_p%>%mutate(type='Pitcher')) %>%
    gather(Metric1, Year1, cpace1, pace1)%>%
    gather(Metric2, Year2, cpace2, pace2) %>%
    mutate(Metric1 = gsub('1','',Metric1),
           Metric2 = gsub('2','',Metric2)) %>%
    filter(Metric1==Metric2)

ggplot(year_to_year,aes(x = Year1, y = Year2))+
    geom_point()+
    geom_smooth(method = 'lm')+
    facet_grid(type~Metric1,scales = 'free_y' )

### Top 10/ Bottom 10 Exp Hours / Season 
hrs_season = bind_rows(rename(batter_pace, name=batter_name)%>% mutate(type='Batters'),
          rename(pitcher_pace, name=pitcher_name)%>% mutate(type='Pitchers'))%>%
filter(season ==2017, PA>400)%>% 
    group_by(type)%>%
    arrange(cpace)%>%
    filter(row_number()<=10 | (row_number()<=n() & row_number()>=(n()-9)))%>% 
    ungroup%>%
    mutate(name = factor(name, levels = name, ordered = T),
           positive = cpace<0)

ggplot(hrs_season)+
    geom_bar(aes(x = name, y = exp_min_added_pa*10, fill = positive), stat = 'identity') +
    coord_flip()+
    scale_y_continuous("\nxHSAA\n\n(Based on 600 PA Season and Lg Avg Pitch/PA)", labels = function(x) paste(x,'hrs'))+
    scale_x_discrete("Player\n")+
    facet_wrap(~type, scales = 'free')+
    theme_fivethirtyeight()+
    guides(fill = F)+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 12))+
    ggtitle("Expected Hours Added Per Season Above Average (xHSAA)",
            subtitle = "Top/Bottom 10 players with at least 400 PA or BF")


#### Rule Effects ####

# No Runners 18 sec clock
#https://stackoverflow.com/questions/46516759/fixed-fill-for-different-sections-of-a-density-plot-with-ggplot

#Create density values for no runners on base:
#  Keep plot values less than 1 min and >0
#  Adjust pace (-5 sec) for time to throw back to pitcher
dens_norunner = density(filter(pace_data, season==2017, pace<60,pace>5, bases=='___',
                               prev_des %in% c('Ball','Swinging Strike','Called Strike'))$pace-5, 
                        n = 2^10, adj = 2)[c('x','y')]

dens_norunner = data.frame(dens_norunner)%>% 
    #create sections to color (18 min pitch clock)
    mutate(section = cut(x, breaks = c(-Inf, 18, Inf)))%>%
    group_by(section) %>% 
    #calculate total percentage in section
    mutate(prob=round(sum(y)*mean(diff(x))*100) ,
            prob_char = paste0(prob,"%"),
            max_prob = max(prob)) %>%
    ungroup%>%
    #Label violations with percentage
    mutate(Violation = ifelse(!grepl('-Inf', section),paste0('Clock Violation (',max_prob,'%)'),
                              paste0('No Violation (',max_prob,'%)')))
          

no_runner = ggplot(dens_norunner, aes(x, y, fill=Violation)) +
    geom_area()+
    scale_fill_discrete("")+
    scale_x_continuous("\nPace", labels = to_sec)+
    scale_y_continuous("")+
    theme_fivethirtyeight()+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 12),
          axis.text.y = element_blank(),
          legend.position = c(.8,.5),
          legend.direction = 'vertical')+
    ggtitle("No Runners on Base: 18 Second Pitch Clock")

# Runners 20 sec clock
#https://stackoverflow.com/questions/46516759/fixed-fill-for-different-sections-of-a-density-plot-with-ggplot
#Calculate density values 

#Create density values for runners on base:
#  Keep plot values less than 1 min and >0
#  Adjust pace (-5 sec) for time to throw back to pitcher
dens_runner = density(filter(pace_data, season==2017, pace<60,pace>5, bases!='___',
                             prev_des %in% c('Ball','Swinging Strike','Called Strike'))$pace-5, 
                      n = 2^10, adj = 2)[c('x','y')]

dens_runner = data.frame(dens_runner)%>% 
    #create sections to color (20 min pitch clock)
    mutate(section = cut(x, breaks = c(-Inf, 20, Inf)))%>%
    group_by(section) %>% 
    #calculate total percentage in section
    mutate(prob=round(sum(y)*mean(diff(x))*100) ,
           prob_char = paste0(prob,"%"),
           max_prob = max(prob)) %>%
    ungroup%>%
    #Label violations with percentage
    mutate(Violation = ifelse(!grepl('-Inf', section),paste0('Clock Violation (',max_prob,'%)'),
                              paste0('No Violation (',max_prob,'%)')))

runner = ggplot(dens_runner, aes(x, y, fill=Violation)) +
    geom_area()+
    scale_fill_discrete("")+
    scale_x_continuous("\nPace" , labels = to_sec)+
    scale_y_continuous("")+
    theme_fivethirtyeight()+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          title = element_text(size = 12),
          axis.text.y = element_blank(),
          legend.position = c(.8,.5),
          legend.direction = 'vertical')+
    ggtitle("Runners on Base: 20 Second Pitch Clock")+
    labs(caption = "Source: PITCHF/x Data")

#Combine plots into one
grid.arrange(no_runner, runner)
