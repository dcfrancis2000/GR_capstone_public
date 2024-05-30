################################################################################

# Plotting Script

################################################################################

# Setup
cleannames <- function(x) sapply(x,function(y) switch(y,
                                                      'AAL'='AAL',
                                                      'PHYSIOLOAD'='Physio-load',
                                                      'SPEEDMAX'= 'Max. Speed',
                                                      snakecase::to_title_case(y)))
setwd("C:/Users/dcfra/GR_capstone_public")
library(ggplot2)
library(tidyverse)
data <- read.csv('./data/ML_data.csv')[,-1]
data_kinexon <- data[,1:10]
data_kinexon$ATHLETE <- as.numeric(as.factor(data_kinexon$ATHLETE))
kin_long <- data_kinexon %>% pivot_longer(cols = 5:10,
                                          names_to = 'METRIC',
                                          values_to = 'VALUE')
kin_long$METRIC <- cleannames(kin_long$METRIC)

################################################################################

# Kinexon metrics over half by player

pdf('./plots_summaries/metrics_by_game.pdf',width=10,height=8)
for(ath in unique(data_kinexon$ATHLETE)) {
    kin_temp <- subset(kin_long,ATHLETE == ath)
    plt <- ggplot(data=kin_temp,aes(x=as.numeric(HALF),y=VALUE,color=DATE)) +
        facet_wrap(~METRIC,scales='free') + 
        geom_line(lwd=0.75) + geom_point() +  theme_classic() + xlab('HALF') + 
        scale_x_continuous(breaks=1:10) + 
        ggtitle(paste0('Kinexon Metrics for Player ',ath))
    print(plt)
}
graphics.off()

################################################################################

# Kinexon metrics over time by player

pdf('./plots_summaries/by_time_half.pdf',width=10,height=8)
for(ath in unique(data_kinexon$ATHLETE)) {
    kin_temp <- subset(kin_long,ATHLETE == ath)
    plt <- ggplot(data = kin_temp,aes(x = TIME,y=VALUE,color=as.factor(HALF))) + 
        facet_wrap(~METRIC,scales='free') + 
        geom_point() +  theme_classic() + xlab('TIME') + 
        ggtitle(paste0('Kinexon Metrics for Player ',ath,' Over Time of Measurement')) + 
        geom_smooth(method = 'lm',lwd=1,fill = 'lightgray') +
        scale_color_discrete(name = 'HALF')
    print(plt)
}
graphics.off()

################################################################################
# line plots across games averaged by player

kin_avg <- kin_long %>% group_by(DATE,HALF,METRIC) %>% mutate_if(is.numeric,mean) %>% distinct()
png('./plots_summaries/metrics_by_game_averaged.png',width=8,height=5,units='in',res=200)
ggplot(data=kin_avg,aes(x=as.numeric(HALF),y=VALUE,color=DATE)) + 
    facet_wrap(~METRIC,scales='free') + 
    theme_classic() + geom_point() + geom_line() +
    scale_x_continuous(breaks=1:10) + xlab('Game Half') + 
    ylab('Metric Value') + ggtitle('Kinexon Metrics by Segment Over Date Averaged by Player') +
    scale_color_discrete(name='Date')
graphics.off()

################################################################################

# histograms of metrics across dates

png('./plots_summaries/kinexon_hists.png',width = 8, height = 6, units = 'in', res = 200)
ggplot(data = kin_avg,aes(x=VALUE)) + facet_wrap(~METRIC,scales='free') + 
    geom_histogram(aes(y=after_stat(density),color='black'),alpha=0.75,fill='white',color='black') + 
    geom_density(fill='lightgray',alpha=0.1) + theme_classic() + ylab('Density') + 
    xlab('Metric') + ggtitle('Distributions of Metrics Across Games Averaged by Player')

graphics.off()

################################################################################


