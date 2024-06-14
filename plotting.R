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
data <- read.csv('./data/fulldata.csv')[,-1]
data_kinexon <- data[,1:10]
#data_kinexon$ATHLETE <- as.numeric(as.factor(data_kinexon$ATHLETE))
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
png('./plots_summaries/metrics_by_game_averaged.png',width=8,height=5,units='in',res=1000)
ggplot(data=kin_avg,aes(x=as.numeric(HALF),y=VALUE,color=DATE)) + 
    facet_wrap(~METRIC,scales='free') + 
    theme_classic() + geom_point() + geom_line() +
    scale_x_continuous(breaks=1:10) + xlab('Game Half') + 
    ylab('Metric Value') + ggtitle('Kinexon Metrics by Segment Over Date Averaged by Player') +
    scale_color_discrete(name='Date')
graphics.off()

################################################################################

# histograms of metrics across dates

png('./plots_summaries/kinexon_hists.png',width = 8, height = 6, units = 'in', res = 1000)
ggplot(data = kin_avg,aes(x=VALUE)) + facet_wrap(~METRIC,scales='free') + 
    geom_histogram(aes(y=after_stat(density),color='black'),alpha=0.75,fill='white',color='black') + 
    geom_density(fill='lightgray',alpha=0.1) + theme_classic() + ylab('Density') + 
    xlab('Metric') + ggtitle('Distributions of Metrics Across Games Averaged by Player')

graphics.off()

################################################################################

# vald metrics across reps

vald_data <- data[-(3:10)]
vald_long <- vald_data %>%  pivot_longer(cols = 4:ncol(vald_data),
                                         names_to = 'METRIC',
                                         values_to = 'VALUE')
metric_names <- colnames(vald_data)[-(1:3)]

pdf('./plots_summaries/vald_metrics_reps.pdf',width=8,height = 8)
for(m in metric_names) {
    print(m)
    temp <- vald_data[c('ATHLETE','REP','DATE',m)]
    colnames(temp)[4] <- 'm'
    plt <- ggplot(data=temp,mapping=aes(x=as.numeric(REP),y=m,color=as.factor(DATE))) + facet_wrap(~ ATHLETE,scales='fixed') + 
        geom_point() + geom_line() + theme_classic() + scale_x_continuous(breaks=c(1,2)) + 
        xlab('Jump') + ylab('Metric Value') + ggtitle(paste0(to_title_case(m),' For Each Player Across Jumps')) + 
        labs(color='Date')
    print(plt)
}
graphics.off()

################################################################################
pdf('./plots_summaries/kinexon_by_vald.pdf',width=10,height = 8)
for(m in metric_names) {
    print(m)
    temp <- full_data[c('ATHLETE','DATE',m)]
    tempdata <- merge(kin_long,temp,by=c('ATHLETE','DATE'))
    colnames(tempdata)[7] <- 'm'
    plt <- ggplot(tempdata,aes(x = m, y=VALUE,color=as.factor(ATHLETE))) + facet_wrap(~METRIC,scales='free') +
        geom_point() + theme_classic() + 
        xlab('Vald Value') + ylab('Kinexon Values') + ggtitle(paste0('Kinexon Metrics by ',to_title_case(m),' Across Players')) + 
        labs(color='Player') + geom_smooth(method='lm',col='black')
    print(plt)
}
graphics.off()
################################################################################
library(patchwork)
athlete_names <- unique(full_data$ATHLETE)

pdf('./plots_summaries/kinexon_by_vald_acrossplayers.pdf',width=16,height = 8)
for(m in metric_names) {
    print(m)
    temp <- full_data[c('ATHLETE','DATE',m)]
    tempdata <- merge(kin_long,temp,by=c('ATHLETE','DATE'))
    colnames(tempdata)[7] <- 'm'
    for(i in 1:5) {
        if(i == 1) {
            plt <- ggplot(subset(tempdata,ATHLETE == athlete_names[i]),
                          aes(x = m, y=VALUE,color=TIME)) + facet_wrap(~METRIC,scales='free') +
                geom_point() + theme_classic() + 
                xlab('Vald Value') + ylab('Kinexon Values') + ggtitle(athlete_names[i]) + 
                labs(color='Time') + 
                geom_smooth(method='lm',col='black')
        }
        else plt <- plt + ggplot(subset(tempdata,ATHLETE == athlete_names[i]),
                aes(x = m, y=VALUE,color=TIME)) + facet_wrap(~METRIC,scales='free') +
                geom_point() + theme_classic() + 
                ggtitle(athlete_names[i]) + 
                labs(color='Time') + xlab('') + ylab('') + 
                scale_color_continuous(breaks = NULL) + 
                geom_smooth(method='lm',col='black')
    }
    print(plt + plot_annotation(title = paste0(m,' By Kinexon Metrics Across Players')))    
}
graphics.off()
