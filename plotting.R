################################################################################

# Plotting Script

################################################################################

# Setup

library(ggplot2)
data <- read.csv('./data/ML_data.csv')[,-1]
data_kinexon <- data[,1:10]
data_kinexon$ATHLETE <- as.numeric(as.factor(data_kinexon$ATHLETE))
kin_long <- data_kinexon %>% pivot_longer(cols = 5:10,
                                          names_to = 'METRIC',
                                          values_to = 'VALUE')

################################################################################

# Kinexon metrics over half by player

pdf('./plots_summaries/scraped_kinexon_metrics.pdf',width=10,height=8)
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

pdf('./plots_summaries/scraped_metrics_time.pdf',width=10,height=8)
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
