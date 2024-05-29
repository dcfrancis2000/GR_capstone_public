################################################################################
# Plotting

kin_long <- kin3 %>% pivot_longer(cols = 5:10,names_to = 'METRIC',values_to = 'VALUE')
pdf('./plots_summaries/scraped_kinexon_metrics.pdf',width=10,height=8)
for(ath in unique(kin3$ATHLETE)) {
    kin_temp <- subset(kin_long,ATHLETE == ath)
    plt <- ggplot(data=kin_temp,aes(x=as.numeric(HALF),y=VALUE,color=DATE)) +
        facet_wrap(~METRIC,scales='free') + 
        geom_line(lwd=0.75) + geom_point() +  theme_classic() + xlab('HALF') + 
        scale_x_continuous(breaks=1:10) + 
        ggtitle(paste0('Kinexon Metrics for Player ',ath))
    print(plt)
}
graphics.off()

pdf('./plots_summaries/scraped_metrics_time.pdf',width=10,height=8)
for(ath in unique(kin3$ATHLETE)) {
    kin_temp <- subset(kin_long,ATHLETE == ath)
    plt <- ggplot(data = kin_temp,aes(x = TIME,y=VALUE,color=HALF)) + 
        facet_wrap(~METRIC,scales='free') + 
        geom_point() +  theme_classic() + xlab('TIME') + 
        ggtitle(paste0('Kinexon Metrics for Player ',ath,' Over Time of Measurement')) + 
        geom_smooth(method = 'lm',lwd=1,fill = 'lightgray')
    
    print(plt)
}
graphics.off()
