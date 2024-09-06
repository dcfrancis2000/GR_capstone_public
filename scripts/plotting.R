uncensored <- vald_final$ATHLETE
censored_name <- as.factor(as.numeric(factor(vald_final$ATHLETE)))
monthday <- format(as.Date(vald_final$DATE),format = '%m-%d')
gameticks <- as.numeric(factor(as.Date(vald_final$DATE)))
vald_metrics <- c('ECCENTRIC_MEAN_FORCE','RSI_MODIFIED','FLIGHT_TIME')
clean_vald_metrics <- c('Eccentric Mean Force (N)','RSI-Modified','Flight Time (s)')

################################################################################

vald_boxplot <- function(index,censored = TRUE) {
    name <- vald_metrics[index]
    metric_vec <- as.matrix(vald_final[name])
    ggplot(data = vald_final,
           aes(x = if(censored) censored_name else uncensored,color = as.factor(REP))) + 
        geom_boxplot(aes(y = metric_vec)) + theme_classic() +
        xlab('Player') + ylab(clean_vald_metrics[index]) + 
        scale_color_discrete(name = 'Jump') + 
        if(index != 1) theme(legend.position = 'none') else theme(legend.position = 'top')
}
boxplot_grid <- lapply(1:3,vald_boxplot)

png('./plots_summaries/paper_plots/vald_boxplots.png',width = 2500,height = 1200,units = 'px',res = 300)
grid.arrange(grobs = boxplot_grid,nrow = 1,ncol = 3)
graphics.off()

################################################################################

vald_long <- function(index,censored = TRUE) {
    name <- vald_metrics[index]
    vald_agg <- vald_final %>% 
               group_by(ATHLETE,DATE) %>%
               mutate_if(is.numeric,mean)
    metric_vec <- as.matrix(vald_agg[name])
    
    # ggplot(data = vald_final,aes(x = as.Date(DATE), y = metric_vec,group = REP,color = REP)) + 
    #     geom_point() + geom_line() + theme_classic() + 
    #     facet_wrap(~ (paste0('Player ',if(censored) censored_name else uncensored)),
    #                scales='fixed') + 
    ggplot(data = vald_agg,
           aes(x = as.Date(DATE),y = metric_vec,
               color= if(censored) censored_name else uncensored)) +
                   geom_line() + geom_point() + theme_classic() + 
        ylab(gsub('_',' ',name)) + xlab('Date') + 
        scale_color_discrete(name = 'Player') + 
        ylab(clean_vald_metrics[index]) + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(date_breaks = '2 weeks',date_labels =  "%m-%d") +
        guides(color = guide_legend(nrow = 2)) +
        if(index != 1) theme(legend.position = 'none') else theme(legend.position = 'top') 
        
}
long_grid <- lapply(1:3,vald_long)
png('./plots_summaries/paper_plots/vald_long.png',width = 2000,height = 800,units = 'px',res = 250)
grid.arrange(grobs = long_grid,nrow = 1,ncol = 3)    
graphics.off()
################################################################################

vald_lines <- function(index,censored = TRUE) {
    name <- vald_metrics[index]
    metric_vec <- as.matrix(vald_final[name])
    ggplot(data = vald_final,
           aes(x = as.numeric(REP),
               y = metric_vec,color=DATE)) + 
        geom_line() + geom_point() + theme_classic() + 
        facet_wrap(~ (paste0('Player ',if(censored) censored_name else uncensored)), scales = 'fixed') + 
        ylab(gsub('_',' ',name)) + xlab('Jump') + 
        ylab(clean_vald_metrics[index]) + 
        theme(legend.position = 'none') + 
        scale_x_continuous(breaks = c(1,2))
}
line_grid <- lapply(1:3,vald_lines)
png('./plots_summaries/paper_plots/vald_lines.png',width = 2000,height = 1000,units = 'px',res = 250)
grid.arrange(grobs = line_grid,nrow = 1,ncol = 3)    
graphics.off()

################################################################################

kin_long <- kin_final %>% pivot_longer(cols = 3:8,
                                  names_to = 'METRIC',
                                  values_to = 'VALUE')

clean_kinexon <- unname(unlist(sapply(kin_long$METRIC, function(x) switch(x,
                          'DISTANCE' = 'Distance',
                          'JUMPS' = 'Jumps',
                          'PHYSIOLOAD' = 'Physio-load',
                          'SPEEDMAX' = 'Max. Speed',
                          'EXERTIONS' = 'Exertions',
                          'AAL'))))
kin_long$METRIC <- clean_kinexon

# boxplot
png('./plots_summaries/paper_plots/kinexon_boxplots.png',
    width = 2000,height = 1200,units = 'px',res = 250)
ggplot(data = kin_long,aes(x = as.factor(ATHLETE),y = VALUE)) + 
    facet_wrap(~ METRIC,scales = 'free') + 
    geom_boxplot() + theme_classic() +
    xlab('Player') + scale_color_discrete(name = 'Half') + ylab('Metric Value')
graphics.off()

# time lines
png('./plots_summaries/paper_plots/kinexon_time.png',
    width = 2000,height = 1200,units = 'px',res = 250)
ggplot(data = kin_long,aes(x = TIME,y = VALUE,color = as.factor(ATHLETE))) + 
    facet_wrap(~ METRIC,scales = 'free') + geom_point() + theme_classic() + 
    geom_smooth(method = 'lm',se = F) + 
    xlab('Time (min)') + ylab('Metric Value') + scale_color_discrete(name = 'Player')
graphics.off()

png('./plots_summaries/paper_plots/kinexon_time_half.png',
    width = 2000,height = 1200,units = 'px',res = 250)
ggplot(data = kin_long,aes(x = TIME,y = VALUE)) + 
    facet_wrap(~ METRIC,scales = 'free') + geom_point() + theme_classic() + 
    geom_smooth(method = 'lm',se = F) + 
    xlab('Time (min)') + ylab('Metric Value') + scale_color_discrete(name = 'Half')
graphics.off()


# date lines
png('./plots/kinexon_long.png',
    width = 2000,height = 1200,units = 'px',res = 250)
ggplot(kin_long,aes(x = as.Date(DATE), y = VALUE,color = as.factor(as.numeric(factor(ATHLETE))))) + 
    facet_wrap(~ METRIC,scales = 'free') + geom_point() + geom_line() + 
    theme_classic() + xlab('Date') + ylab('Metric Value') + theme_classic() + 
    scale_color_discrete(name = 'Player') + 
    scale_x_date(date_breaks = '2 weeks',date_labels =  "%m-%d") + 
    theme(axis.text.x = element_text(angle = 90))
graphics.off()

################################################################################

big_data <- merge(kin_total,vald_final,all.x=TRUE)

vald_kinexon <- function(x) {
    name <- vald_metrics[x]
    metric_vec <- as.matrix(big_data[name])
    ggplot(data = big_data,aes(x = metric_vec,y=VALUE,
                               color=as.factor(as.numeric(factor(ATHLETE))))) + 
        facet_wrap(~METRIC,scales = 'free') +
        geom_point() + # geom_smooth(method = 'lm',se = F) + 
        theme_classic() +
        xlab(clean_vald_metrics[x]) + ylab('LPS Metric Value') + 
        scale_color_discrete(name = 'Player') + 
        
    geom_smooth(aes(group = ATHLETE),method = 'lm',se=F,color = 'black') 
    # geom_smooth(aes(group = NA),method = 'lm',se=T,color='black') 
}

vald_kinexon_grid <- lapply(1:3,vald_kinexon)

png('./plots_summaries/paper_plots/vald_kinexon.png',
    width = 7,height = 9,units = 'in',res = 250)
grid.arrange(grobs = vald_kinexon_grid,nrow = 3,ncol = 1)
graphics.off()
vald_kinexon(3)

################################################################################

icc_data <- as.data.frame(instance_wrap$icc) %>% cbind(metric = cmj_metrics) %>% pivot_longer(cols = 1:5,names_to = 'player',values_to = 'icc')
cv_data <- as.data.frame(instance_wrap$cv) %>% cbind(metric = cmj_metrics) %>% pivot_longer(cols = 1:5,names_to = 'player',values_to = 'cv')

summary_data <- merge(icc_data,cv_data,by=c('metric','player'))
icc_plot <- ggplot(data = summary_data,aes(x = reorder(metric,-cv,median))) + 
    geom_path(aes(y = icc)) +
    geom_point(aes(y = icc),color='black') + coord_flip() + theme_classic() + 
    # theme(text = element_text(size = 6)) + 
    geom_hline(yintercept = 70,color='red')+
    geom_hline(yintercept = 0,color='black',lty=2) + 
    ylab('ICC (%)')


cv_plot <- ggplot(data = summary_data,aes(x = reorder(metric,-cv,median))) + 
    geom_path(aes(y = cv)) +
    geom_point(aes(y = cv),color='black') + coord_flip() + theme_classic() + 
    #theme(text = element_text(size = 6)) +
    geom_hline(yintercept = 10,color='red') + 
    ylab('CV (%) (Log Scale)') + 
    scale_y_log10() + 
    xlab('CMJ Metric')

png('./plots/result_chart.png',height = 10.5,width = 8,units='in',res=300)

cowplot::plot_grid(cv_plot + theme(axis.text.y = element_text(size = 7)), 
                   icc_plot + theme(axis.text.y = element_blank(),
                                   axis.title.y = element_blank(),
                                   axis.ticks.y = element_blank()),
                   nrow = 1,
                   align = 'v',
                   axis = 'r',
                   rel_widths = c(2,1))
graphics.off()
