################################################################################

# Cleaning & Processing Script

################################################################################

# Setup
setwd("C:/Users/dcfra/GR_capstone_public")
library(tidyverse)
library(snakecase)

kin <- read.csv('./data/kinexon_scraped.csv')
vald <- read.csv('./data/vald_scraped.csv')[,-1]

################################################################################

# Kinexon cleaning: mostly aggregating

kin$time <- difftime(kin$dtstop, kin$dtstart,'UTC','mins')
kin$phasename <- factor(gsub("\\D", "", kin$phasename),levels = 1:10)
kin$half <- as.factor(ifelse(as.numeric(kin$phasename) <= 5,1,2))

# One observation per game half, two per person per game
kin_final <- subset(kin, phasename != "Segment 11 (OT)") %>% # no overtime
    mutate_at(c("playerid","sessionid","phaseid"),as.factor) %>% 
    mutate_at(c("dtstart"), as.Date) %>% 
    group_by(playername,sessionid,half) %>%
    summarize(
        date = as.factor(dtstart),
        time = as.numeric(sum(time)),
        aal = sum(aal),
        speedmax = max(speedmax),
        jumps = sum(jumps),
        physioload = sum(physioload),
        exertions = sum(exertions),
        distance = sum(distancetotal)
    ) %>% distinct() %>% ungroup() %>%
    dplyr::select(-c(sessionid))
colnames(kin_final) <- c('ATHLETE','HALF','DATE','TIME','AAL','SPEEDMAX','JUMPS','PHYSIOLOAD','EXERTIONS','DISTANCE')

################################################################################

# Vald cleaning: mostly formatting

replacebadcells <- function(x) as.numeric(replace(x,(is.na(x) | 
                                                     as.numeric(x)==0) | 
                                                     is.nan(x), 
                                                  mean(na.omit(as.numeric(x)))))

dates <- unique(kin_final$DATE)
problematic_metrics <- c('JUMP_HEIGHT_FLIGHT_TIME','CONCENTRIC_MAXIMUM_RFD')
vald$trial_date <- as.Date(vald$trial_date)

# Scraped data is in long format, pivoting to wide format
vald_wide <- pivot_wider(vald,
    id_cols = c(athlete_id,name,trial_date,test_id,trial_id),
    names_from = metric_name, 
    values_from = value,
    values_fn = median
    )
vald_wide <- vald_wide[order(vald_wide$test_id),-1] # order necessary for reps
colnames(vald_wide) <- c('ATHLETE','DATE',
                         to_screaming_snake_case(colnames(vald_wide)[-(1:2)]))

# adding rep 1,2 indicator to aid with reliability testing
reps <- vald_wide %>% group_by(TEST_ID) %>%
    summarise(REP = 1:n()) %>% ungroup() %>% dplyr::select(REP) %>% as.matrix()

vald_final <-
    cbind(reps,vald_wide)[vald_wide2$DATE %in% dates & as.numeric(vald_wide2$REP) <= 2, # first 2 reps only
                          c(2,3,1,6:(ncol(vald_wide)))] %>% # order columns
    dplyr::select(-(problematic_metrics)) %>%
    mutate_at(c("ATHLETE", "REP", "DATE"), as.factor) %>%
    mutate_if(is.numeric, replacebadcells) # mean imputation 

################################################################################

# Merging Datasets

full_data <- merge(kin_final,vald_final,all.x = TRUE) |> na.exclude()

# Weeding out unreliable vald metrics
source('./reliability_testing.R')
instance <- get_reliability_instance(vald_final)

# reliable metrics for at least 2/5 players
reliable_index <- apply(instance,1,sum) >= 2

ML_data <- full_data[,c(rep(T,10),F,reliable_index)] %>%
    group_by(ATHLETE,DATE) %>% 
    mutate_at(-(1:10),mean) %>% distinct() # average jump metrics over reps

write.csv(ML_data,'./data/ML_data.csv')
