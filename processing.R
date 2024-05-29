setwd("C:/Users/dcfra/GR_capstone_public")
library(tidyverse)
kin <- read.csv('./data/kinexon_scraped.csv')

# Kinexon

timevec <- difftime(kin$dtstop, kin$dtstart,'UTC','mins')
kin$time <- timevec
kin2 <- subset(kin, phasename != "Segment 11 (OT)") %>%
    mutate_at(c("playerid","sessionid","phaseid"),as.factor) %>% 
    mutate_at(c("dtstart","dtstop"), as.Date) %>% 
    group_by(playername,playerpos,sessionid,phasename) %>%
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
kin2$phasename <- factor(gsub("\\D", "", kin2$phasename),levels = 1:10)
colnames(kin2) <- c('ATHLETE','POSITION','SEGMENT','DATE','TIME','AAL','SPEEDMAX','JUMPS','PHYSIOLOAD','EXERTIONS','DISTANCE')

kin2$HALF <- as.factor(ifelse(as.numeric(kin2$SEGMENT) <= 5,1,2))
kin_final <- kin2 %>% group_by(ATHLETE,DATE,HALF) %>% summarize(
    TIME = sum(TIME),
    AAL = sum(AAL),
    SPEEDMAX = max(SPEEDMAX),
    JUMPS = sum(JUMPS),
    PHYSIOLOAD = sum(PHYSIOLOAD),
    EXERTIONS = sum(EXERTIONS),
    DISTANCE = sum(DISTANCE),
    
) %>% distinct() %>% ungroup()

################################################################################
problematic_metrics <- c('JUMP_HEIGHT_FLIGHT_TIME')

# Vald
vald <- read.csv('./data/vald_scraped.csv')[,-1]

vald_metric_descriptions <- unique(vald[c('metric_name','metric_description')])
#write.csv(vald_metric_descriptions,'./database_nonsense/vald_descriptions.csv')

vald_wide <- pivot_wider(vald,id_cols = c(athlete_id, name,trial_date, test_id, trial_id),
                         names_from = metric_name, values_from = value,values_fn = median)
vald_wide <- vald_wide[order(vald_wide$test_id),]
vald_wide$trial_date <- as.Date(vald_wide$trial_date)

colnames(vald_wide) <- c('ATHLETEID','ATHLETE','DATE',snakecase::to_screaming_snake_case(colnames(vald_wide)[-(1:3)]))

reps <- vald_wide %>% group_by(TEST_ID) %>%
    summarise(REP = 1:n()) %>% ungroup() %>% dplyr::select(REP) %>% as.matrix()
vald_wide <- cbind(reps,vald_wide)

vald_wide2 <- vald_wide[,c(3,1,4,7:(ncol(vald_wide)))]
vald_wide2$REP <- as.factor(vald_wide2$REP)

replacebadcells <- function(x) as.numeric(replace(x,(is.na(x) | as.numeric(x)==0) | is.nan(x), 
                                                  mean(na.omit(as.numeric(x)))))

vald_wide2[,4:ncol(vald_wide2)] <- apply(vald_wide2[,4:ncol(vald_wide2)],2,replacebadcells)
dates <- unique(kin_final$DATE)

vald_final <- vald_wide2[vald_wide2$DATE %in% dates & as.numeric(vald_wide2$REP) <= 2,] %>%
    mutate_at(c("ATHLETE","REP","DATE"),as.factor) %>%
    dplyr::select(-(problematic_metrics))

full_data <- merge(kin_final,vald_final,all.x = TRUE) |> na.exclude()

source('./reliability_testing.R')
instance <- get_reliability_instance(vald_final)

# reliable metrics for at least 4/5 players
reliable_index <- apply(instance,1,sum) >= 4

cut_data <- full_data[,as.logical(c(rep(1,11),reliable_index))]
    
ML_data <- cut_data %>% dplyr::select(-REP) %>%
    mutate_at(1:3,as.factor) %>%
    mutate_at(-(1:3),as.numeric) %>%
    group_by(ATHLETE,DATE,HALF) %>% distinct() %>% ungroup() %>%
    mutate_if(is.numeric,scale)

write.csv(ML_data,'./data/ML_data.csv')
