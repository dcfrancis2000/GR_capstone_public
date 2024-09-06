################################################################################

# Cleaning & Processing Script

################################################################################

# Setup

setwd("C:/Users/dcfra/GR_capstone_public")
library(tidyverse)
library(snakecase)

kin <- read.csv('./vald_final/kinexon_scraped.csv') %>%
    filter(phasename != "Segment 11 (OT)")
vald <- read.csv('./vald_final/vald_scraped.csv')[,-1]

################################################################################

# Kinexon cleaning: mostly aggregating

# stop - stop time for length of measurement
kin$time <- difftime(kin$dtstop, kin$dtstart,'UTC','mins')

# anonymizing players
kin$playername <- as.numeric(as.factor(kin$playername))

kin_final <- kin %>% 
    na.exclude() %>%
    mutate_at(c("playerid","sessionid"),as.factor) %>% 
    mutate_at(c("dtstart"), as.Date) %>% 
    group_by(playername,sessionid) %>%
    summarize(
        date = as.factor(dtstart),
        aal = sum(aal),
        speedmax = max(speedmax),
        jumps = sum(jumps),
        physioload = sum(physioload),
        exertions = sum(exertions),
        distance = sum(distancetotal),
        time = as.numeric(sum(time)),
    ) %>% distinct() %>% ungroup() %>%
    dplyr::select(-c(sessionid))

colnames(kin_final) <- c('ATHLETE','DATE','AAL','SPEEDMAX','JUMPS','PHYSIOLOAD','EXERTIONS','DISTANCE','TIME')

################################################################################

# Vald cleaning: mostly formatting

replacebadcells <- function(x) as.numeric(replace(x,(is.na(x) | 
                                                         as.numeric(x)==0) | 
                                                      is.nan(x), 
                                                  mean(na.omit(as.numeric(x)))))

dates <- unique(kin_final$DATE)
problematic_metrics <- c('JUMP_HEIGHT_FLIGHT_TIME',
                         'CONCENTRIC_MAXIMUM_RFD',
                         "START_OF_MOVEMENT",
                         "START_OF_MOVEMENT_DETECTION_THRESHOLD",
                         "START_OF_INTEGRATION",                                   
                         "START_OF_CONCENTRIC_PHASE",
                         "JUMP_HEIGHT_IMP_MOM_IN_INCHES", 
                         "CONCENTRIC_RFD_100_MS",
                         "CONCENTRIC_RFD_200_MS",
                         "CONCENTRIC_RFD_50_MS",
                         "MOVEMENT_START_TO_PEAK_FORCE",
                         "MOVEMENT_START_TO_PEAK_POWER",
                         "START_OF_BRAKING_PHASE",
                         "START_OF_ECCENTRIC_DECELERATION_PHASE",
                         "CONCENTRIC_IMPULSE_50_MS",
                         "CONCENTRIC_IMPULSE_100_MS", 
                         "ECCENTRIC_BRAKING_RFD_100_MS",
                         "ECCENTRIC_BRAKING_RFD_100_MS_BM",
                         "ECCENTRIC_DECELERATION_RFD_BM",
                         "CONCENTRIC_RPD_50_MS",
                         "CONCENTRIC_RPD_50_MS_BM",
                         "CONCENTRIC_RPD_100_MS",
                         "CONCENTRIC_RPD_100_MS_BM", 
                         "LANDING_RFD_50_MS",
                         "JUMP_HEIGHT_FLIGHT_TIME_IN_INCHES"
)
vald$trial_date <- as.Date(vald$trial_date)

# Scraped vald_final is in long format, pivoting to wide format
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
    cbind(reps,vald_wide)[vald_wide$DATE %in% dates & reps <= 2, # first 2 reps only
                          c(2,3,1,6:(ncol(vald_wide)))] %>% # order columns
    dplyr::select(-all_of(problematic_metrics)) %>%
    mutate_at(c("ATHLETE","REP","DATE"), as.factor) %>%
    mutate_if(is.numeric, replacebadcells) %>% # mean imputation 
    mutate(ATHLETE = as.factor(as.numeric(as.factor(ATHLETE)))) 

metric_names <- colnames(vald_final)[-(1:3)]

################################################################################

# Preparing Predictive Dataset

# Merging Datasets
full_data <- merge(kin_final,vald_final %>% select(-REP) %>%
                       group_by(ATHLETE,DATE) %>%
                       mutate_if(is.numeric,mean) %>% distinct(),all.x = TRUE) %>%
    na.exclude() %>%
    mutate_at(-c(1,2),scale)

write.csv(full_data,'./vald_final/full_data.csv',row.names = F)

################################################################################

# Reliability testing (refuses to work if in its own script)

icc_mat <- matrix(NA,ncol = 5,nrow = length(metric_names))
cv_mat <- matrix(NA,ncol = 5,nrow = length(metric_names))
instance <- matrix(NA,nrow = length(metric_names),ncol=5)

for(i in 1:5) {
    newdata <- subset(vald_final,ATHLETE == sort(unique(vald_final$ATHLETE))[i])[,-1] 
    n <- length(metric_names)
    icc_vec <- cv_vec <- rep(NA, n)
    G <- newdata[,1]
    J <- newdata[,2]
    
    for (j in 1:n) {
        Y <- newdata[,j + 2]
        mod <- lm(Y ~ r(G) + J,unrestricted = FALSE)
        
        # Extract ANOVA table
        mod_anova <- anova(mod)$anova
        
        # Make sure the object structure is correct
        msb <- mod_anova[1, 3]  # Mean squares between groups (J)
        mse <- mod_anova[3, 3]  # Mean squares error
        
        icc_est <- ((msb - mse) / (msb + mse)) * 100
        
        # Variance component for G (this assumes mixlm output structure)
        game_var <- max(0,anova(mod)$var.comps[3])  # Extract variance component for G
        
        cv_est <- (sqrt(game_var) / abs(mean(Y))) * 100
        
        icc_vec[j] <- icc_est
        cv_vec[j] <- cv_est
    }
    
    icc_mat[,i] <- icc_vec
    cv_mat[,i] <- cv_vec
    
    icc_condition <- icc_vec >= 70
    # between day CV smaller than 10%
    cv_condition <- cv_vec <= 10 & cv_vec != 0 
    weeding_condition <- icc_condition & cv_condition
    instance[,i] <- weeding_condition
}

colnames(instance) <- sort(unique(vald_final$ATHLETE))
rownames(instance) <- metric_names
instance_wrap <- list(icc = icc_mat,cv = cv_mat,instance = instance + 0)

instance <- instance_wrap$instance

# instance matrix to be used in Python script
write.csv(instance,'./results/instance.csv',row.names = F)
