if(any(!(c('metric_names',"instance_wrap") %in% ls()))) source('./scripts/pre_processing.R')

cmj_metrics <- c(
    "Countermovement Depth",
    "Peak Power per Body Mass",
    "Flight Time",
    "Jump Height Relative to Landing Rate of Force Development",
    "Jump Height Relative to Peak Landing Force",
    "Jump Height from Displacement",
    "Jump Height from Impulse and Momentum",
    "Landing Rate of Force Development",
    "Mean Landing Acceleration",
    "Mean Landing Power",
    "Mean Landing Velocity",
    "Mean Takeoff Acceleration",
    "Concentric Mean Force",
    "Eccentric Mean Force",
    "Eccentric Mean Power",
    "Eccentric to Concentric Mean Force Ratio",
    "Concentric Mean Power",
    "Concentric Mean Power per Body Mass",
    "Eccentric Mean Power per Body Mass",
    "Mean Takeoff Velocity",
    "Peak Landing Acceleration",
    "Peak Landing Force",
    "Peak Landing Power",
    "Peak Landing Velocity",
    "Peak Takeoff Acceleration",
    "Peak Takeoff Force",
    "Peak Power",
    "Concentric Peak Velocity",
    "Concentric Rate of Force Development",
    "Time to Concentric Peak Force",
    "Contraction Time",
    "Eccentric Duration",
    "Landing Net Peak Force per Body Mass",
    "Peak Net Takeoff Force per Body Mass",
    "Concentric Rate of Force Development per Body Mass",
    "Vertical Velocity at Takeoff",
    "Concentric Duration",
    "Contraction Time to Eccentric Duration Ratio",
    "Eccentric to Concentric Duration Ratio",
    "Flight Time to Contraction Time Ratio",
    "Flight Time to Eccentric Duration Ratio",
    "Minimum Eccentric Force",
    "Braking Phase Duration",
    "Time to Braking Phase",
    "Eccentric Acceleration Phase Duration",
    "Eccentric Deceleration Phase Duration",
    "Braking Phase to Contraction Time Ratio",
    "Braking Phase to Concentric Duration Ratio",
    "Eccentric Mean Braking Force",
    "Eccentric Mean Deceleration Force",
    "Force at Peak Power",
    "First Half Concentric Impulse",
    "Second Half Concentric Impulse",
    "Eccentric Braking Rate of Force Development",
    "Eccentric Braking Rate of Force Development per Body Mass",
    "Eccentric Deceleration Rate of Force Development",
    "Peak Takeoff Force per Body Mass",
    "Concentric Peak Force",
    "Concentric Peak Force per Body Mass",
    "Eccentric Peak Force",
    "Eccentric Peak Force per Body Mass",
    "Peak Landing Force per Body Mass",
    "Mean Eccentric and Concentric Power to Time Ratio",
    "Lower-Limb Stiffness",
    "Concentric Rate of Power Development",
    "Concentric Rate of Power Development per Body Mass",
    "Modified Reactive Strength Index",
    "Total Work",
    "Velocity at Peak Power",
    "Eccentric Peak Velocity",
    "Eccentric Peak Power",
    "Eccentric Braking Impulse",
    "Eccentric Deceleration Impulse",
    "Concentric Impulse",
    "Force at Zero Velocity",
    "Positive Impulse",
    "Eccentric Unloading Impulse",
    "Positive Takeoff Impulse",
    "CMJ Stiffness",
    "Concentric Mean Force per Body Mass",
    "Eccentric Peak Power per Body Mass",
    "Eccentric to Concentric Peak Power Ratio",
    "Second to First Half Concentric Impulse Ratio",
    "Concentric Impulse in 100ms to Total Impulse Ratio",
    "Landing Impulse",
    "Eccentric Deceleration Impulse per Body Mass",
    "Displacement at Takeoff",
    "Force at Zero Velocity per Body Mass",
    "Modified Reactive Strength Index from Impulse and Momentum"
)

################################################################################

# CMJ Results

icc_tab <- instance_wrap$icc
rownames(icc_tab) <- cmj_metrics
write.csv(icc_tab,'./plots_summaries/CMJ_ICC.csv')

cv_tab <- instance_wrap$cv
rownames(cv_tab) <- counter_movement_jump_metrics
write.csv(cv_tab,'./plots_summaries/CMJ_CV.csv')

################################################################################

# Python Results

coef_results <- read.csv('./results/coefs_results.csv') %>%
    mutate(Feature = replace_na(cmj_metrics[match(Feature,metric_names)],'Time')) %>%
    pivot_wider(id_cols = c('Player','Feature'),names_from = 'index',values_from = 'Coefficient') %>%
    as.data.frame()
colnames(coef_results)[-(1:2)] <- c('AAL','Max Speed','Jumps','Physio-load','Exertions','Distance')

write.csv(coef_results,'./results/coefs_processed.csv')

rmse_results <- read.csv('./results/rmse_results.csv') %>%
    pivot_wider(id_cols = c('Player','Model'),names_from = 'index',values_from = 'RMSE') %>%
    as.data.frame()
colnames(rmse_results)[-(1:2)] <- c('AAL','Max Speed','Jumps','Physio-load','Exertions','Distance','Aggregate')

data_summary <- rmse_results %>%
    group_by(Player) %>%
    summarize(across(AAL:Aggregate, ~ .[Model == "Intercept"] - .[Model == "Elastic"]))

# Compute the average RMSE reduction
average_reduction <- data_summary %>%
    summarize(across(AAL:Aggregate, mean))

# Print the results
rbind(rmse_results,c(NA,NA,as.numeric(average_reduction)))

write.csv(rbind(rmse_results,c(NA,NA,as.numeric(average_reduction))),
          './results/rmse_processed.csv')

model_results <- t(rbind(1:5,read.csv('./results/model_results.csv')))
colnames(model_results) <- c('Player','alpha','n','l1/l2')
rownames(model_results) <- NULL
write.csv(model_results,'./results/model_processed.csv')

################################################################################