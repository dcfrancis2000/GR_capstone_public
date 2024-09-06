################################################################################

# Reliability testing script

################################################################################

library(mixlm)

get_reliability_instance <- function(data) {
    icc_mat <- matrix(NA,ncol = 5,nrow = length(metric_names))
    cv_mat <- matrix(NA,ncol = 5,nrow = length(metric_names))
    instance <- matrix(NA,nrow = length(metric_names),ncol=5)
    for(i in 1:5) {
        newdata <- subset(data,ATHLETE == sort(unique(data$ATHLETE))[i])[,-1] 
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
            game_var <- max(0,anova(mod)$var.comps[1])  # Extract variance component for G
            
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
    colnames(instance) <- sort(unique(data$ATHLETE))
    rownames(instance) <- metric_names
    list(icc = icc_mat,cv = cv_mat,instance = instance + 0)
}
