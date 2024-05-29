swag <- subset(vald_final,ATHLETE == 'Akuel Kot')

icc_funct_desctools <- function(data) {
    metric_names <- colnames(data)[-(1:2)]
    n <- length(metric_names)
    within_output <- rep(NA,n)
    between_output <- rep(NA,n)
    
    for (i in 1:n) {
        data_cut <- data[, c(1,2,i + 2)]
        rater_matrix <- reshape(data_cut, idvar = "REP", timevar = "DATE", direction = "wide")
        
        # Between days: columns reps (11 x 2) each
        rater_between <- as.matrix(rater_matrix[,-1])
        # within days: columns days (2 x 11) each
        rater_within <- as.matrix(t(rater_matrix[,-1]))
        colnames(rater_within) <- colnames(rater_between) <-
            rownames(rater_within) <- rownames(rater_between) <- NULL
        
        icc_within <- DescTools::ICC(rater_within)
        within_output[i] <- icc_within$results[3,2] * 100
        icc_between <- DescTools::ICC(rater_between)
        between_output[i] <- icc_between$results[3,2] * 100
    }
    output <- cbind(within_output,between_output)
    rownames(output) <- metric_names
    colnames(output) <- c('within','between')
    output
}

cv_funct_sumsq <- function(data) {
    # coefficient of variation based on within/between day mean squares
    metric_names <- colnames(data)[-(1:2)]
    n <- length(metric_names)
    within_output <- rep(NA,n)
    between_output <- rep(NA,n)
    
    for(i in 1:n) {
        temp <- data[,c(1,2,i+2)]
        X <- model.matrix(~ -1 + DATE,data=temp)[,] # design matrix is constant
        Y <- as.matrix(temp[,3])
        yhat <- X %*% MASS::ginv(t(X) %*% X) %*% t(X) %*% Y # pred
        ybar <- rep(mean(Y),nrow(X))
        
        # between day mean square
        msb <- t(yhat - ybar) %*% (yhat - ybar) / (qr(X)$rank - 1)
        # within day mean square
        msw <- t(Y - yhat) %*% (Y - yhat) / (length(Y) - qr(X)$rank)
        
        # absolute value in case of negative mean
        within_output[i] <- sqrt(msw) / abs(mean(Y)) * 100
        between_output[i] <- sqrt(msb) / abs(mean(Y)) * 100
        
    }
    output <- cbind(within_output,between_output)
    rownames(output) <- metric_names
    colnames(output) <- c('within','between')
    output
}

get_reliability_instance <- function(data) {
    # reliability for each metric for each player
    instance <- matrix(NA,nrow=ncol(data) - 3,ncol=5)
    for(i in 1:5) {
        newdata <- subset(data,ATHLETE == unique(data$ATHLETE)[i]) %>%
            dplyr::select(-ATHLETE)
        cv <- cv_funct_sumsq(newdata)
        icc <- icc_funct_desctools(newdata)
        
        # within-day ICC greater than 70%
        icc_condition <- icc[,1] < 70
        # between and within-day CV smaller than 10%
        cv_condition <- apply(cv < 10,1,function(x) x[1] & x[2])
        weeding_condition <- icc_condition & cv_condition
        instance[,i] <- as.numeric(weeding_condition)
    }
    colnames(instance) <- unique(data$ATHLETE)
    rownames(instance) <- colnames(data)[-c((1:3))]
    instance
}
