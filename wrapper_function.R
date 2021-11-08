mlm_bain_std <- function(x, hypotheses, fraction, standardize = FALSE, type.of.stand
                         = c("overall", "within"), seed){
  n_vas <- names(x@frame) #extract names of all variables in the data
  N <- nrow(coef(x)[[1]]) #sample size = level 2 observations
  #--------------------------------------
  #Unstandardized
  estimates <- coef(x) #extract the ML (unstandardized) "random" effects 
  estimates <- as.data.frame(estimates[[1]])
  estimates <- estimates[ , -1] #remove the intercept
  fixed_estimates <- apply(estimates, 2, mean) # obtain fix effects 
  cov <- as.matrix(vcov(x)[-1, -1]) #obtain the covariance matrix of the 
  #--------------------------------------
  #Overall standardization of the data "overall predictors"`
  standardized <- apply(x@frame, 2, scale)
  standardized <- as.data.frame(standardized)
  standardized[, ncol(standardized)] <- x@frame[ , ncol(x@frame)] #group
  names(standardized) <- n_vas #add the same names
  s <- lmer(x@call$formula, data = standardized, REML = FALSE)
  estimates_s <- coef(s) #extract the ML (unstandardized) "random" effects 
  estimates_s <- as.data.frame(estimates_s[[1]])
  estimates_s <- estimates_s[ , -1] #remove the intercept
  fixed_estimates_s <- apply(estimates_s, 2, mean) # obtain fix effects 
  #names(fixed_estimates_s) <- n #add names
  cov_s <- as.matrix(vcov(s)[-1, -1]) #obtain the covariance matrix of the 
  #--------------------------------------
  #Within group standardization of the (lvl1) predictors "within group predictors"
  y <- ave(x@frame[, 1], x@frame[, 4], FUN = scale) # standardize the outcome
  x1 <- ave(x@frame[, 2], x@frame[, 4], FUN = scale) # standardize the lvl1 pred
  # for the level 2 predictor: ------------
  split_group <- split(x@frame, x@frame[,4]) 
  x_lvl2 <- matrix(nrow = nrow(x@frame), ncol = 1)
  for (i in seq_along(split_group)){
    x_lvl2[i, ] <- split_group[[i]][1,3]
  }
  x_lvl2 <- scale(x_lvl2)  # standardize
  n_gr <- sapply(split_group, nrow)  #obtain size of each group
  x_lvl2_full <- suppressWarnings(rep(x_lvl2, each = n_gr))
  gr <- x@frame[ , ncol(x@frame)]  #factor for groups
  #------------------
  dat <- data.frame(y, x1, x_lvl2_full, gr) #combine in a data frame
  z <- lmer(y ~ 1 + x1 + x_lvl2_full + (0 + x1|gr), data = dat, REML = F) #improved
  estimates_z <- coef(z) #extract the ML (unstandardized) "random" effects 
  estimates_z <- as.data.frame(estimates_z[[1]])
  estimates_z <- estimates_z[ , -1] #remove the intercept
  fixed_estimates_z <- apply(estimates_z, 2, mean) # obtain fix effects
  n <- names(estimates) #extrct the names of parameters  I ONLY NEED THIS FOR NOW
  names(fixed_estimates_z) <- n #add names  I ONLY NEED THIS FOR NOW
  cov_z <- as.matrix(vcov(z)[-1, -1]) #obtain the covariance matrix 
  #---------------------------------------  
  #Compute BF's  
  
  if(standardize == FALSE){ 
    set.seed(seed)
    results <- bain(fixed_estimates, hypotheses, n = N, Sigma = cov, 
                    group_parameters = 0, 
                    joint_parameters = length(estimates), 
                    fraction = fraction)
  }
  
  else{
    
    
    if(type.of.stand == "within"){
      set.seed(seed)
      results <- bain(fixed_estimates_z, hypotheses, n = N, Sigma = cov_z,
                      group_parameters = 0, joint_parameters = length(estimates), 
                      fraction = fraction)
    }
    
    if(type.of.stand == "overall"){
      set.seed(seed)
      results <- bain(fixed_estimates_s, hypotheses, n = N, Sigma = cov_s,
                      group_parameters = 0, joint_parameters = length(estimates), 
                      fraction = fraction)
    }
    
  }
  return(results)
} 