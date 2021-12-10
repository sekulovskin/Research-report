bain_2lmer <- function(x, hypotheses, fraction, standardize = FALSE, 
                       N = c("level_1", "level_2", "ICC_effective"), seed){
 
  n_vas <- names(x@frame) #extract names of all variables in the data
  estimates <- coef(x) #extract the ML (unstandardized) "random" effects 
  estimates <- as.data.frame(estimates[[1]])
  estimates <- estimates[ , -1] #remove the intercept
  
  #--------------------------------------
  
if(N == "level_1"){
    
    N <- nrow(x@frame) #sample size = level 1 observations
    
  if(standardize == FALSE){ 
      
       #Unstandardized data
       fixed_estimates <- apply(estimates, 2, mean) # obtain fix effects 
       cov <- as.matrix(vcov(x)[-1, -1]) #obtain the covariance matrix of the 
       #--------------------------------------
      
       set.seed(seed)
       results <- bain(fixed_estimates, hypotheses, n = N, Sigma = cov, 
                      group_parameters = 0, 
                      joint_parameters = length(estimates), 
                      fraction = fraction)
    }
    
    else{
      
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
        
        set.seed(seed)
        results <- bain(fixed_estimates_s, hypotheses, n = N, Sigma = cov_s,
                        group_parameters = 0, joint_parameters = length(estimates), 
                        fraction = fraction)
    }
    
  }
  
  
if(N == "level_2"){
    
    N <- nrow(coef(x)[[1]]) #sample size = level 2 observations
    
  if(standardize == FALSE){ 
      
        #Unstandardized data
        estimates <- coef(x) #extract the ML (unstandardized) "random" effects 
        estimates <- as.data.frame(estimates[[1]])
        estimates <- estimates[ , -1] #remove the intercept
        fixed_estimates <- apply(estimates, 2, mean) # obtain fix effects 
        cov <- as.matrix(vcov(x)[-1, -1]) #obtain the covariance matrix of the 
      #--------------------------------------
      
        set.seed(seed)
        results <- bain(fixed_estimates, hypotheses, n = N, Sigma = cov, 
                      group_parameters = 0, 
                      joint_parameters = length(estimates), 
                      fraction = fraction)
  }
    
    else{
      
        #Overall standardization of the data "overall predictors"`
        standardized <- apply(x@frame, 2, scale)
        standardized <- as.data.frame(standardized)
        standardized[, ncol(standardized)] <- x@frame[ , ncol(x@frame)] #add grouping factor
        names(standardized) <- n_vas #add the same names
        s <- lmer(x@call$formula, data = standardized, REML = FALSE)
        estimates_s <- coef(s) #extract the ML (unstandardized) "random" effects 
        estimates_s <- as.data.frame(estimates_s[[1]])
        estimates_s <- estimates_s[ , -1] #remove the intercept
        fixed_estimates_s <- apply(estimates_s, 2, mean) # obtain fix effects 
        #names(fixed_estimates_s) <- n #add names
        cov_s <- as.matrix(vcov(s)[-1, -1]) #obtain the covariance matrix of the 
        #--------------------------------------
        
        set.seed(seed)
        results <- bain(fixed_estimates_s, hypotheses, n = N, Sigma = cov_s,
                        group_parameters = 0, joint_parameters = length(estimates), 
                        fraction = fraction)
    }
      
  }
  
if(N == "ICC_effective"){
    
    N_lvl1 <- nrow(x@frame)
    gr <- x@frame[ , ncol(x@frame)] # extract grouping factor
    split_gr <- split(x@frame, gr) #split with respect to the grouping variable
    N_clus <- mean(sapply(split_gr, nrow)) # compute the average group size
    model_0 <- lmer(x@frame[,1] ~ 1 + (1|gr)) # fit a random-intercept model
    var <- as.data.frame(VarCorr(model_0)) #extract the variances of the random intercept and residuals
    ICC <- var[1, 4]/ (var[1, 4] + var[2, 4]) #compute the ICC
    #compute the effective sample size based on the ICC approach 
    N <- N_lvl1 / (1 + (N_clus - 1) * ICC)
    
  if(standardize == FALSE){ 
      
       #Unstandardized data
       estimates <- coef(x) #extract the ML (unstandardized) "random" effects 
       estimates <- as.data.frame(estimates[[1]])
       estimates <- estimates[ , -1] #remove the intercept
       fixed_estimates <- apply(estimates, 2, mean) # obtain fix effects 
       cov <- as.matrix(vcov(x)[-1, -1]) #obtain the covariance matrix of the 
       #--------------------------------------
      
       set.seed(seed)
       results <- bain(fixed_estimates, hypotheses, n = N, Sigma = cov, 
                      group_parameters = 0, 
                      joint_parameters = length(estimates), 
                      fraction = fraction)
    }
    
  else{
      
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
        
      set.seed(seed)
      results <- bain(fixed_estimates_s, hypotheses, n = N, Sigma = cov_s,
                        group_parameters = 0, joint_parameters = length(estimates), 
                        fraction = fraction)
      }
      
 }
  return(results)
} 
