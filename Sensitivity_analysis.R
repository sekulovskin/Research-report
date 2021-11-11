######## Analysis presented in the Research Report ########
#Sensitivity of Null Hypothesis Bayesian Testing in the context of two-level models

#IMPORTANT: set a working directory where the downloaded folder `code` is stored

# Load the required packages -------------------------------------

library(foreign) #for loading the data
library(bain) #the wrapper function does not work without loading bain
source("wrapper_function.R") # load the wrapper function
library(lme4) #for multilevel modeling
library(jtools) #for nice summaries of lmer objects
library(psych) #for nice descriptives


# Load the data -----------------------------------------------------------

dat <- read.spss(file = "exam.sav", to.data.frame = T)

#Additionally the data can be loaded directly from the `R2MLwiN` package, BUT NOTE
# in this case the data would contain many more variables (not used in this analysis)
#and additionally the naming of the variables is slightly different, here we will use
#the data loaded above

#library(R2MLwiN)
#data(tutorial)

# Descriptive statistics  -------------------------------------------------

describe(dat)

# Fit the two-level models using lmer -------------------------------------

x <- lmer(Examscore ~ 1 + LRTscore + AvsLRT + (LRTscore | School), 
          REML = F, data = dat)
summary(x) #for the default output
summ(x)  #for nicer output



# Use the wrapper to test the hypotheses using the default value for b ----------
# In this case 1*b (where b = 2/65 = 0.03)
mlm_bain_std(x, "LRTscore = AvsLRT = 0;
         LRTscore = AvsLRT; LRTscore > 0 & AvsLRT >0", 
             fraction = 1, standardize = FALSE, seed = 123)


# SENSITIVITY ANALYSIS ---------------------------------------------------

# 2 * b
mlm_bain_std(x, "LRTscore = AvsLRT = 0;
         LRTscore = AvsLRT; LRTscore > 0 & AvsLRT >0", 
             fraction = 2, standardize = FALSE, seed = 123)
# 3 * b
mlm_bain_std(x, "LRTscore = AvsLRT = 0;
         LRTscore = AvsLRT; LRTscore > 0 & AvsLRT >0", 
             fraction = 3, standardize = FALSE, seed = 123)
# 4 * b
mlm_bain_std(x, "LRTscore = AvsLRT = 0;
         LRTscore = AvsLRT; LRTscore > 0 & AvsLRT >0", 
             fraction = 4, standardize = FALSE, seed = 123)

# Obtain the plot in the paper --------------------------------------------

# store the results obtained above
BF_o <- c(0.35, 0.25, 0.20, 0.17)
BF_i <- rep(4.21, 4)

# plot
par(mfrow = c(1, 2)) 
plot(BF_o, type = "l",  xaxt = "n", xlab = "times b", yaxt = "n", ylab = "BF", main = "a")
axis(1, at = 1:4)
axis(2, at = BF_o)
plot(BF_i, type = "l", xaxt = "n", yaxt = "n", xlab = "times b", ylab = "BF", main = "b")
axis(1, at = 1:4)
axis(2, at = BF_i)


# END OF ANALYSIS ------------------------------------------------------
