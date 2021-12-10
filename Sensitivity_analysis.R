#######################################################################################
######## Analysis presented in the Research Report ###################################
#####################################################################################
#Sensitivity of Null Hypothesis Bayesian Testing in the context of two-level models#
###################################################################################

#IMPORTANT: 

  #1 set a working directory where the downloaded folder from GitHub is stored

#setwd("...")
#OR set it manually by clicking: Session -> Set Working Directory -> Choose Directory...

  #2 In case (some of) the packages below do not load uncomment and run (some of) the following code:

#install.packages("foreign")
#install.packages("bain")
#install.packages("lme4")
#install.packages("jtools")
#install.packages("psych")


# Load the required packages -------------------------------------

library(foreign) #for loading the data
library(bain) #the wrapper function does not work without loading bain
library(lme4) #for multilevel modeling
library(jtools) #for nice summaries of lmer objects
library(psych) #for nice descriptive statistics 
source("wrapper_function.R") # load the wrapper function

# Load the data -----------------------------------------------------------

dat <- read.spss(file = "tutorial.sav", to.data.frame = T)

#Additionally the data can be loaded directly from the `R2MLwiN` package, BUT NOTE
#in that case the data would contain many more variables (not used in this analysis)
#and additionally the naming of the variables is slightly different, here we will use
#the data loaded above


#If the reader wants to load the data directly from the `R2MLwiN` package (not advised):

#library(R2MLwiN)
#data(tutorial)

# Descriptive statistics  -------------------------------------------------

describe(dat)

# Fit the two-level model using lmer -------------------------------------

x <- lmer(Examscore ~ 1 + LRTscore + AvsLRT + (LRTscore | School), 
          REML = F, data = dat)
summary(x) #for the default output
summ(x)  #for nicer output



# SENSITIVITY ANALYSIS ----------------------------------------------------


# Use the wrapper to test the hypotheses using the default value for b 
#We use `standardize = FALSE`, since the data is already standardized by the original authors
#`N = "level_2" ` indicates that we will use the number of level 2 observations as the sample size 
#We use `fraction = 1`
#All of this means we apply: `b` = 2/65 = 0.03 (since, b = J/N), i.e., 1 * 0.03
bain_2lmer(x, "LRTscore = AvsLRT = 0;
         LRTscore = AvsLRT; LRTscore > 0 & AvsLRT >0", 
             fraction = 1, standardize = FALSE, N = "level_2" ,seed = 123)


# Iteratively change the value of the argument `fraction`

#`fraction = 2`, i.e., 2 * b
bain_2lmer(x, "LRTscore = AvsLRT = 0;
         LRTscore = AvsLRT; LRTscore > 0 & AvsLRT >0", 
           fraction = 2, standardize = FALSE, N = "level_2" ,seed = 123)

#`fraction = 3`, i.e., 3 * b
bain_2lmer(x, "LRTscore = AvsLRT = 0;
         LRTscore = AvsLRT; LRTscore > 0 & AvsLRT >0", 
           fraction = 3, standardize = FALSE, N = "level_2" ,seed = 123)

#`fraction = 4`, i.e., 4 * b
bain_2lmer(x, "LRTscore = AvsLRT = 0;
         LRTscore = AvsLRT; LRTscore > 0 & AvsLRT >0", 
           fraction = 4, standardize = FALSE, N = "level_2" ,seed = 123)

# Obtain the plot in the paper --------------------------------------------

# store the results obtained above
BF_o <- c(0.35, 0.25, 0.20, 0.17)
BF_i <- rep(4.21, 4)

# plot
par(mfrow = c(1, 2)) 
plot(BF_o, type = "l",  xaxt = "n", xlab = "times b", yaxt = "n", ylab = "BF", main = "(a)")
axis(1, at = 1:4)
axis(2, at = BF_o)
plot(BF_i, type = "l", xaxt = "n", yaxt = "n", xlab = "times b", ylab = "BF", main = "(b)")
axis(1, at = 1:4)
axis(2, at = BF_i)


# END OF ANALYSIS ------------------------------------------------------
