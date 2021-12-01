 

#-------------------------------------------------------------------------------
# Multilevel Regression Modeling 
#
# using lme4 on hypothetical Retail Data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# PREPARATION
#-------------------------------------------------------------------------------

# Clear workspace
rm(list=ls(all=TRUE)) 
 
# (Install and) Load required packages
library("haven")       # Import Stata, SPSS, or SAS dataset 
library("lme4")        # Multilevel regression models
library("performance") # Performance of multilevel and other regression models
library("texreg")      # Well-formatted tables (mostly for regression)
library("lattice")     # Plotting results of lme4

# Make current directory the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  

# Start writing to text file and send output to screen (split = TRUE)
sink("MultilevelRetail.txt", split = TRUE) 

# Start writing plots to pdf file
pdf("MultilevelRetailPlots.pdf")

# Print text below in output: cat("text")
cat("
#-------------------------------------------------------------------------------
# Multilevel regression using R-Package lme4 on hypothetical retail data
#-------------------------------------------------------------------------------\n")

# Print start date and time in output
date() 

# Read-in retail data (Stata format) and name this "retail"
# NOTE: Data are hypothetical, with perfect fit if the correct model is used.
retail <- read_dta('retail.dta') 

cat("# Show some data from 'retail' data frame. \n")

summary(retail)    # Summary statistics of data frame
head(retail, 10)   # Show 10 lines of raw data

cat("\n
# Note below, all detailed analyses are blocked out by two leading pound signs.
# That is: ##.
# For detailed results, drop the leading-## and re-run the analyses. \n")

cat("
#-------------------------------------------------------------------------------
# Option 1: Single-level regression model (ignore multilevel)
#-------------------------------------------------------------------------------\n")

# Use the R-base procedure lm for linear regression models
option1.a <- lm(loyalty_1  ~ 1 + satisfac_1, data = retail)

# Drop the two-## in the next line for detailed results
##summary (option1.a)                         
screenreg(option1.a,
          custom.model.name = "Option 1: Loyalty_1",  # New model name in table
          single.row = TRUE,                          # Estimate (se) on 1 row
          digits = 3)                                 # Precision in output 

option1.b <- lm(loyalty_2  ~ 1 + satisfac_1, data = retail)
##summary (option1.b)                          
screenreg(option1.b,
          custom.model.name = "Option 1: Loyalty_2",  # New model name in table 
          single.row = TRUE,                          # Estimate (se) on 1 row
          digits = 3)                                 # Precision in output 

cat("
#-------------------------------------------------------------------------------
# Option 4: Multilevel regression model
#-------------------------------------------------------------------------------\n")

# Use R-package package lme4

cat("\n# Multilevel Model on loyalty_1: Random ANOVA's (no predictors) to determine ICC.\n")
option4.1 <- lmer(loyalty_1 ~ 1 + (1 | store), data = retail)
##summary(option4.1)
icc(option4.1)  # Report Conditional ICC (from R-Package 'performance')

cat("\n# Multilevel Model on loyalty_2: Random ANOVA's (no predictors) to determine ICC.\n")
option4.2 <- lmer(loyalty_2 ~ 1 + (1 | store), data = retail)
##summary(option4.2)
icc(option4.2)  # Report Conditional ICC (from R-Package 'performance')

# Multilevel Model: Random-Intercept
option4.a <- lmer(loyalty_1 ~ 1 + satisfac_1 + (1 | store), data = retail)
##summary(option4.a)
# Marginal r2 accounts for fixed effects
# Conditional r2 accounts for fixed and random effects 
r2(option4.a)  

cat("\n\n
# lmer will gives a warning. No worries. 
# This is due to the fact that our hypothetical example has few data 
# (n = 21, 3 groups), and a perfect structure. 
# In practice this is exceedingly rare.\n\n")

# Multilevel Model: Random-Slope
option4.b <- lmer(loyalty_2 ~ 1 + satisfac_1 + (satisfac_1 | store), data = retail)
##summary(option4.b)
# Marginal r2 accounts for fixed effects
# Conditional r2 accounts for fixed and random effects 
r2(option4.b) # Gives error message here because R2 is perfect 

cat("\n\n
# lmer will gives a warning. No worries. 
# This is due to the fact that our hypothetical example has few data 
# (n = 21, 3 groups), and a perfect structure. 
# In practice this is exceedingly rare.\n\n")

cat("
#-------------------------------------------------------------------------------
# Combine model summaries in a single table
#-------------------------------------------------------------------------------\n")

screenreg(list(option1.a, option4.a, option1.b, option4.b),
          custom.model.names = c("Single-loyalty_1","Multi-loyalty_1", 
                                 "Single_loyalty_2", "Multi_loyalty_2"), 
          single.row=TRUE, digits = 2) 
cat("
#-------------------------------------------------------------------------------
# PLOTTING
#-------------------------------------------------------------------------------\n")

cat("# Overall plot X by estimated Y for the level-2 units (here: stores). \n")
plot_loyalty_1 <- xyplot (fitted(option4.a) ~ satisfac_1 , 
                 data = retail [1:21,],
                 groups = store, # level-2 grouping variable
                 type=c("p","r"), # p = point, r = simple regression per level-2 unit
                 col = c("green","blue","red"), 
                 lty = c(2,1,3), # line type: 1 = straight, 2 = striped, 3 = dotted
                 lwd = 3,        # line width: 3 is a little thicker than 2
                 xlab="Satisfaction",
                 ylab="Estimated Loyalty_1")

cat("# Overall plot X by estimated Y for the level-2 units (here: stores).\n")
plot_loyalty_2 <- xyplot (fitted(option4.b) ~ satisfac_1 , 
                 data = retail [1:21,],
                 groups = store, # level-2 grouping variable
                 type=c("p","r"), # p = point, r = simple regression per level-2 unit
                 col = c(3,4,2), # alternative: color: 1 = black, 2 = red, 3 = green 4 = blue, 
                 lty = c(2,1,3), # line type: 1 = straight, 2 = striped, 3 = dotted
                 lwd = 3,        # line width: 3 is a little thicker than 2
                 xlab="Satisfaction",
                 ylab="Estimated Loyalty_2")

print(plot_loyalty_1) # Print ensures that plots appear in pdf output
print(plot_loyalty_2) # Print ensures that plots appear in pdf output

cat("
#-------------------------------------------------------------------------------
# FINALIZING
#-------------------------------------------------------------------------------\n")

# Print end date and time in output
date()

cat("\n# R^2 and ICC estimation for multilevel models used R-package performance.
# Use the following citation when reporting results.\n")
citation("performance")

# Stop sinking
sink()

# Stop plotting
dev.off()      

# Show results in console
file.show("MultilevelRetail.txt")
file.show("MultilevelRetailPlots.pdf")

### END of code ###

#-------------------------------------------------------------------------------
# Notes
#-------------------------------------------------------------------------------

# Note 1: Once more on plotting (need to repeat this)

# If plotting is not ended properly (something went wrong),
# you might see error messages when trying to plot in a next run.
# This is due to MS windows. Grrrr...
# The plotting device needs to be truned off before plotting again,
#
# If you get such plotting or "error opening pdf file" messages, then
# highlight dev.off() and run it several times until that gives an error (good).
# dev.off() should turn the device off.
# Then re-run the code
# dev.off is your friend

# Note 2: On model convergence:

# lme4 reports that the multilevel models here "fails to converge". 
# This is because the multilevel models fit the hypothetical data perfectly.
# Here this is no problem: The example only illustrates the use of lme4
# If this occurs with real data, remedial activity is needed.
# Check, for instance:
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


# Note 2: On R^2 for multilevel models

# Estimating R^2 for multilevel models is an active area in statistics.
# Here, we use the Nakagawa (2017) R^2 estimation as implemented in R-Package 
# 'performance'.
#
# From the performance package: 
# "The marginal r-squared considers only the variance of the fixed effects, 
# while the conditional r-squared takes both the fixed and random effects into account."
#
# When reporting the multilevel R^2, report which package was used to estimate it or them,
# and (ideally) which formulation the package uses.  
# Here's how to obtain the reference to a package

citation("performance")
