

#-------------------------------------------------------------------------------
# Multilevel Regression Modeling 
#
# using lme4 on the Goldstein (1995) School Data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# PREPARATION
#-------------------------------------------------------------------------------

# Clear workspace
rm(list=ls(all=TRUE)) 

# (Install and) Load required packages
library(haven)       # Import Stata, SPSS, or SAS dataset 
library(lme4)        # Multilevel regression models
library(texreg)      # Well-formatted tables (mostly for regression)
library(lattice)     # Plotting results of lme4
library(performance) # Multilevel and other regression functions. Here: ICC and r2

# Ensure same multilevel estimation results across machines/platforms
set.seed(12345)    

# Make current directory the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  

# Start writing to text file with "name.txt" and also show in console (split = TRUE)
sink("MultilevelSchools.txt", split = TRUE) 

# Start writing plots to pdf file
pdf("MultilevelSchoolsPlots.pdf")

cat("
#-------------------------------------------------------------------------------
# Multilevel regression modeling using R-Package lme4 
# on Goldstein's (1995) School Data
#-------------------------------------------------------------------------------\n") 

date()  

cat("
#-------------------------------------------------------------------------------
# Data Exploration
#-------------------------------------------------------------------------------\n")

# Read-in Goldstein school data (Stata format) and name this "schools",
# using R-package 'haven'
schools <- read_dta('schools.dta') 

cat("\n# Show some data.\n")
summary(schools) # Summary statistics of data frame

cat("\n# First few lines of data set \n")       
head(schools, 6) # First 6 lines

cat("
#-------------------------------------------------------------------------------
# M0: Random Anova Model, with intercept random across schools (using lme4).
#-------------------------------------------------------------------------------\n")

model.0 <- lmer(normexam ~ 1 + (1 | school), data = schools)
##summary (model.0)                       # Drop ## for summary output
icc(model.0)                              # ICC (using package performance)
r2(model.0)                               # Marginal and conditional R2 of model

cat("
#-------------------------------------------------------------------------------
# M1: Multilevel Regression Model, with random intercept.
#-------------------------------------------------------------------------------\n")

model.1 <- lmer(normexam ~ 1 + standlrt + (1 | school), data = schools)
##summary(model.1)                        # Drop ## for summary output  
r2(model.1)                                

cat("
#-------------------------------------------------------------------------------
# M2: Multilevel Regression Model, with random intercept and slope.
#-------------------------------------------------------------------------------\n")

model.2 <- lmer(normexam ~ 1 + standlrt + (1 + standlrt | school), data = schools)
##summary(model.2)                        # Drop ## for summary output  
r2(model.2)                                

cat("
#-------------------------------------------------------------------------------
# Combine model summaries in a single table
#-------------------------------------------------------------------------------\n")

screenreg(list(model.0, model.1, model.2),
          custom.model.names = c("M0: Random Anova", 
                                 "M1: Random Interc", 
                                 "M2: Random Interc and Slopes"),
          digits = 2, single.row = TRUE) 

cat("
#-------------------------------------------------------------------------------
# Likelihood Ratio (LR) test for model comparison
#-------------------------------------------------------------------------------\n")

# The anova command does LR-test, using ML to estimate loglikelihood (LL)
anova(model.0, model.1, model.2)   # Compare Model 0, Model 1, and Model 2

cat("\n
# Conclusion: 
# M1 outperforms M0 (LR test is significant).
# M2 outperforms M1 (LR test is significant).
#
# M2 is statistically significantly better than M1, but it is also more complex.
# Still, the AIC and BIC of Model 2 are also smaller than those of Model 1 and 0, 
# and smaller (or more negative) is better here. 
# M2 is the optimal model.
# \n")

cat("
#-------------------------------------------------------------------------------
# PLOTTING
#-------------------------------------------------------------------------------\n")

cat("# Plot 1: Overall plot X by estimated Y for the level-2 units (here: schools)
# Model.1: random intercept but fixed slope. \n")
plot1 <- xyplot (fitted(model.1) ~ standlrt , 
                 main = "M1: Random Intercepts", # title
                 data = schools [1:4059,],   # data is schools first-to-last rows
                 groups = school,            # separate lines for each group
                 type="l",                   # show fitted regression line
                 color = "school",           # separate color for each school
                 xlab="Standardized learning test",
                 ylab="Estimated normed exam score")

cat("# Plot 2: Overall plot X by estimated Y for the level-2 units (here: schools).
# Model.1: random intercept AND random slope.    \n")
plot2 <- xyplot (fitted(model.2) ~ standlrt , 
                 main = "M2: Random Intercepts and Slopes",
                 data = schools [1:4059,],
                 groups = school,
                 type="l",
                 color = "school",
                 xlab="Standardized learning test",
                 ylab="Estimated normed exam score")

cat("# Separate plot X by raw Y, for each of the level-2 units (here: schools)\n")
plot3 <- xyplot (normexam ~ standlrt | as.factor (school), 
                 main = "Separate regression line for each school", 
                 data = schools [1:4059,],
                 type=c("p","r"), # p = point, r = simple regression per level-2 unit
                 col="dark blue",
                 col.line="black",
                 xlab="Standardized learning test",
                 ylab="Observed normed exam score")

print(plot1)                      # print(plot name) to write to pdf
print(plot2)                      # print(plot name) to write to pdf
print(plot3)                      # print(plot name) to write to pdf

cat("
#-------------------------------------------------------------------------------
# FINALIZING
#-------------------------------------------------------------------------------\n")

# Print end date and time in output
date()

# Stop sinking
sink()

# Stop plotting
dev.off()                           

# Show results in console
file.show("MultilevelSchools.txt")
file.show("MultilevelSchoolsPlots.pdf")

### END of code ###

#-------------------------------------------------------------------------------
# COMMENTS
#-------------------------------------------------------------------------------

# Note 1: On estimation results

# Results of multilevel models can slightly differ between programs and platforms.
# Therefore:
# 1. Always set a random seed: set.seed(1234) (or any other nmber)
# 2. cite the packages used for estimations. To find the cite, do:
citation("lme4")

#-------------------------------------------------------------------------------
# Note 2: On R2: 

# R-squared (R2) for multilevel models here is based on: 
# Reference:
# Nakagawa S, Johnson P, Schielzeth H. 2017. 
# The coefficient of determination R2 and intra-class correlation coefficient 
# from generalized linear mixed-effects models revisted and expanded. 
# Journal of the Royal Society Interface 14. doi: 10.1098/rsif.2017.0213

# This R2 is implemented in the R-package 'performance'. 
# It reports:
# Conditional R2: fixed and random effects in the model.
# Marginal R2 : fixed effects only.
# Both are informative.

citation("performance")

#-------------------------------------------------------------------------------
# Note 3: On Estimating Random-effects in Multilevel Models

# My personal preference is to account for all random-components when 
# possible and useful (not when they are obviously zero or lead to estimation 
# problems)
#
# model.1 is such a Full Multilevel Regression Model
#
# model.1 <- lmer(normexam ~ 1 + standlrt + (1 + standlrt | school), data = schools)
#
# It also estimates the correlation between the random-intercept and slope
# that this the correlation between '1 and standlrt', where 1 is the random intercept.
# 
# in lme4 the default is to estimate this correlation.
# In SPSS MIXED the default is to NOT estimate this correlation
# 
# The SPSS behavior can be implemented in lme4 by having 
# two vertical bars || for random effects
#
# model.1 <- lmer(normexam ~ 1 + standlrt + (1 + standlrt || school), data = schools)
#
# 
