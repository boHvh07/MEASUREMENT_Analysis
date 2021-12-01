

#===============================================================================
# Scale Construction
#
# This code generates the data and analyzes it.
#-------------------------------------------------------------------------------

# Some initial bookkeeping: clean up the workspace
rm(list=ls(all=TRUE)) 

# (Install and) load required packages
library(MASS)           # Various Support Functions, here for data generation
library(psych)          # Tools for psycho-metrics. Here Cronbach's alpha
library(texreg)         # Nice tables (mostly for regression)
library(rstudioapi)     # Change directories easily

# Directory where this code is saved is working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(12345)         # To ensure that generated data replicate across platforms

sink("ScaleConstruction.txt", split=TRUE)  # Start writing text output to file "name"

cat("
#===============================================================================
# Scale Construction
#-------------------------------------------------------------------------------\n")

date()

cat("\n
#-------------------------------------------------------------------------------
# Generate data frames with specified variance-covariance or correlation structure, 
# using R-package MASS
#
# Skip if you focus on scale construction, and not data generation
#-------------------------------------------------------------------------------\n")

# data1: Example from Scale Construction Module in Survey Research Course
data1 <- data.frame(mvrnorm(500,                # Sample size is 500
                    mu = c(0,0,0),              # Means zero of three variables
                    Sigma = matrix(c(           # Specify square correlation matrix
                    1.00, 0.50, 0.50,
                    0.50, 1.00, 0.50,
                    0.50, 0.50, 1.00), 
                    ncol = 3),                  # Indicate n of columns
                    empirical = TRUE))          # Force exact correlations

# Name the variables in the data frame
names(data1) = c('x1', 'x2', 'x3')

# Write the data in csv format (now blocked; drop two leading ## to unblock)
## write.csv(x = data, file = "data1.csv")


#-------------------------------------------------------------------------------
# data2: Effect of reversed items: items which need reverse coding before scale construction
data2 <- data.frame(mvrnorm(500,                # Sample size is 500
                    mu = c(0,0,0,0),            # Means zero of four variables
                    Sigma = matrix(c(           # Specify square correlation matrix
                    1.00, -0.50, -0.50, -0.50,  # Item 1 is reversed, but not yet reverse-coded
                   -0.50, 1.00, 0.50, 0.50,
                   -0.50, 0.50, 1.00, 0.50,
                   -0.50, 0.50, 0.50, 1.00), 
                    ncol = 4),                  # Indicate n of columns
                    empirical = TRUE))          # Force exact correlations

# Name the variables in the data frame
names(data2) = c('x1', 'x2', 'x3', 'x4')

# The mvrnorm() function generates continuous normally distributed variables.
# Let us recode these into 5-point Likert-type of items common in surveys

data2$x1 <- as.numeric(cut(data2$x1,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data2$x2 <- as.numeric(cut(data2$x2,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data2$x3 <- as.numeric(cut(data2$x3,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data2$x4 <- as.numeric(cut(data2$x4,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))

#-------------------------------------------------------------------------------
# data3: Effect of bad items: items which correlated badly with the rest
data3 <- data.frame(mvrnorm(500,                  # Sample size is 500
                    mu = c(0,0,0,0),              # Means zero of four variables
                    Sigma = matrix(c(             # Specify square correlation matrix
                    1.00, 0.00, 0.00, 0.00,       # Item 1 is a bad item: low correlation with rest
                    0.00, 1.00, 0.50, 0.50,
                    0.00, 0.50, 1.00, 0.50,
                    0.00, 0.50, 0.50, 1.00), 
                    ncol = 4),                    # Indicate n of columns
                    empirical = TRUE))            # Force exact correlations

# Name the variables in the data frame
names(data3) = c('x1', 'x2', 'x3', 'x4')

# The mvrnorm() function generates continuous normally distributed variables.
# Let us recode these into 5-point Likert-type of items common in surveys

data3$x1 <- as.numeric(cut(data3$x1,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data3$x2 <- as.numeric(cut(data3$x2,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data3$x3 <- as.numeric(cut(data3$x3,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data3$x4 <- as.numeric(cut(data3$x4,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))

#-------------------------------------------------------------------------------
# data4: Drop redundant item when making a short-form scale
data4 <- data.frame(mvrnorm(500,                  # Sample size is 500
                    mu = c(0,0,0,0),              # Means zero of four variables
                    Sigma = matrix(c(             # Specify square correlation matrix
                    1.00, 0.80, 0.80, 0.80,       # All items are highly correlated
                    0.80, 1.00, 0.80, 0.80,       # Less items suffice
                    0.80, 0.80, 1.00, 0.80,
                    0.80, 0.80, 0.80, 1.00), 
                    ncol = 4),                    # Indicate n of columns
                    empirical = TRUE))            # Force exact correlations

# Name the variables in the data frame
names(data4) = c('x1', 'x2', 'x3', 'x4')

# The mvrnorm() function generates continuous normally distributed variables.
# Let us recode these into 5-point Likert-type of items common in surveys

data4$x1 <- as.numeric(cut(data4$x1,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data4$x2 <- as.numeric(cut(data4$x2,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data4$x3 <- as.numeric(cut(data4$x3,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data4$x4 <- as.numeric(cut(data4$x4,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
#-------------------------------------------------------------------------------
# data5: Scale Construction by Simple and Weighted Averaging
data5 <- data.frame(mvrnorm(500,                        # Sample size is 500
                    mu = c(0,0,0,0,0,0),                # Means zero of six variables
                    Sigma = matrix(c(                   # Specify square correlation matrix
                    1.00, 0.50, 0.50, 0.50, 0.50, 0.70, # Items vary in correlation: x1 most common
                    0.50, 1.00, 0.10, 0.20, 0.30, 0.30,
                    0.50, 0.10, 1.00, 0.30, 0.40, 0.20,
                    0.50, 0.20, 0.30, 1.00, 0.10, 0.10,
                    0.50, 0.30, 0.40, 0.10, 1.00, 0.10,
                    0.70, 0.30, 0.20, 0.10, 0.10, 1.00), 
                    ncol = 6),                          # Indicate n of columns
                    empirical = TRUE))                  # Force exact correlations

# Name the variables in the data frame
names(data5) = c('x1', 'x2', 'x3', 'x4', 'x5', 'y')

# The mvrnorm() function generates continuous normally distributed variables.
# Let us recode these into 5-point Likert-type of items common in surveys

data5$x1 <- as.numeric(cut(data5$x1,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data5$x2 <- as.numeric(cut(data5$x2,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data5$x3 <- as.numeric(cut(data5$x3,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data5$x4 <- as.numeric(cut(data5$x4,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data5$x5 <- as.numeric(cut(data5$x5,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
data5$y <- as.numeric(cut(data5$y,
                     breaks=c(-Inf, -1.5, -.5, .5, 1.5, Inf),
                     labels=c("1","2","3", "4", "5")))
cat("\n
#===============================================================================
# Data Analysis - 1: Scale Construction by Simple Averaging
#-------------------------------------------------------------------------------\n")

cat("\n
#-------------------------------------------------------------------------------
# Example 1: Class example, using data1 
#-------------------------------------------------------------------------------\n")

round(cor(data1), 1) 

cat("\n
# Cronbach's standardized alpha for data1 using R-package 'psych'.
# Note: package 'psych' reports two digits precision")
alpha(data1)

cat("\n
#-------------------------------------------------------------------------------
# Example 2:  x1 = Reversed item, using data2 
#-------------------------------------------------------------------------------\n")

# There are various ways to summarize data (e.g., with stargazer, R-base, etc.)
# Here we use the 'describe' function in R-library 'psych' (without skewness)
describe(data2, skew = FALSE) 

cat("\n
# Show correlation matrix for data2, n = 500, rounded to 2 digits.
# Making continuous variables ordinal reduces correlations, as expected.\n") 
round(cor(data2), 2) 

cat("\n
# Cronbach's standardized alpha for data2. \n")
alpha(data2)

# Function reports a potential problem with a negatively correlation item.
# by adding check.keys = TRUE, the function finds negatively worded items and 
# reverse codes them for this analysis (not permanently: You have to do that)
alpha(data2, check.keys = TRUE)

cat("\n
# Function identifies x1 correctly as the reversed item.
# Let us 'reverse key' the item. The item now ranges from 1 to 5.
# reverse keying is done by: 6 - item. \n")
data2$x1r <- 6 - data2$x1

cat("\n
# Re-estimate Cronbach' alpha. Note: skip column 1 where x1 is")
alpha(data2[-1])

cat("\n
# Make a scale by averaging across the scores of the five items,
# assuming equal weights for all items, and add scale to the data frame. 
# Note: use the reversed-key item: x1r. \n")
data2$scale2 <- (data2$x1r + data2$x2 + data2$x3 + data2$x4) / 4

cat("# Again descriptives of 'data2' \n")
describe(data2, skew = FALSE) 

cat("\n
# Conclusion:
# x1 is a good item.
#
# Action:
# 1. Reverse code x1 before making a scale.
# 2. Document and report that x1 was reverse coded.")

cat("\n
#-------------------------------------------------------------------------------
# Example 3: x1 = Bad loading item, using data3 
#-------------------------------------------------------------------------------\n")

describe(data3, skew = FALSE) 

cat("\n
# Show correlation matrix for data3, n = 500. \n") 
round(cor(data3), 2) 

cat("\n
# Cronbach's standardized alpha for data3.\n")
alpha(data3)

cat("\n
# x1 is a bad item. Current alpha = .53. If x1 deleted, alpha of remaining items = .71.\n") 
alpha(data3[-1])

cat("\n
# Make a scale by averaging across the scores of the three good items,
# assuming equal weights for all items, and add scale to the data frame. 
# Note: exclude the bad item: x1. \n")
data3$scale3 <- (data3$x2 + data3$x3 + data3$x4) / 3

cat("# Again descriptives of 'data3' \n")
describe(data3, skew = FALSE) 

cat("\n
# Conclusion:
# x1 is a bad item. It strongly reduces the reliability of the scale.
#
# Action:
# 1. Drop x1 before making a scale.
# 2. Document and report that x1 was dropped and why (was it hard to comprehend?).")

cat("\n
#-------------------------------------------------------------------------------
# Example 4: Items correlate highly, items could be dropped, using data4. 
#
# Typically: if a 'short-form' instrument is needed for the next study. 
#-------------------------------------------------------------------------------\n")

describe(data4, skew = FALSE) 

cat("\n
# Show correlation matrix for data4, n = 500. \n") 
round(cor(data4), 2) 

cat("\n
# Cronbach's standardized alpha for data3.\n")
alpha(data4)

cat("\n
# Cronbach's standardized alpha is .92. Dropping an item,
# reduces alpha to .90, which is still high. 
# Action:
# 1. If the survey has already been conducted: fine.
# 2. If these are results of a pilot survey, and an efficient multi-item measure 
#    is needed for the main survey, then consider dropping an item and make 
#    the scale with three items, perhaps even two items 
#    (say: for street or telephone survey).
# 3. (Always) report which items were dropped, why, and with which effect(s).")

cat("\n
#===============================================================================
# Data Analysis - 2 : Scale Construction by Unweighted and Weighted Averaging 
#-------------------------------------------------------------------------------\n")

cat("\n
# So far all scales were simple averages across a set of items. 
# This assumes the same (unit = 1) weight for each item.
# Yet, some items may be 'better' than others by having more 'true score'.
# Factor Analysis can be used to obtain weighted scales scores. \n")

describe(data5, skew = FALSE) 

cat("\n# Show correlation matrix for data5, n = 500. \n") 
round(cor(data5), 2) 

cat("\n# Cronbach's standardized alpha for data5")
# So far, we have used all items or we referred to column numbers.
# Here is an example of using the names of variables in the data to select a subset.

alpha(data5[, c("x1","x2","x3", "x4", "x5")])

cat("\n
# Cronbach's standardized alpha is .69, which is just satisfactory.
# The 95% Confidence interval is [.65; .74], so it overlaps .70: not toooo bad")

# Scale construction as un-weighted average scores across the 5 items
data5$scale5u <- (data5$x1 + data5$x2 + data5$x3 + data5$x4 + data5$x5) / 5

# Weighted average of items, using Factor Analysis with a single factor
# This example shows how to 
# (1) select a subset of items: item y is not selected here,
# (2) extract a single factor, and 
# (3) calculate factor scores using 'regression' approach
# Note that factor scores are standardized (mean = 0, sd = 1)

fa5 <- factanal(data5[, c("x1","x2","x3", "x4", "x5")], 
                factors = 1, scores = "regression") 
data5$scale5w <- fa5$scores           # Add factor scores: weighted average scale
loadings(fa5)                         # Explore factor loadings: x1 has largest loading 

describe(data5, skew = FALSE)
round(cor(data5[, c("scale5u", "scale5w")]),3)

cat("\n
#-------------------------------------------------------------------------------
# Regression analysis of y on unweighted (model5u) and weighted scale (model5w)
#-------------------------------------------------------------------------------\n")

model5u <- lm(y ~ scale5u, data = data5)
model5w <- lm(y ~ scale5w, data = data5)

# Compare two regression models in single table, using R-package texreg 
# which is an alternative to stargazer. texreg has many defaults.

screenreg(list(model5u, model5w), 
          custom.model.name = 
            c("Model 1: Unweighted Scale", "Model 2: Weighted Scale"), 
          single.row = TRUE, digits = 3, ci.force = TRUE) 

cat("\n
# Conclusion:
# 1. When items have different loadings on their underlying construct,
#    a weighted and unweighted scale do not correlate perfectly.
# 2. Then, the correlation of the scales with an external criterion, such as y,
#    may differ. Here, the weighted scale had a much large fit (R^2) than
#    the weighted score did. This may not always be the case.  
# 3. Yet, the weighted scale and its correlation with an external criterion, 
#    such as y, are generally is closer to the 'true score'.
#
# Action:
# 1. In case, the scale is published and validated, and Cronbach's alpha is high,
#    it is usually best to stick common practice: use an unweighted scale.
# 2. In case, the scale is new or 'ad hoc' and Cronbach's alpha is not very high,
#    and loadings of items differ, consider using a weighted scale.
# 3. In all cases, report which analyses were done, and why 'this' scale was used.\n\n")

#-------------------------------------------------------------------------------
# Final Bookkeeping
#-------------------------------------------------------------------------------

# To cite an R-package in your publication (report, paper):
citation("psych")

cat("\n\n# Done! \n")                        # Indicate end of the code 

sink()                                     # Stop writing output to txt file
file.show("ScaleConstruction.txt")         # Show output of file "name"

# END of code #
