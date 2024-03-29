#################################################################################################
# The following code uses Logistic Regression to test for DIF using the DIF.Logistic            #
# function.                                       		                                    #
#################################################################################################

# Load rms package: It performs logistic (binary and ordinal) that is used by  #
# the DIF.Logistic function.                                                  #
library(rms)

# Read data: The data are in in a csv file in which first 30 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
myfile <- system.file("extdata", "MixedData.csv", package = "MeasInv")
Mixed.data <- read.csv(myfile, sep=",", header=T)

Mixed.data$group <- factor(Mixed.data$group) # Convert the grouping variable, "group", to a factor #
						   # which means R treats it as an unordered-categorical #
						   # (i.e., grouping) variable.                          #


# Use logistic regression to test for DIF using the lrm function which is in the rms package. #
raw.score <- apply(X = Mixed.data[,1:40], MARGIN = 1, FUN = sum)    # Compute raw score #
grp <- Mixed.data$group

lrm(Mixed.data[,1] ~ raw.score + grp + raw.score*grp)          # Perform logistic regression: Model 3 #
lrm(Mixed.data[,1] ~ raw.score)                                # Perform logistic regression: Model 1 #

# Perform DIF using logistic regression via the DIF.Logistic function. #
DIF.Logistic(data = Mixed.data[,1:40], group = Mixed.data$group, sig.level = .05, purify = "yes", 
             output.filename = "LogReg Output")


