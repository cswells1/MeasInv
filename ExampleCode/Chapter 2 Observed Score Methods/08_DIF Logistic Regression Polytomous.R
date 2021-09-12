#################################################################################################
# The following code uses Logistic Regression to test for DIF using the DIF.Logistic            #
# function.                                                                          		#
#################################################################################################
# Set working directory to location of data file. #
setwd("c:\\dropbox\\books\\Measurement Invariance\\Data\\")

# Load rms package: It performs logistic (binary and ordinal) that is used by  #
# the DIF.Logistic function.                                                  #
library(rms)

# Read data: The data are in in a csv file in which first 30 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
Likert.data <- read.csv("LikertData.csv", sep=",", header=T)

Likert.data$group <- factor(Likert.data$group) # Convert the grouping variable, "group", to a factor #
						   # which means R treats it as an unordered-categorical #
						   # (i.e., grouping) variable.                          #

# Use logistic regression to test for DIF using the lrm function which is in the rms package. #
raw.score <- apply(X = Likert.data[,1:12], MARGIN = 1, FUN = sum)    # Compute raw score #
grp <- Likert.data$group

lrm(Likert.data[,1] ~ raw.score + grp + raw.score*grp)          # Perform logistic regression: Model 3 #
lrm(Likert.data[,1] ~ raw.score)                                # Perform logistic regression: Model 1 #

# Perform DIF using logistic regression via the DIF.Logistic function. #
Logistic.results <- DIF.Logistic(data = Likert.data[,1:12],grp = Likert.data$group, sig.level = .05, purify = TRUE, 
		                    output.filename = "LogReg Output")

