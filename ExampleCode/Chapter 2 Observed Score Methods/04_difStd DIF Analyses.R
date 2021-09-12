#####################################################################################
# The following code performs the Standardization DIF method using 			#
# the difStd function that is located in the difR package.                   		#
#####################################################################################

# Set working directory to location of data file. #
setwd("c:\\dropbox\\books\\Measurement Invariance\\Data\\")

# Load difR package: It contains the difTID function for performing   #
# the TID method which uses the distance from the principal axis line #
# to flag items as DIF.                                               #
library(difR)

# Read data: The data are in in a csv file in which first 30 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
MC.data <- read.csv("MCData.csv", sep=",", header=T)

MC.data$group <- factor(MC.data$group) # Convert the grouping variable, "group", to a factor #
						   # which means R treats it as an unordered-categorical #
						   # (i.e., grouping) variable.                          #

# Perform Standardization DIF method using the difStd function, no purification. # 
STD.results <- difStd(Data = MC.data, group = "group", focal.name = 1, stdWeight = "focal",
			thrSTD = .05, save.output = TRUE, output = c("STD Output","default"))

# Perform Standardization DIF method using the difStd function, with two-step purification. # 
STD.results <- difStd(Data = MC.data, group = "group", focal.name = 1, stdWeight = "focal",
			purify = TRUE, nrIter = 2, thrSTD = 0.05, save.output = TRUE, 
			output = c("STD Output","default"))

# Use the plot.cond.p function to plot conditions p-values and the difference in condition p-values. #
all.items <- seq(1, 30)
plot.cond.p(data = MC.data[,1:30], item = 20, grp = MC.data$group, focal.name = 1, ref.name = 0, anchor = all.items)



