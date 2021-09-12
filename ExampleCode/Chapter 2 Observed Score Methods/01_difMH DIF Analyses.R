#################################################################################################
# The following code performs the Mantel-Haenszel DIF method using         				#
# the difMH function that is located in the difR package.                                       #
#################################################################################################
# Set working directory to location of data file. #
setwd("c:\\dropbox\\books\\Measurement Invariance\\Data\\")

# Load difR package #
library(difR)

# Read data: The data are in in a csv file in which first 30 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
MC.data <- read.csv("MCData.csv", sep=",", header=T)

MC.data$group <- factor(MC.data$group)  # Convert the grouping variable, "group", to a factor #
						                            # which means R treats it as an unordered-categorical #
						                            # (i.e., grouping) variable.                          #

# Perform MH DIF method using the difMH function, no purification. # 
MH.results <- difMH(Data = MC.data, group = "group", focal.name = 1, alpha = .05,
			              correct = TRUE, save.output = TRUE, output = c("MH Output","default"))

# Perform MH DIF method using the difMH function, purifying the anchor. # 
MH.results <- difMH(Data = MC.data, group = "group", focal.name = 1, alpha = .05, purify = TRUE, 
			              nrIter = 2,	correct = TRUE, save.output = TRUE, output = c("MH Output", "default"))

# Perform MH DIF method using the difMH function, purifying the anchor,      #
# and controlling the Type I error rate using the Dunn-Bonferroni procedure. # 
MH.results <- difMH(Data = MC.data, group = "group", focal.name = 1, alpha = .05, purify = TRUE, 
			              nrIter = 2,	correct = TRUE, p.adjust.method = "bonferroni", save.output = TRUE, 
			              output = c("MH Output", "default"))

# Compute confidence interval for D.MH (i.e., 2.35*Log(common odds ratio)) 
# using the CI.for.D function from the difStats package. #
CI.results <- CI.for.D(difMH.output = MH.results, sig.level = .05)

# Compute the effect size based on proportion correct metric and its corresponding confidence interval #
# using the PDIF function found in the "Functions for DIF.R" file.             				 #
PDIF.results <- PDIF(data = MC.data[,1:30], group = MC.data$group, focal.name = 1, 
	                  difMH.output = MH.results, sig.level = .05)







