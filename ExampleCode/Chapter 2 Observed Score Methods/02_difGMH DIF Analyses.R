#################################################################################################
# The following code performs the Mantel-Haenszel DIF method using         				#
# the difMH function that is located in the difR package.                                       #
#################################################################################################

# Load difR package: It contains the difTID function for performing   #
# the TID method which uses the distance from the principal axis line #
# to flag items as DIF.                                               #
library(difR)

# Read data: The data are in in a csv file in which first 30 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
myfile <- system.file("extdata", "MCData3Group.csv", package = "MeasInv")#
MC.data <- read.csv(myfile, sep=",", header=T)

MC.data$group <- factor(MC.data$group) # Convert the grouping variable, "group", to a factor #
						   # which means R treats it as an unordered-categorical #
						   # (i.e., grouping) variable.                          #

# Perform GMH DIF method using the difGMH function, two-step purification. # 
GMH.results <- difGMH(Data = MC.data[,1:34], group = MC.data$group, focal.names = c(1,2), alpha = .05,
                      purify = TRUE, nrIter = 2, save.output = TRUE, output = c("GMH Output", "default"))

### Perform follow-up tests using difMH function. ###
# Select two groups of interest (e.g., groups 0 and 1). #
temp <- MC.data[MC.data$group==0 | MC.data$group==1,]

# Perform MH DIF method using the difMH function, two-step purification. # 
MH.results <- difMH(Data = temp, group = "group", focal.name = 1, alpha = .05, purify = TRUE, 
			              nrIter = 2,	correct = TRUE)


# Select two groups of interest (e.g., groups 0 and 2). #
temp <- MC.data[MC.data$group==0 | MC.data$group==2,]

# Perform MH DIF method using the difMH function, two-step purification. # 
MH.results <- difMH(Data = temp, group = "group", focal.name = 2, alpha = .05, purify = TRUE, 
                    nrIter = 2,	correct = TRUE)

