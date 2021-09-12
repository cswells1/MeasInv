#################################################################################################
# The following code uses Logistic Regression to test for DIF using the difLogistic             #
# function that is located in the difR package.                                       		#
#################################################################################################

# Load rms package: It performs logistic (binary and ordinal) tha is used by  #
# the DIF.Logistic function.                                                  #
library(mirt)

# Read data: The data are in in a csv file in which first 30 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
myfile <- system.file("extdata", "MCData.csv", package = "MeasInv")
MC.data <- read.csv(myfile, sep=",", header=T)

MC.data$group <- factor(MC.data$group) # Convert the grouping variable, "group", to a factor #
						   # which means R treats it as an unordered-categorical #
						   # (i.e., grouping) variable.                          #



# Perform DIF on individual items using SIBTEST via the SIBTEST function    # 
# found in the mirt R package. Items 1-15 define the matching variable.     #
# Test item 21 #
SIBTEST(dat = MC.data[,1:30], group = MC.data$group, match_set = 1:15, 
        suspect_set = 21, focal_name = 1)


# Save the adjusted conditional item scores using the details=TRUE option. #
item.stats <- SIBTEST(dat = MC.data[,1:30], group = MC.data$group, match_set = 1:15, 
                      suspect_set = 21, focal_name = 1, details=TRUE)

# Test all items in an exploratory fashion. #
SIBTEST.results <- SIBTEST.exp(data = MC.data[,1:30], grp = MC.data$group, 
                               focal.name = 1, sig.level = .05, purify = FALSE)



# Read data: The data are in in a csv file in which first 12 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
myfile <- system.file("extdata", "LikertData.csv", package = "MeasInv")
Likert.data <- read.csv(myfile, sep=",", header=T)

Likert.data$group <- factor(Likert.data$group) # Convert the grouping variable, "group", to a factor #
# which means R treats it as an unordered-categorical #
# (i.e., grouping) variable.                          #



# Perform DIF on individual items using SIBTEST via the SIBTEST function    # 
# found in the mirt R package. Items 1-11 define the matching variable.     #
# Test item 12 #
SIBTEST(dat = Likert.data[,1:12], group = Likert.data$group, match_set = 1:11, suspect_set = 12, focal_name = 1)

# Test all items in an exploratory fashion. #
SIBTEST.results <- SIBTEST.exp(data = Likert.data[,1:12], grp = Likert.data$group, focal.name = 1, sig.level = .05, purify = TRUE)


# Perform DBF based on items 15-20 using SIBTEST via the SIBTEST function             # 
# found in the mirt R package. Items 1-14 and 21-30 define the matching variable.     #
SIBTEST(dat = MC.data[,1:30], group = MC.data$group, match_set = c(1:15), suspect_set = 16:20, focal_name = 1)



