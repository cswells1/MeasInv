#####################################################################################
# The following code performs the Standardization DIF method for polytomous data 	  #
# using the SMD function.                   		                                    #
#####################################################################################

# Set working directory to location of data file. #
setwd("c:\\dropbox\\books\\Measurement Invariance\\Data\\")

# Read data: The data are in in a csv file in which first 12 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
Likert.data <- read.csv("LikertData.csv", sep=",", header=T)

Likert.data$group <- factor(Likert.data$group)  # Convert the grouping variable, "group", to a factor #
                                                # which means R treats it as an unordered-categorical #
                                                # (i.e., grouping) variable.                          #

# Compute SMD statistic #
SMD.output <- SMD(data = Likert.data[,1:12], group = Likert.data$group,  
                  focal.name = 1, ref.name = 0, purify = FALSE)


# Plot conditional expected item response scores using all items as matching variables #
all.items <- seq(1,12)
plot.cond.exp(data = Likert.data[,1:12], group = Likert.data$group, item = 11, 
              focal.name = 1, ref.name= 0, anchor = all.items)


# Plot conditional expected item response scores using nonDIF items as matching variable    #
# (it is important to also include the studied item in the matching variable set,           #
# even if it was flagged as DIF                                                             #
all.items <- seq(1,12)
flag <- ifelse(abs(SMD.output[,2]) >= .05, 1, 0)
nonDIF.items <- sort(unique(c(all.items[flag == 0],11))) # Identifies the nonDIF items and includes item being tested #
plot.cond.exp(data = Likert.data[,1:12]-1, group = Likert.data$group, item = 11, 
              focal.name = 1, ref.name= 0, anchor = nonDIF.items)

