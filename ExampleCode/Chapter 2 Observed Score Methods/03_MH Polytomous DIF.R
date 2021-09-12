#####################################################################################
# The following code performs the Mantel-Haenszel DIF method for polytomous data 	  #
# using the Mantel.poly and GMH.poly functions.  		                                    #
#####################################################################################

# Read data: The data are in in a csv file in which first 12 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #
myfile <- system.file("extdata", "LikertData.csv", package = "MeasInv")
Likert.data <- read.csv(myfile, sep=",", header=T)

Likert.data$group <- factor(Likert.data$group)  # Convert the grouping variable, "group", to a factor #
                                                # which means R treats it as an unordered-categorical #
                                                # (i.e., grouping) variable.                          #


# Perform Mantel DIF procedure for polytomous item responses #
Mantel.output <- Mantel.poly(data = Likert.data[,1:12], group = Likert.data$group, 
		                    focal.name = 1, ref.name = 0, sig.level = .05, purify = TRUE)



# Perform GMH DIF procedure for polytomous item responses #
GMH.results <- GMH.poly(data = Likert.data[,1:12], group = Likert.data$group, 
                       sig.level = .05, purify = TRUE)






