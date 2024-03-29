#################################################################################################
# The following code uses Logistic Regression to test for DIF using the difLogistic             #
# function that is located in the difR package.                                       	      	#
#################################################################################################

# Read item paramter files from flexMIRT: The data are in a csv file in which first 30 columns #
# represent item responses and the last column contains the grouping                           #
# variable.                                                                                    #

# For Reference Group
myfile <- system.file("extdata", "MixedDataReference.par", package = "MeasInv")
Ref.est <- read.table(myfile, header = FALSE, sep = ",") 

# For Focal Group
myfile <- system.file("extdata", "MixedDataFocal.par", package = "MeasInv")
Foc.est <- read.table(myfile, header = FALSE, sep = ",")

# Examine threshold parameters #
b.grm.R <- c(t(as.matrix(Ref.est[37:40,5:8])))
b.Ref <- c(Ref.est[1:36,5], b.gpcm.R)
b.grm.F <- c(t(as.matrix(Foc.est[37:40,5:8])))
b.Foc <- c(Foc.est[1:36,5],b.gpcm.F)

b.plot(b.R = b.Ref, b.F = b.Foc, purify = "yes", sig.level = .01)


# Examine average threshold parameters #
b.grm.R <- apply(Ref.est[37:40,5:8],1,mean)
b.Ref <- c(Ref.est[1:36,5],b.grm.R)
b.grm.F <- apply(Foc.est[37:40,5:8],1,mean)
b.Foc <- c(Foc.est[1:36,5],b.grm.F)


b.plot(b.R = b.Ref, b.F = b.Foc, purify = "yes", sig.level = .01)

# a-plot
a.Ref <- c(Ref.est[,4])
a.Foc <- c(Foc.est[,4])

a.plot(a.R = a.Ref, a.F = a.Foc, purify = "yes", sig.level = .01)



          