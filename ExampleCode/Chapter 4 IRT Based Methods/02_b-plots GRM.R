#################################################################################################
# The following code uses Logistic Regression to test for DIF using the difLogistic             #
# function that is located in the difR package.                                       		#
#################################################################################################

# Read item paramter files from flexMIRT: The data are in a csv file in which first 30 columns #
# represent item responses and the last column contains the grouping #
# variable.                                                          #

# For Reference Group
myfile <- system.file("extdata", "LikertReference.par", package = "MeasInv")
Ref.est <- read.table(myfile, header = FALSE, sep = ",") 

# For Focal Group
myfile <- system.file("extdata", "LikertFocal.par", package = "MeasInv")
Foc.est <- read.table(myfile, header = FALSE, sep = ",")

# Examine threshold parameters #
b.Ref <- c(t(as.matrix(Ref.est[,5:8])))
b.Foc <- c(t(as.matrix(Foc.est[,5:8])))

b.plot(b.R = b.Ref, b.F = b.Foc, purify = "yes", sig.level = .01)


# Examine average threshold parameters #
b.Ref <- apply(Ref.est[,5:8],1,mean)
b.Foc <- apply(Foc.est[,5:8],1,mean)

b.plot(b.R = b.Ref, b.F = b.Foc, purify = "yes", sig.level = .01)

a.plot(a.R = Ref.est[,4], a.F = Foc.est[,4], purify = "yes", sig.level = .01)



          