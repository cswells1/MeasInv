#################################################################################################
# This code performs the b-plot DIF method.                                        		          #
#################################################################################################

# Read item parameter files (3PLM) # 
# For Reference Group
myfile <- system.file("extdata", "MCDataRef3PL.par", package = "MeasInv")
Ref.est <- read.table(myfile, header = FALSE, sep = ",") 

# For Focal Group
myfile <- system.file("extdata", "MCDataFoc3PL.par", package = "MeasInv")
Foc.est <- read.table(myfile, header = FALSE, sep = ",")

# Peform b-plot DIF procedure using the b.plot function #
b.plot(b.R = Ref.est[,5], b.F = Foc.est[,5], purify = "yes", sig.level = .01)

# Peform a-plot DIF procedure using the b.plot function #
a.plot(a.R = Ref.est[,4], a.F = Foc.est[,4], purify = "yes", sig.level = .01)



          