#################################################################################################
# This code performs the b-plot DIF method.                                        		          #
#################################################################################################
# Set working directory to location of data file. #
setwd("c:\\dropbox\\books\\Measurement Invariance\\Chapter 4 Methods Based on IRT\\Data\\")

# Read item parameter file (3PLM) #
Ref.est <- read.table("MCDataRef3PL.par", header = FALSE, sep = ",")
Foc.est <- read.table("MCDataFoc3PL.par", header = FALSE, sep = ",")

# Peform b-plot DIF procedure using the b.plot function #
b.plot(b.R = Ref.est[,5], b.F = Foc.est[,5], purify = "yes", sig.level = .01)

# Peform a-plot DIF procedure using the b.plot function #
a.plot(a.R = Ref.est[,4], a.F = Foc.est[,4], purify = "yes", sig.level = .01)



          