##########################################################################################################
##########################################################################################################
# The following R code performs the Lord chi-square DIF test via the LordChi function from the           #
# MeasInv package.                                                                                       #
##########################################################################################################

# Load irtplay and dplyr packages #
library(dplyr)
library(irtplay)

### 3PLM ###
# Read item parameter estimates #

# For Reference Group
myfile <- system.file("extdata", "MCDataRef3PL.par", package = "MeasInv")
Ref.est <- read.table(myfile, header = FALSE, sep = ",") 

# For Focal Group
myfile <- system.file("extdata", "MCDataFoc3PL.par", package = "MeasInv")
Foc.est <- read.table(myfile, header = FALSE, sep = ",")


# Compute covariance matrix #
Foc.cov <- covirt(Foc.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov
Ref.cov <- covirt(Ref.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov

Lord.results <- LordChi(par.Ref=Ref.est, par.Foc=Foc.est, cov.Ref=Ref.cov, cov.Foc=Foc.cov, fix.g=TRUE, same.scale=FALSE,
                        purify=TRUE, sig.level=.05, delta.a=0.1, delta.b=0.3, delta.c=0.02)

plot.ICC(par.R = Lord.results$par.est.Ref, par.F = Lord.results$par.est.Focal.t, item = 20, D=1)



### Samejima's GRM ### 
# Read item parameter estimates #

# For Reference Group
myfile <- system.file("extdata", "LikertReference.par", package = "MeasInv")
Ref.est <- read.table(myfile, header = FALSE, sep = ",") 

# For Focal Group
myfile <- system.file("extdata", "LikertFocal.par", package = "MeasInv")
Foc.est <- read.table(myfile, header = FALSE, sep = ",")


# Compute covariance matrix #
Foc.cov <- covirt(Foc.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov
Ref.cov <- covirt(Ref.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov

Lord.results <- LordChi(par.Ref=Ref.est, par.Foc=Foc.est, cov.Ref=Ref.cov, cov.Foc=Foc.cov, same.scale=FALSE,
                        purify=TRUE, sig.level=0.05, delta.a=0.1, delta.b=0.1)

plot.ICC(par.R=Lord.results$par.est.Ref, par.F=Lord.results$par.est.Focal.t, item=4, D=1)
plot.ICC(par.R=Lord.results$par.est.Ref, par.F=Lord.results$par.est.Focal.t, item=11, D=1)



### Mixed Format Test: 3PLM and GPCM ###
# Read item parameter estimates # 

# For Reference Group
myfile <- system.file("extdata", "MixedDataReference.par", package = "MeasInv")
Ref.est <- read.table(myfile, header = FALSE, sep = ",") 

# For Focal Group
myfile <- system.file("extdata", "MixedDataFocal.par", package = "MeasInv")
Foc.est <- read.table(myfile, header = FALSE, sep = ",")

# Compute covariance matrix #
Foc.cov <- covirt(Foc.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov
Ref.cov <- covirt(Ref.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov

Lord.results <- LordChi(par.Ref=Ref.est, par.Foc=Foc.est, cov.Ref=Ref.cov, cov.Foc=Foc.cov, fix.g=FALSE, same.scale=FALSE,
                        purify=TRUE, sig.level=0.05, delta.a=0.1, delta.b=0.3, delta.c=0.02)

plot.ICC(par.R=Lord.results$par.est.Ref, par.F=Lord.results$par.est.Focal.t, item=12, D=1)
plot.ICC(par.R=Lord.results$par.est.Ref, par.F=Lord.results$par.est.Focal.t, item=19, D=1)
plot.ICC(par.R=Lord.results$par.est.Ref, par.F=Lord.results$par.est.Focal.t, item=40, D=1)






### 2PLM ###
# Read item parameter estimates #
Ref.est <- read.table("MCDataReference2PL.par", header=FALSE, sep=",")
Foc.est <- read.table("MCDataFocal2PL.par", header=FALSE, sep=",")

# Compute covariance matrix #
Foc.cov <- covirt(Foc.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov
Ref.cov <- covirt(Ref.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov

Lord.results <- LordChi(par.Ref=Ref.est, par.Foc=Foc.est, cov.Ref=Ref.cov, cov.Foc=Foc.cov, fix.g=FALSE, same.scale=FALSE,
                        purify=TRUE, sig.level=.05, delta.a=0.05, delta.b=0.2, delta.c=0)


### 1PLM ###
# Read item parameter estimates #
Ref.est <- read.table("MCDataReference1PL.par", header=FALSE, sep=",")
Foc.est <- read.table("MCDataFocal1PL.par", header=FALSE, sep=",")


# Compute covariance matrix #
Foc.cov <- covirt(Foc.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov
Ref.cov <- covirt(Ref.est, D=1, nstd=2500, norm.prior=c(0, 1), nquad=15)$cov

Lord.results <- LordChi(par.Ref=Ref.est, par.Foc=Foc.est, cov.Ref=Ref.cov, cov.Foc=Foc.cov, fix.g=FALSE, same.scale=FALSE,
                        purify=TRUE, sig.level=.05, delta.a=0, delta.b=0.5, delta.c=0)




