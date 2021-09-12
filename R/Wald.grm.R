
# REVISE CODE # TEST EACH PARAMETER OR TEST A AND THEN B'S TOGETHER?

Wald.grm <- function(par.Ref, par.Foc, cov.Ref, cov.Foc, slope, delta.par, K){
  
  cov.Foc <- matrix(sapply(cov.Foc, as.numeric), ncol=K, byrow=TRUE)
  cov.Ref <- matrix(sapply(cov.Ref, as.numeric), ncol=K, byrow=TRUE)
  par.Ref <- as.matrix(par.Ref)
  par.Foc <- as.matrix(par.Foc)
  
  Wald.a <- matrix(ncol=2)
  Wald.a[1] <- round(((par.Ref[1] - par.Foc[1])^2)/(cov.Ref[1,1] + (1/slope^2)*cov.Foc[1,1]), 2)
  Wald.ncp <- (delta.par[1]^2)/(cov.Ref[1,1] + (1/slope^2)*cov.Foc[1,1])
  Wald.ncp <- as.numeric(ifelse(Wald.ncp < 0, 0, Wald.ncp))
  Wald.a[2] <- round(1 - pchisq(Wald.a[,1], df = 1, ncp = Wald.ncp), 3)
  
  Wald.b <- matrix(ncol=2*(K-1))
  TS.col <- seq(1, 2*(K-1)-1, by = 2)
  pval.col <- seq(2, 2*(K-1), by = 2)
  
  for (b in 1:(K-1)){
    Wald.b[TS.col[b]] <- round(((par.Ref[b+1] - par.Foc[b+1])^2)/(cov.Ref[b+1,b+1] + (slope^2)*cov.Foc[b+1,b+1]),2)
    Wald.ncp <- (delta.par[2]^2)/(cov.Ref[b+1,b+1] + (1/slope^2)*cov.Foc[b+1,b+1])
    Wald.ncp <- as.numeric(ifelse(Wald.ncp < 0, 0, Wald.ncp))
    Wald.b[pval.col[b]] <- round(1-pchisq(Wald.b[TS.col[b]], df=1, ncp=Wald.ncp),3)
  }

  #Wald.TS <- as.numeric(matrix(cbind(Wald.a, Wald.b),nrow=1))
  Wald.TS <- cbind(Wald.a, Wald.b)
  return(Wald.TS)

}
