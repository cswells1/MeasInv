Wald.1pl <- function(par.Ref, par.Foc, cov.Ref, cov.Foc, slope, delta.par){
  
  cov.Foc <- matrix(sapply(cov.Foc, as.numeric), ncol=1, byrow=TRUE)
  cov.Ref <- matrix(sapply(cov.Ref, as.numeric), ncol=1, byrow=TRUE)
  
  Wald.b <- round(((par.Ref[2] - par.Foc[2])^2)/(cov.Ref[1,1] + (slope^2)*cov.Foc[1,1]),2)

  Wald.TS <- as.numeric(matrix(c(Wald.b),nrow=1))
  
  Wald.ncp.b <- (delta.par[2]^2)/(cov.Ref[1,1] + (slope^2)*cov.Foc[1,1])
  Wald.ncp <- as.numeric(matrix(c(Wald.ncp.b),ncol=1))
  
  pval.out <- round(1-pchisq(Wald.TS,df=1,ncp=Wald.ncp),3)
  return(c(Wald.TS[1], pval.out[1]))

}
