Wald.3pl <- function(par.Ref, par.Foc, cov.Ref, cov.Foc, slope, delta.par){
  
  cov.Foc <- matrix(sapply(cov.Foc, as.numeric), ncol=3, byrow=TRUE)
  cov.Ref <- matrix(sapply(cov.Ref, as.numeric), ncol=3, byrow=TRUE)
  
  Wald.a <- round(((par.Ref[1] - par.Foc[1])^2)/(cov.Ref[1,1] + (1/slope^2)*cov.Foc[1,1]),2)
  Wald.b <- round(((par.Ref[2] - par.Foc[2])^2)/(cov.Ref[2,2] + (slope^2)*cov.Foc[2,2]),2)
  Wald.c <- round(((par.Ref[3] - par.Foc[3])^2)/(cov.Ref[3,3] + cov.Foc[3,3]),2)
  
  Wald.TS <- as.numeric(matrix(c(Wald.a, Wald.b, Wald.c),nrow=1))
  
  Wald.ncp.a <- (delta.par[1]^2)/(cov.Ref[1,1] + (1/slope^2)*cov.Foc[1,1])
  Wald.ncp.b <- (delta.par[2]^2)/(cov.Ref[2,2] + (slope^2)*cov.Foc[2,2])
  Wald.ncp.c <- (delta.par[3]^2)/(cov.Ref[3,3] + cov.Foc[3,3])
  Wald.ncp <- as.numeric(matrix(c(Wald.ncp.a, Wald.ncp.b, Wald.ncp.c),ncol=1))
  
  pval.out <- round(1-pchisq(Wald.TS,df=1,ncp=Wald.ncp),3)
  return(c(Wald.TS[1], pval.out[1], Wald.TS[2], pval.out[2], Wald.TS[3], pval.out[3]))

}
