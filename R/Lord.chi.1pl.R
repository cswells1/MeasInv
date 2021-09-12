
Lord.chi.1pl <- function(par.Ref, par.Foc, cov.Ref, cov.Foc,
                         slope, delta.b){

  dfree <- 1

  temp1 <- as.numeric((par.Ref - par.Foc)^2)

  cov.Foc.n <- matrix(sapply(cov.Foc, as.numeric), ncol=1, byrow=TRUE)
  cov.Ref.n <- matrix(sapply(cov.Ref, as.numeric), ncol=1, byrow=TRUE)
  A.mat <- matrix(c(slope^2), ncol=1, byrow=TRUE)
  cov.Foc.A <- A.mat*cov.Foc.n
  temp2 <- (cov.Foc.A + cov.Ref.n)

  TS <- round((temp1/temp2),2)

  ncp <- ncp.1pl(delta.b, cov.Ref.n, cov.Foc.A)
  pval <- round(1-pchisq(TS, df=dfree, ncp=ncp),3)

  return(c(TS, dfree, ncp, pval))

}


