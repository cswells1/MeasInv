
Lord.chi.3pl <- function(par.Ref, par.Foc, cov.Ref, cov.Foc,
                         slope, delta.a, delta.b, delta.c){
  
  dfree <- length(par.Ref)

  temp1 <- matrix(as.numeric(par.Ref - par.Foc),nrow=1)
  
  cov.Foc.n <- matrix(sapply(cov.Foc, as.numeric), ncol=3, byrow=TRUE)
  cov.Ref.n <- matrix(sapply(cov.Ref, as.numeric), ncol=3, byrow=TRUE)
  A.mat <- matrix(c(1/slope^2, 1, 1/slope, 1, slope^2, slope, 1/slope, slope, 1), ncol=3, byrow=TRUE)
  cov.Foc.A <- A.mat*cov.Foc.n
  temp2 <- solve(cov.Foc.A + cov.Ref.n)
  
  TS <- (round(temp1 %*% temp2 %*% matrix(as.numeric(par.Ref - par.Foc), ncol=1),2))
  
  cov.Ref.n[3,1] <- ifelse(TS < 0,0, cov.Ref.n[3,1])
  cov.Foc.n[3,1] <- ifelse(TS < 0,0, cov.Foc.n[3,1])
  cov.Ref.n[3,2] <- ifelse(TS < 0,0, cov.Ref.n[3,2])
  cov.Foc.n[3,2] <- ifelse(TS < 0,0, cov.Foc.n[3,2])
  cov.Foc.A <- A.mat*cov.Foc.n
  temp2 <- solve(cov.Foc.A + cov.Ref.n)

  TS <- (round(temp1 %*% temp2 %*% matrix(as.numeric(par.Ref - par.Foc), ncol=1),2))
  
  ncp <- ncp.3pl(delta.a, delta.b, delta.c, cov.Ref.n, cov.Foc.A)
  pval <- round(1-pchisq(TS, df=dfree, ncp=ncp),3)

  return(c(TS, dfree, ncp, pval))
  
}


