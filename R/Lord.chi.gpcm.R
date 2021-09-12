
Lord.chi.gpcm <- function(par.Ref, par.Foc, cov.Ref, cov.Foc,
                         slope, delta.a, delta.b){
  
  dfree <- length(par.Ref)
  
  K <- length(par.Ref)
  
  temp1 <- matrix(as.numeric(par.Ref - par.Foc),nrow=1)
  
  cov.Foc.n <- matrix(sapply(cov.Foc, as.numeric), ncol=K, byrow=TRUE)
  cov.Ref.n <- matrix(sapply(cov.Ref, as.numeric), ncol=K, byrow=TRUE)
  
  A.mat <- matrix(1, nrow=K, ncol=K)
  diag(A.mat) <- slope^2
  A.mat[1,1] <- 1/slope^2

  cov.Foc.A <- A.mat*cov.Foc.n
  
  temp2 <- solve(cov.Foc.A + cov.Ref.n)
  
  TS <- (round(temp1 %*% temp2 %*% matrix(as.numeric(par.Ref - par.Foc), ncol=1),2))
  
  ncp <- ncp.grm(delta.a, delta.b, cov.Ref.n, cov.Foc.A, K)
  pval <- round(1-pchisq(TS, df=dfree, ncp=ncp),3)

  return(c(TS, dfree, ncp, pval))
  
}


