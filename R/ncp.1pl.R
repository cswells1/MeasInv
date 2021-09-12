ncp.1pl <- function(delta.b, cov.Ref, cov.Foc.A){
  
  ncp <- matrix(-999, ncol=1)

  temp2 <- (cov.Foc.A + cov.Ref)
  
  delta <- delta.b^2
  ncp[1] <- (round(delta^2/temp2,2))

  max.ncp <- ifelse(ncp < 0, 0, ncp)
  return(max.ncp)
  
}
