ncp.grm <- function(delta.a, delta.b, cov.Ref, cov.Foc.A, K){
  
  ncp <- matrix(-999, ncol=4)

  temp2 <- solve(cov.Foc.A + cov.Ref)
  
  delta <- matrix(c(delta.a, rep(delta.b, K-1)))
  temp1 <- t(delta)
  ncp[1] <- (round(temp1 %*% temp2 %*% delta,2))

  delta <- matrix(c(-delta.a, rep(delta.b, K-1)))
  temp1 <- t(delta)
  ncp[2] <- (round(temp1 %*% temp2 %*% delta,2))

  delta <- matrix(c(delta.a, rep(-delta.b, K-1)))
  temp1 <- t(delta)
  ncp[3] <- (round(temp1 %*% temp2 %*% delta,2))

  delta <- matrix(c(-delta.a, rep(-delta.b, K-1)))
  temp1 <- t(delta)
  ncp[4] <- (round(temp1 %*% temp2 %*% delta,2))

  max.ncp <- max(ncp)
  max.ncp <- ifelse(max.ncp < 0, 0, max.ncp)
  return(max.ncp)
  
}
