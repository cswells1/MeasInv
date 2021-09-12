ncp.2pl <- function(delta.a, delta.b, cov.Ref, cov.Foc.A){
  
  ncp <- matrix(-999, ncol=4)

  temp2 <- solve(cov.Foc.A + cov.Ref)
  
  delta <- c(delta.a,delta.b)
  temp1 <- t(delta)
  ncp[1] <- (round(temp1 %*% temp2 %*% delta,2))

  delta <- c(-delta.a,delta.b)
  temp1 <- t(delta)
  ncp[2] <- (round(temp1 %*% temp2 %*% delta,2))

  delta <- c(delta.a,-delta.b)
  temp1 <- t(delta)
  ncp[3] <- (round(temp1 %*% temp2 %*% delta,2))

  delta <- c(-delta.a,-delta.b)
  temp1 <- t(delta)
  ncp[4] <- (round(temp1 %*% temp2 %*% delta,2))

  max.ncp <- max(ncp)
  max.ncp <- ifelse(max.ncp < 0, 0, max.ncp)
  return(max.ncp)
  
}
