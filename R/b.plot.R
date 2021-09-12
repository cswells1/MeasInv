#' The b-plot DIF procedure
#'
#' @param b.R numeric: b-parameters for reference group
#' @param b.F numeric: b-parameters for focal group
#' @param purify logical: purification for the anchor
#' @param sig.level numeric: the significance level
#'
#' @return b-plot and a data frame including b_Reference (b-parameters for reference group),
#' b_Focal(b-parameters for focal group), Distance(distance from the principal axis line),
#' Distance/SD (Distance devided by the standard deviation of the points around the principal axis line),
#' and Flagged(item(s) flagged as DIF).
#' @export
#'
b.plot <- function(b.R,b.F,purify,sig.level){

  slope <- (var(b.F)-var(b.R)+sqrt((var(b.F)-var(b.R))^2+4*cov(b.F,b.R)^2))/(2*cov(b.F,b.R))
  int <- mean(b.F) - slope*(mean(b.R))
  dist.line <- (slope*b.R - b.F + int)/sqrt(slope^2 + 1)
  SE.est <- sqrt(sum(dist.line^2)/(length(b.F)-2))
  dist.std <- dist.line/SE.est
  flag.items <- ifelse(abs(dist.std) >= abs(qnorm(sig.level/2)),1,0)
  items <- seq(1,length(b.R))
  nonDIF.items <- items[flag.items==0]
  anchor <- switch(purify,
                   yes=nonDIF.items,
                   no=seq(1,length(b.R)))
  slope <- (var(b.F[anchor])-var(b.R[anchor])+sqrt((var(b.F[anchor])-var(b.R[anchor]))^2+4*cov(b.F[anchor],b.R[anchor])^2))/(2*cov(b.F[anchor],b.R[anchor]))
  int <- mean(b.F[anchor]) - slope*(mean(b.R[anchor]))
  dist.line <- (slope*b.R - b.F + int)/sqrt(slope^2 + 1)
  SE.est <- sqrt(sum(dist.line^2)/(length(b.F)-2))
  dist.std <- dist.line/SE.est
  flag.items <- ifelse(abs(dist.std) >= abs(qnorm(sig.level/2)),1,0)
  nonDIF.items <- items[flag.items==0]
  DIF.items <- items[flag.items==1]
  dev.new(width = 8, height = 8)
  par(family="serif")
  max.x <- ceiling(max(abs(c(b.R,b.F))))
  plot(b.R[nonDIF.items], b.F[nonDIF.items],xlim=c(-max.x,max.x),ylim=c(-max.x,max.x),xlab="b-parameter Estimates (Reference Group)",
       ylab="b-parameter Estimates (Focal Group)",main="b-Plot",pch=1)
  DIF.items.y <- ifelse(length(DIF.items > 0), "yes", "no")
  switch(DIF.items.y,
         yes=text(b.R[DIF.items],b.F[DIF.items],label=DIF.items,col='red'),
         no=text(c(-100,100),c(-100,100),label=" ",col='red'))
  abline(int,slope)
  legend(-max.x,max.x,lty=1,legend=c("Principal Axis Line"),bty="n")
  points(b.R[DIF.items],b.F[DIF.items],pch=1,cex=3.5)
  flagged <- ifelse(abs(dist.std) >= abs(qnorm(sig.level/2)),"***","")
  out.stats <- data.frame(round(b.R,2),round(b.F,2),round(dist.line,2),round(dist.std,2),flagged)
  rownames(out.stats) <- paste("item",seq(1,length(b.R)))
  colnames(out.stats) <- c("b_Reference","b_Focal","Distance","Distance/SD","Flagged")
  return(out.stats)
}

