#' a-plot DIF method
#'
#' @param a.R numeric: a-parameters for reference group
#' @param a.F numeric: a-parameters for focal group
#' @param purify logical: purification for the anchor
#' @param sig.level numeric: the significance level
#'
#' @return a-plot and a data frame including a_Reference
#' (a-parameters for reference group), a_Focal(a-parameters
#' for focal group), Distance(distance from the principal axis line),
#' Distance/SD (Distance divided by the standard deviation of the points
#' around the principal axis line), and Flagged(item(s) flagged as DIF).
#' @export
#'
#'
a.plot <- function(a.R,a.F,purify,sig.level){
  slope <- (var(a.F)-var(a.R)+sqrt((var(a.F)-var(a.R))^2+4*(cov(a.F,a.R))^2))/(2*cov(a.F,a.R))
  int <- mean(a.F) - slope*(mean(a.R))
  dist.line <- (slope*a.R - a.F + int)/sqrt(slope^2 + 1)
  SE.est <- sqrt(sum(dist.line^2)/(length(a.F)-2))
  dist.std <- dist.line/SE.est
  flag.items <- ifelse(abs(dist.std) >= abs(qnorm(sig.level/2)),1,0)
  items <- seq(1,length(a.R))
  nonDIF.items <- items[flag.items==0]
  anchor <- switch(purify,
                   yes=nonDIF.items,
                   no=seq(1,length(a.R)))
  slope <- (var(a.F[anchor])-var(a.R[anchor])+sqrt((var(a.F[anchor])-var(a.R[anchor]))^2+4*cov(a.F[anchor],a.R[anchor])^2))/(2*cov(a.F[anchor],a.R[anchor]))
  int <- mean(a.F[anchor]) - slope*(mean(a.R[anchor]))
  dist.line <- (slope*a.R - a.F + int)/sqrt(slope^2 + 1)
  SE.est <- sqrt(sum(dist.line^2)/(length(a.F)-2))
  dist.std <- dist.line/SE.est
  flag.items <- ifelse(abs(dist.std) >= abs(qnorm(sig.level/2)),1,0)
  nonDIF.items <- items[flag.items==0]
  DIF.items <- items[flag.items==1]
  dev.new(width = 8, height = 8)
  par(family="serif")
  max.x <- ceiling(max(c(a.R,a.F)))
  min.x <- floor(min(c(a.R,a.F)))
  plot(a.R[nonDIF.items], a.F[nonDIF.items],xlim=c(min.x,max.x),ylim=c(min.x,max.x),xlab="a-parameter Estimates (Reference Group)",
       ylab="a-parameter Estimates (Focal Group)",main="a-Plot",pch=1)
  DIF.items.y <- ifelse(length(DIF.items) > 0, "yes", "no")
  switch(DIF.items.y,
         yes=text(a.R[DIF.items],a.F[DIF.items],label=DIF.items,col='red'),
         no=text(c(-100,100),c(-100,100),label=" ",col='red'))
  abline(int,slope)
  legend(min.x,max.x,lty=1,legend=c("Principal Axis Line"),bty="n")
  points(a.R[DIF.items],a.F[DIF.items],pch=1,cex=3.5)
  flagged <- ifelse(abs(dist.std) >= abs(qnorm(sig.level/2)),"***","")
  out.stats <- data.frame(round(a.R,2),round(a.F,2),round(dist.line,2),round(dist.std,2),flagged)
  rownames(out.stats) <- paste("item",seq(1,length(a.R)))
  colnames(out.stats) <- c("a_Reference","a_Focal","Distance","Distance/SD","Flagged")
  return(out.stats)
}
