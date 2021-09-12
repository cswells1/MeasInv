#' The conditional p-value plot for the Standardized p-value difference (DSTD)
#'
#' @param data numeric: either the data matrix only, or the data matrix plus the vector of group membership.
#' @param item factor:  the vector of group membership
#' @param grp numeric or character: either the number or the name of the single item for which
#' the plots of conditional expected scores are plotted
#' @param focal.name numeric or character: a single value
#' @param ref.name numeric or character: a single value
#' @param anchor numeric: a vector of integer values specifying which items are currently considered
#' as anchor (DIF free) items.
#'
#' @return the plots of conditional p-value in the reference group and in the focal group
#' using the Standardized p-value difference (DSTD) method.
#' @export
#'

plot.cond.p <- function(data,item,grp,focal.name,ref.name,anchor){
  test.length <- length(data[1,])
  p.F <- matrix(-999, test.length+1, ncol=1)
  p.R <- matrix(-999, test.length+1, ncol=1)
  temp <- data[grp == focal.name,1:test.length]
  raw.score <- apply(temp,1,sum)
  for (m in 1:(test.length+1)){
    p.F[m] <- mean(temp[raw.score==(m-1),item], na.rm = T)
  }
  temp <- data[grp == ref.name,1:test.length]
  raw.score <- apply(temp[,anchor],1,sum)
  for (m in 1:(test.length+1)){
    p.R[m] <- mean(temp[raw.score==(m-1),item], na.rm = T)
  }
  dev.new(width = 8, height = 9)
  par(family="serif")
  par(mfrow=c(2,1))
  plot(seq(0,test.length),p.F,ylim=c(0,1),xlab = "Raw Score", ylab = "Proportion Correct",pch=21,
       main="Conditional p-values")
  points(seq(0,test.length),p.R,pch=16)
  legend(-1.5,1.08,pch=c(21,16),legend=c("Focal","Reference"),bty="n")

  Diff <- p.F-p.R
  par(family="serif")
  plot(seq(0,test.length),Diff,ylim=c(-1*max(abs(Diff),na.rm=T),max(abs(Diff),na.rm=T)),
       pch=20,xlab="Raw Score", ylab="Prop(Focal) - Prop(Ref)",
       main="Difference in Conditional p-values")
  abline(0,0)
  abline(.2,0,lty=2)
  abline(.4,0,lty=2)
  abline(-.2,0,lty=2)
  abline(-.4,0,lty=2)
}

#Dorans, N. J. & Holland, P. W. (1993). DIF detection and description: Mantel-Haenszel and standardization. In P. W. Holland & H. Wainer (Eds.), Differential item functioning (pp. 35-66). Hillsdale, NJ: Erlbaum.
#Dorans, N. J. & Kulick, E. (1986). Demonstrating the utility of the standardization approach to assessing unexpected differential item performance on the Scholastic Aptitude Test. Journal of Educational Measurement, 23, 355-368.
#Dorans, N. J., Schmitt, A. P., & Bleistein, C. A. (1988). The standardization approach to assessing differential speededness (Research Report No. 88-31). Princeton, NJ: Educational Testing Services.
