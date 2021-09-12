#' Plots of the conditional expected scores for polytomous item
#'
#' @param data numeric: either the data matrix only, or the data matrix plus the vector of group membership.
#' @param group factor:  the vector of group membership
#' @param item numeric or character: either the number or the name of the single item for which
#' the plots of conditional expected scores are plotted
#' @param focal.name numeric or character: a single value
#' @param ref.name numeric or character: a single value
#' @param anchor numeric: a vector of integer values specifying which items are currently considered
#' as anchor (DIF free) items.
#'
#' @return the plots of conditional expected scores for polytomous items in the reference group and in the focal group.
#' @export
#'

plot.cond.exp <- function(data,group,item,focal.name,ref.name,anchor){
  min.cat <- min(data[,item])
  max.cat <- max(data[,item])
  test.length <- length(data[1,])
  num.scores <- 1 + (max.cat-1)*length(anchor)
  min.score <- length(anchor)*min.cat
  max.score <- length(anchor)*max.cat
  EY.F <- matrix(-999, num.scores, ncol=1)
  EY.R <- matrix(-999, num.scores, ncol=1)
  temp <- data[group == focal.name,1:test.length]
  raw.score <- apply(temp[,anchor],1,sum)
  for (m in min.score:max.score){
    EY.F[(m+1)-min.score] <- mean(temp[raw.score==m,item], na.rm = T)
  }
  temp <- data[group == ref.name,1:test.length]
  raw.score <- apply(temp[,anchor],1,sum)
  for (m in min.score:max.score){
    EY.R[(m+1)-min.score] <- mean(temp[raw.score==m,item], na.rm = T)
  }
  dev.new(width = 8, height = 9)
  par(family="serif")
  par(mfrow=c(2,1))
  plot(seq(min.score,max.score),EY.F,ylim=c(min.cat,max.cat),xlab = "Raw Score", ylab = "Mean Score",pch=21,
       main="Conditional Expected-Values")
  points(seq(min.score,max.score),EY.R,pch=16)
  legend(min.score,max.cat,pch=c(21,16),legend=c("Focal","Reference"),bty="n")
  Diff <- EY.F-EY.R
  par(family="serif")
  plot(seq(min.score,max.score),Diff,ylim=c(-1*max(abs(Diff),na.rm=T),max(abs(Diff),na.rm=T)),
       pch=20,xlab="Raw Score", ylab="Focal - Reference",
       main="Difference in Conditional Expected Item Scores")
  abline(0,0)
}

#Dorans, N. J. & Holland, P. W. (1993). DIF detection and description: Mantel-Haenszel and standardization. In P. W. Holland & H. Wainer (Eds.), Differential item functioning (pp. 35-66). Hillsdale, NJ: Erlbaum.
#Dorans, N. J. & Kulick, E. (1986). Demonstrating the utility of the standardization approach to assessing unexpected differential item performance on the Scholastic Aptitude Test. Journal of Educational Measurement, 23, 355-368.
#Dorans, N. J. & Schmitt, A. P. (1993). Constructed response and differential item functioning: A pragmatic approach. In R. E. Bennett & W. C. Ward (Eds.), Construction versus choice in cognitive measurement: Issues in constructed response, performance testing, and portfolio assessment (pp. 135-165). Hillsdale, NJ: Erlbaum.
#Dorans, N. J., Schmitt, A. P., & Bleistein, C. A. (1988). The standardization approach to assessing differential speededness (Research Report No. 88-31). Princeton, NJ: Educational Testing Services.

