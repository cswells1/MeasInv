#' The generlized MH procedure on polytomous data
#'
#' @param data numeric: either the data matrix only, or the data matrix plus the vector of group membership.
#' @param group factor: the vector of group membership
#' @param sig.level numeric: the significance level
#' @param purify logical: purification for the anchor
#'
#' @return The data frame of the output has a number of rows equla to the number of items.
#' The column include the GMH_Chi_Square (GMH Chi-square statistic), df (degree of freedom)
#' and p-value (p-value for the GMH Chi-square statistic).
#' @export
#'

GMH.poly <- function(data, group, sig.level,purify){
  test.length <- length(data[1,])
  raw.scores <- apply(data,1,sum)
  GMH.stat <- matrix(-999,test.length,ncol=1)
  GMH.df <- matrix(-999,test.length,ncol=1)
  GMH.pval <- matrix(-999,test.length,ncol=1)
  for (i in 1:test.length){
    temp <- table(group,data[,i],raw.scores)
    M <- length(temp[1,1,])
    freq.M <- matrix(-999,M,ncol=1)
    for (m in 1:M){
      freq.M[m] <- sum(temp[,,m])
    }
    pick <- c(1:M)
    pick <- pick[(freq.M > 1)]
    temp2 <- temp[,,pick]
    GMH.output <- mantelhaen.test(temp2, correct = TRUE)
    GMH.stat[i] <- round(GMH.output$statistic,3)
    GMH.df[i] <- GMH.output$parameter
    GMH.pval[i] <- round(GMH.output$p.value,3)
  }
  sig <- ifelse(GMH.pval < sig.level, 1, 0)
  items <- seq(1,test.length)
  nonDIF <- items[sig==0]
  for (i in 1:test.length){
    if(purify==TRUE){
      anchor.items <- sort(unique(c(nonDIF,i)))
    }
    if(purify==FALSE){
      anchor.items <- items
    }
    raw.scores <- apply(data[,anchor.items],1,sum)
    temp <- table(group,data[,i],raw.scores)
    M <- length(temp[1,1,])
    freq.M <- matrix(-999,M,ncol=1)
    for (m in 1:M){
      freq.M[m] <- sum(temp[,,m])
    }
    pick <- c(1:M)
    pick <- pick[(freq.M > 1)]
    temp2 <- temp[,,pick]
    GMH.output <- mantelhaen.test(temp2, correct = TRUE)
    GMH.stat[i] <- round(GMH.output$statistic,3)
    GMH.df[i] <- GMH.output$parameter
    GMH.pval[i] <- round(GMH.output$p.value,3)
  }
  out.stats <- data.frame(GMH.stat,GMH.df,GMH.pval)
  rownames(out.stats) <- paste("item",seq(1,test.length))
  colnames(out.stats) <- c("GMH_Chi_Square","df","p-value")
  return(out.stats)
}

#Mantel, N. & Haenszel, W. (1959). Statistical aspects of the analysis of data from retrospective studies of disease. Journal of the National Cancer Institute, 22, 719-748.
#Somes, G. W. (1986). The generalized Mantel-Haenszel statistic. American Statistician, 40, 106-108.
#Zenisky, A. L., Hambleton, R. K., & Robin, F. (2003). Detection of differential item functioning in large-scale state assessments: A study evaluating a two-stage approach. Educational and Psychological Measurement, 63, 51-64.

