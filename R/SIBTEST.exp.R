#' SIBTEST
#'
#' @param data numeric: either the data matrix only, or the data matrix plus the vector of group membership.
#' @param grp factor: the vector of group membership
#' @param focal.name numeric or character: a single value
#' @param sig.level numeric: the significance level
#' @param purify logical: purification for the anchor
#'
#' @return The output is returned as lists for uniform DIF and non-uniform DIF. Each list includes Item (item name),
#' focal_group (membership), n_matched_set (the matching items), n_suspect_set (the item to be tested for DIF),
#' beta (DIF measure) , SE (the standard error for beta), X2 (the Chi-square test statistics for DIF),
#' df (degree of freedom), p (statistical p-value).
#'
#' @references
#' Chalmers, R. P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29. doi:10.18637/jss.v048.i06
#' Shealy, R. & Stout, W. (1993a). An item response theory model for test bias and differential item functioning. In P. Holland & H Wainer (Eds.), \emph{Differential item functioning} (pp. 197-240). Hillsdale, NJ: Erlbaum.
#' Shealy, R. & Stout, W. (1993b). A model-based standardization approach that separates true bias/DIF. \emph{Psychometrika, 58}, 159-194.
#' Zenisky, A. L., Hambleton, R. K., & Robin, F. (2003). Detection of differential item functioning in large-scale state assessments: A study evaluating a two-stage approach. \emph{Educational and Psychological Measurement, 63}, 51-64.

#'
#' @export
#'

SIBTEST.exp <- function(data,grp,focal.name,sig.level,purify){
  test.length <- length(data[1,])
  uni.SIB <- matrix(-999, nrow = test.length, ncol = 8)
  cro.SIB <- matrix(-999, nrow = test.length, ncol = 8)
  anchor <- seq(1,test.length)
  for (i in 1:test.length){
    temp <- SIBTEST(dat = data, group = grp, match_set = anchor[-i], suspect_set = i, focal_name = focal.name)
    temp <- as.matrix(temp)
    uni.SIB[i,] <- temp[1,]
    cro.SIB[i,] <- temp[2,]
  }
  pval.uni <- uni.SIB[,8]
  pval.cro <- cro.SIB[,8]
  flag.items <- ifelse(pval.uni < sig.level | pval.cro < sig.level,1,0)
  DIF.items <- seq(1,test.length)
  nonDIF.items <- DIF.items[flag.items==0]
  for (i in 1:test.length){
    if(purify==TRUE){
      anchor <- nonDIF.items
    }
    if(purify==FALSE){
      anchor <- seq(1,test.length)
    }
    temp <- SIBTEST(dat = data, group = grp, match_set = anchor[!anchor==i], suspect_set = i, focal_name = focal.name)
    temp <- as.matrix(temp)
    uni.SIB[i,] <- temp[1,]
    cro.SIB[i,] <- temp[2,]
  }
  uni.SIB[,c(4,5,6,8)] <- round(uni.SIB[,c(4,5,6,8)],3)
  cro.SIB[,c(4,5,6,8)] <- round(cro.SIB[,c(4,5,6,8)],3)
  items <- paste("item",seq(1,test.length),sep="")
  uni.SIB <- data.frame(items,uni.SIB)
  cro.SIB <- data.frame(items,cro.SIB)
  colnames(uni.SIB) <- c("Item","focal_group","n_matched_set","n_suspect_set","beta","SE","X2","df","p" )
  colnames(cro.SIB) <- c("Item","focal_group","n_matched_set","n_suspect_set","beta","SE","X2","df","p" )
  save.stats <- list(uniform=uni.SIB, non.uniform=cro.SIB)
  cat("\nTest for Uniform DIF\n\n")
  print(uni.SIB,row.names=FALSE)
  cat("\nTest for Non-uniform DIF\n\n")
  print(cro.SIB,row.names=FALSE)
  return(save.stats)
}

#Chalmers, R. P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. Journal of Statistical Software, 48(6), 1-29. doi:10.18637/jss.v048.i06
#Shealy, R. & Stout, W. (1993a). An item response theory model for test bias and differential item functioning. In P. Holland & H Wainer (Eds.), Differential item functioning (pp. 197-240). Hillsdale, NJ: Erlbaum.
#Shealy, R. & Stout, W. (1993b). A model-based standardization approach that separates true bias/DIF. Psychometrika, 58, 159-194.
#Zenisky, A. L., Hambleton, R. K., & Robin, F. (2003). Detection of differential item functioning in large-scale state assessments: A study evaluating a two-stage approach. Educational and Psychological Measurement, 63, 51-64.

