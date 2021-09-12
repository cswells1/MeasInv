#' The PDIF effect size from the difMH
#'
#' @param data numeric: either the data matrix only, or the data matrix plus the vector of group membership.
#' @param group factor:  the vector of group membership
#' @param focal.name numeric or character: a single value
#' @param difMH.output character: either the file path or file name to save the output
#' @param sig.level numeric: the significance level
#'
#' @return The data frame of the output has a number of rows equal to the number of items.
#' The columns include Prop_Focal (proportion correct for the focal group),
#' Prop_Ref* (predicted proportion correct in the reference group), PDIF, SE(PDIF) (standard error of the PDIF),
#' Lower_Limit (lower limit of the confidence interval for the parameter),
#' and Upper_Limit (upper limit of the confidence interval for the parameter).
#' @export
#'
PDIF <- function(data,group,focal.name,difMH.output,sig.level){
  var.alpha.hat <- difMH.output$varLambda
  alpha.hat <- difMH.output$alphaMH
  item.names = difMH.output$names
  p.F <- round(apply(data[group==focal.name,], 2, mean, na.rm=T),3)
  p.R.hat <- round((alpha.hat*p.F)/((1-p.F) + alpha.hat*p.F),3)
  PDIF.est <- round(p.F - p.R.hat,3)
  q.F <- (1 - p.F)
  N.F <- sum(group==focal.name)
  K <- alpha.hat/(q.F + alpha.hat*p.F)
  part1 <- (((1-K)^2)*p.F*q.F)/(N.F)
  part2 <- (K^2)*((p.F*q.F)^2)*var.alpha.hat
  part3 <- (2*K*(1-K)*p.F*q.F)/(N.F)
  SE.PDIF <- round(sqrt(part1 + part2 + part3),3)
  CI.for.PDIF <- round(cbind((PDIF.est + SE.PDIF*qnorm(sig.level/2)),
                             (PDIF.est + SE.PDIF*qnorm(1-sig.level/2))),3)
  out.stats <- data.frame(p.F,p.R.hat,PDIF.est,SE.PDIF,CI.for.PDIF)
  colnames(out.stats) <- c("Prop_Focal","Prop_Ref*","PDIF","SE(PDIF)","Lower_Limit","Upper_Limit")
  return(out.stats)
}

#Dorans, N. J. & Holland, P. W. (1993). DIF detection and description: Mantel-Haenszel and standardization. In P. W. Holland & H. Wainer (Eds.), Differential item functioning (pp. 35-66). Hillsdale, NJ:
#Magis, D., Beland, S., Tuerlinckx, F. & De Boeck, P. (2010). A general framework and an R package for the detection of dichotomous differential item functioning. Behavior Research Methods, 42, 847-862.

