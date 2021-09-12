#' The confidence interval for the MH delta effect size
#'
#' @param difMH.output the data frame of the output from the statistics that are provided by the difMH function
#' @param sig.level numeric: the significance level
#'
#' @return The data frame of the output has a number of rows equal to the number of items.
#' The columns include DeltaMH (the transformed MH common odds ratio onto the Δ-metric),
#' SE(DeltaMH) (The standard error of deltaMH), Lower_Limit (the lower limits of the confidence interval for DeltaMH),
#' Upper_Limit (upper limits of the confidence interval for DeltaMH), Classification (Classification of DIF)
#' @export
#'

CI.for.D <- function(difMH.output, sig.level){
  var.alpha.hat <- difMH.output$varLambda
  alpha.hat <- difMH.output$alphaMH
  item.names = difMH.output$names
  SE.D.MH <- round(2.35*sqrt(var.alpha.hat),3)
  D.hat <- round(-2.35*log(alpha.hat),3)
  CI.for.D <- round(cbind((D.hat + SE.D.MH*qnorm(sig.level/2)), (D.hat + SE.D.MH*qnorm(1-sig.level/2))),3)
  class.DIF <- matrix("A",length(D.hat), ncol = 1)
  min.limit <- apply(abs(CI.for.D),1,min)
  class.DIF <- ifelse(min.limit > 1.0, "B", class.DIF)
  class.DIF <- ifelse(min.limit > 1.5, "C", class.DIF)
  out.stats <- data.frame(D.hat, SE.D.MH, CI.for.D, class.DIF)
  colnames(out.stats) <- c("DeltaMH","SE(DeltaMH)","Lower_Limit","Upper_Limit","Classification")
  rownames(out.stats) <- item.names
  return(out.stats)
}

#Magis, D., Beland, S., Tuerlinckx, F. & De Boeck, P. (2010). A general framework and an R package for the detection of dichotomous differential item functioning. Behavior Research Methods, 42, 847-862.
#Dorans, N. J. & Holland, P. W. (1993). DIF detection and description: Mantel-Haenszel and standardization. In P. W. Holland & H. Wainer (Eds.), Differential item functioning (pp. 35-66). Hillsdale, NJ: Erlbaum.
#Holland, P. W. (1985). On the study of differential item performance without IRT. Proceedings of the 27th annual conference of the Military Testing Association (Vol. 1, pp. 282-287). San Diego.
#Holland, P. W. (1989). A note on the covariance of the Mantel-Haenszel log-odds estimator and the sample marginal rates. Biometrics, 45, 1009-1015.
#Holland, P. W. & Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer & H. Braun (Eds.), Test validity (pp. 129-145). Hillsdale, NJ: Erlbaum.
