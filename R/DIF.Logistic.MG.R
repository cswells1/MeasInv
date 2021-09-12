#' Logistic regression DIF method for more than two groups
#'
#' @description This code performs logistic regression testing for DIF using more than
#' two groups.
#'
#' @param data numeric: either the data matrix only, or the data matrix plus the vector of group membership.
#' @param grp factor:  the vector of group membership
#' @param sig.level numeric: the significance level
#' @param purify logical: Default is FALSE
#' @param output.filename character: either the file path or file name to save the output
#'
#' @export
#'
DIF.Logistic.MG <- function(data,grp,sig.level,purify,output.filename){
  test.length <- length(data[1,])
  G2.uni <- matrix(-999,test.length,ncol=1)
  G2.nonuni <- matrix(-999,test.length,ncol=1)
  G2.both <- matrix(-999,test.length,ncol=1)
  pval.uni <- matrix(-999,test.length,ncol=1)
  pval.nonuni <- matrix(-999,test.length,ncol=1)
  pval.both <- matrix(-999,test.length,ncol=1)
  change.R2.uni <- matrix(-999,test.length,ncol=1)
  change.R2.nonuni <- matrix(-999,test.length,ncol=1)
  change.R2.both <- matrix(-999,test.length,ncol=1)
  x <- apply(data[,1:test.length],1,sum)
  for (i in 1:test.length){
    Model.1 <- lrm(data[,i] ~ x)
    Model.3 <- lrm(data[,i] ~ x + grp + grp*x)
    G2.both[i] <- round(Model.3$stats[3] - Model.1$stats[3],3)
    change.R2.both[i] <- round(Model.3$stats[10]-Model.1$stats[10],3)
  }
  df.b <- Model.3$stats[4]-Model.1$stats[4]
  pval.both <- round(1-pchisq(G2.both,df.b),3)
  flag.items <- ifelse(pval.both < sig.level,1,0)
  DIF.items <- seq(1,test.length)
  nonDIF.items <- DIF.items[flag.items==0]
  x.pur <- apply(data[,nonDIF.items],1,sum)
  if(purify==TRUE){
    x <- x.pur
  }
  if(purify==FALSE){
    x <- x
  }
  for (i in 1:test.length){
    Model.1 <- lrm(data[,i] ~ x)
    Model.2 <- lrm(data[,i] ~ x + grp)
    Model.3 <- lrm(data[,i] ~ x + grp + grp*x)
    G2.uni[i] <- round(Model.2$stats[3] - Model.1$stats[3],3)
    change.R2.uni[i] <- round(Model.2$stats[10]-Model.1$stats[10],3)
    G2.nonuni[i] <- round(Model.3$stats[3] - Model.2$stats[3],3)
    change.R2.nonuni[i] <- round(Model.3$stats[10]-Model.2$stats[10],3)
    G2.both[i] <- round(Model.3$stats[3] - Model.1$stats[3],3)
    change.R2.both[i] <- round(Model.3$stats[10]-Model.1$stats[10],3)
  }
  df.u <- Model.2$stats[4]-Model.1$stats[4]
  pval.uni <- round(1-pchisq(G2.uni,df.u),3)
  df.n <- Model.3$stats[4]-Model.2$stats[4]
  pval.nonuni <- round(1-pchisq(G2.nonuni,df.n),3)
  df.b <- Model.3$stats[4]-Model.1$stats[4]
  pval.both <- round(1-pchisq(G2.both,df.b),3)


  #class <- matrix("Neg",test.length,ncol=1)
  #class <- ifelse(pval.both < sig.level & change.R2.both >= .035, "Moderate", class)
  #class <- ifelse(pval.both < sig.level & change.R2.both >= .07, "Large", class)
  items <- paste("item",seq(1,test.length),sep="")
  out.stats <- data.frame(items,G2.uni,pval.uni,change.R2.uni,
                          G2.nonuni,pval.nonuni,change.R2.nonuni,
                          G2.both,pval.both,change.R2.both)
  colnames(out.stats) <- c("Item","G^2(Uniform)","p-value(Uniform)","Change-R^2(Uniform)",
                           "G^2(Non-uniform)","p-value(Non-uniform)","Change-R^2(Non-uniform)",
                           "G^2(Simult.)","p-value(Simult.)","Change-R^2(Simult.)")
  save.stats <- list(uniform=out.stats[,1:4], non.uniform=out.stats[,5:7], both=out.stats[,8:10])

  sink(paste(output.filename,".txt",sep=""),append=FALSE,split=TRUE)
  cat("\nTest for Uniform DIF\n\n")
  print(out.stats[,1:4],row.names=FALSE)
  cat("\nTest for Non-uniform DIF\n\n")
  print(out.stats[,c(1,5:7)],row.names=FALSE)
  cat("\nTest for Uniform and Non-uniform DIF\n\n")
  print(out.stats[,c(1,8:10)],row.names=FALSE)
  sink()
  return(save.stats)
}

#Harrell, F. E. (2020). rms: Regression Modeling Strategies. R package version 6.1-0. https://CRAN.R-project.org/package=rms
#Swaminathan, H. & Rogers, H. J. (1990). Detecting differential item functioning using logistic regression procedures. Journal of Educational Measurement, 27, 361-370.
#Zenisky, A. L., Hambleton, R. K., & Robin, F. (2003). Detection of differential item functioning in large-scale state assessments: A study evaluating a two-stage approach. Educational and Psychological Measurement, 63, 51-64.
#Zumbo, B. D. (1999). A handbook on the theory and methods of differential item functioning (DIF): Logistic regression modeling as a unitary framework for binary and Likert-type (ordinal) item scores. Ottawa, ON: Directorate of Human Resources Research and Evaluation, Department of National Defense.
