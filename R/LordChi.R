#' Lord’s Chi-Square DIF Procedure
#'
#' @param par.Ref a data frame for the reference group including the columns of item ID,
#' number of score categories, IRT model, a-parameter, b-parameter, and g-parameter
#' @param par.Foc a data frame for the focal group including the columns of item ID,
#' number of score categories, IRT model, a-parameter, b-parameter, and g-parameter
#' @param cov.Ref a list of covariance matrices for each item for the reference group
#' @param cov.Foc a list of covariance matrices for each item for the focal group
#' @param fix.g logical: whether g is fixed or constrained to be equal between the groups (Default is FALSE.)
#' @param same.scale logical: whether the item parameter estimates are on the same scale (Default is FALSE.)
#' @param purify logical: purification for the anchor (Default is FALSE.)
#' @param sig.level numeric: the significance level (Default is FALSE.)
#' @param delta.a (Default is 0.)
#' @param delta.b (Default is 0.)
#' @param delta.c (Default is 0.)
#'
#' @return A list that contains the metadata information for the reference group
#' ($par.est.Ref), the metadata information($par.est.Focal.t), and test statistics
#' for the Lord’s chi-square($Lord.TS) and Wald tests ($Wald.TS) for the focal group
#' with the transformed item parameter estimates. The data frame for Lord.TS contains
#' for each item the item ID, number of score categories, and IRT model, as well as
#'   the Lord’s chi-square test statistic, degrees of freedom, its corresponding p-value,
#'   noncentrality parameter, and whether the item was flagged as DIF using the significance
#'   level supplied by the user.
#' @export
#'

LordChi <- function(par.Ref, par.Foc, cov.Ref, cov.Foc, fix.g = FALSE, same.scale=FALSE,
                    purify = FALSE, sig.level=.05, delta.a=0, delta.b=0, delta.c=0){

  # Define test length #
  test.length <- nrow(par.Ref)
  items <- seq(1,test.length)


  # Define variables types in par.Ref and par.Foc data frames #
  num.fields <- length(par.Ref[1,])
  par.Ref[,1] <- as.character(par.Ref[,1])
  par.Ref[,2] <- as.numeric(as.character(par.Ref[,2]))
  par.Ref[,3] <- as.character(par.Ref[,3])

  par.Foc[,1] <- as.character(par.Foc[,1])
  par.Foc[,2] <- as.numeric(as.character(par.Foc[,2]))
  par.Foc[,3] <- as.character(par.Foc[,3])

  for (f in 4:num.fields){
    par.Ref[,f] <-     as.numeric(as.character(par.Ref[,f]))
    par.Foc[,f] <-     as.numeric(as.character(par.Foc[,f]))
  }


  # Change model for 3PLM with fixed g #
  model <- par.Ref[,3]

  for (i in 1:test.length){

    if(model[i] == "3PLM" & fix.g == TRUE) {
      model[i] <- "3PLM.fix.g"
    } else {
      model[i] <- model[i]
    }
  }


  # Initialize matrix that stores Lord's chi-square test statistic #
  Lord.TS <- matrix(nrow=test.length, ncol=4)


  # Place focal group parameter estimates onto reference group scale using SL #
  anchor.R <- items
  anchor.F <- items
  if (same.scale == "TRUE"){
    rescale.stats <- list(par.NewFoc=par.Foc, coefficients=c(slope = 1, intercept = 0))
  }
  if (same.scale == "FALSE") {
    rescale.stats <- rescale(par.Ref, par.Foc, anchor.R, anchor.F)
  }
  par.F.t <- rescale.stats$par.NewFoc
  A <- rescale.stats$coefficients[1]

  # Convert parameter estimates to numeric values #
  num.par <- length(par.F.t[1,])-3
  for (f in 4:(num.fields)){
    par.F.t[,f] <- as.numeric(as.character(par.F.t[,f]))
  }

  # Perform Lord Chi-square test for each item #
  Lord.TS <- Lord.stats(par.Ref, par.F.t, cov.Ref, cov.Foc, test.length,
                        model, A, delta.a, delta.b, delta.c)


  if(purify == "TRUE") {
    sig <- ifelse(Lord.TS[,4] <= sig.level, 1, 0)
    anchor.R <- items[sig == 0]
    anchor.F <- items[sig == 0]
    rescale.stats <- rescale(par.Ref, par.Foc, anchor.R, anchor.F)
    par.F.t <- cbind(rescale.stats$par.NewFoc)
    A <- rescale.stats$coefficients[1]
    Lord.TS <- Lord.stats(par.Ref, par.F.t, cov.Ref, cov.Foc, test.length,
                          model, A, delta.a, delta.b, delta.c)
  } else {
    Lord.TS <- Lord.TS
  }


  # Define delta.par matrix for Wald statistics #
  delta.par <- c(delta.a, delta.b, delta.c)


  # Perform Wald test #
  Wald.out <- matrix(NA,(2*(length(par.Ref[1,])-3)), nrow=test.length)
  for (i in 1:test.length){
    K <- par.Ref[i,2]
    if(model[i] == "1PLM") {
      col.dim <- c(1,2)
    }
    else {
      col.dim <- 1:(2*(sum(!is.na(par.Ref[i,]))-3))
    }
    Wald.out[i,col.dim] <- Wald.stats(par.Ref[i,], par.F.t[i,], cov.Ref[i], cov.Foc[i],
                                      model[i], slope=A, delta.par, K)
  }

  Wald.out <- data.frame(par.Ref[,1:3],Wald.out)
  temp.names <- NA
  for (f in 1:(num.fields-3))  {
    temp.names <- c(temp.names,paste("Wald.par",f,sep=""),paste("pval.par",f,sep=""))
  }
  Wald.col.names <- c("ItemID", "NumCat", "Model", temp.names[-1])
  colnames(Wald.out) <- Wald.col.names


  # Create output #
  out.par.est.R <- par.Ref
  out.par.est.F <- par.F.t
  par.names <- paste((rep("par",(num.fields-3))),seq(1:(num.fields-3)),sep="")
  colnames(out.par.est.R) <- c("ItemID","NumCat", "Model",par.names)
  colnames(out.par.est.F) <- c("ItemID","NumCat", "Model",par.names)

  Lord.out <- cbind(par.Ref[,1:3], Lord.TS)
  Flagged <- ifelse(Lord.TS[,4] <= sig.level, "***", "")
  Lord.out <- cbind(Lord.out, Flagged)
  colnames(Lord.out) <- c("ItemID","NumCat", "Model","Lord's_Chisquare","df","NCP","p-value","Flagged")

  out.stats <- list(par.est.Ref=out.par.est.R, par.est.Focal.t=out.par.est.F, Lord.TS=Lord.out, Wald.TS=Wald.out)

  return(out.stats)

}

