#' Mantel–Haenszel DIF Statistics for polytomous data
#'
#' @param data numeric: either the data matrix only, or the data matrix plus the vector of group membership.
#' @param group factor: the vector of group membership
#' @param focal.name numeric or character: a single value
#' @param ref.name numeric or character: a single value
#' @param sig.level numeric: the significance level
#' @param purify logical: purification for the anchor
#'
#' @return The data frame of the output has a number of rows equla to the number of items.
#' The column include the Mantel_Chi_Square (Mantel Chi-square statistic)
#' and p-value (p-value for the Mantel Chi-square statistic).
#' @export
#'

Mantel.poly <- function(data, group, focal.name, ref.name, sig.level, purify){
  test.length <- length(data[1,])
  data.F <- data[group == focal.name,]
  data.R <- data[group == ref.name,]
  raw.scores <- apply(data,1,sum)
  raw.scores.F <- raw.scores[group==focal.name]
  raw.scores.R <- raw.scores[group==ref.name]
  uniq.raw.scores <- sort(unique(raw.scores))
  #max.categ <- max(data)
  max.categ <- apply(data,2,max)
  M <- sum(max.categ) - 1
  #M <- test.length*max.categ - 1
  TS <- matrix(-999,test.length,ncol=1)
  for (i in 1:test.length){
    S.Fm <- matrix(-999,M,ncol=1)
    exp.S.Fm <- matrix(-999,M,ncol=1)
    var.S.Fm <- matrix(-999,M,ncol=1)
    for (m in 1:M){
      temp.F <- data.F[raw.scores.F==m,i]
      temp.R <- data.R[raw.scores.R==m,i]
      temp <- data[raw.scores==m,i]
      c.vals.F <- as.numeric(names(table(temp.F)))
      S.Fm[m] <- sum(table(temp.F)*c.vals.F)
      N.Fm <- length(temp.F)
      N.Rm <- length(temp.R)
      N.m <- N.Fm + N.Rm
      exp.S.Fm[m] <- N.Fm*mean(temp,na.rm=T)
      var.part1 <- (N.Fm*N.Rm)/((N.m^2)*(N.m-1))
      c.vals <- as.numeric(names(table(temp)))
      N.cm <- table(temp)
      var.part2 <- N.m*(sum((c.vals^2)*N.cm)) - (sum(c.vals*N.cm))^2
      var.S.Fm[m] <- var.part1*var.part2
    }
    TS[i] <- round((sum(S.Fm,na.rm=T) - sum(exp.S.Fm,na.rm=T))^2/sum(var.S.Fm,na.rm=T),3)
  }
  pval <- round(1-pchisq(TS,1),3)
  sig <- ifelse(pval < sig.level, 1, 0)
  items <- seq(1,test.length)
  nonDIF <- items[sig==0]
  TS <- matrix(-999,test.length,ncol=1)
  for (i in 1:test.length){
    if(purify==TRUE){
      anchor.items <- sort(unique(c(nonDIF,i)))
    }
    if(purify==FALSE){
      anchor.items <- items
    }
    raw.scores <- apply(data[,anchor.items],1,sum)
    raw.scores.F <- raw.scores[group==focal.name]
    raw.scores.R <- raw.scores[group==ref.name]
    uniq.raw.scores <- sort(unique(raw.scores))
    #M <- length(anchor.items)*max.categ - 1
    max.categ <- apply(data[,anchor.items],2,max)
    M <- sum(max.categ) - 1
    S.Fm <- matrix(-999,M,ncol=1)
    exp.S.Fm <- matrix(-999,M,ncol=1)
    var.S.Fm <- matrix(-999,M,ncol=1)
    for (m in 1:M){
      temp.F <- data.F[raw.scores.F==m,i]
      temp.R <- data.R[raw.scores.R==m,i]
      temp <- data[raw.scores==m,i]
      c.vals.F <- as.numeric(names(table(temp.F)))
      S.Fm[m] <- sum(table(temp.F)*c.vals.F)
      N.Fm <- length(temp.F)
      N.Rm <- length(temp.R)
      N.m <- N.Fm + N.Rm
      exp.S.Fm[m] <- N.Fm*mean(temp,na.rm=T)
      var.part1 <- (N.Fm*N.Rm)/((N.m^2)*(N.m-1))
      c.vals <- as.numeric(names(table(temp)))
      N.cm <- table(temp)
      var.part2 <- N.m*(sum((c.vals^2)*N.cm)) - (sum(c.vals*N.cm))^2
      var.S.Fm[m] <- var.part1*var.part2
    }
    TS[i] <- round((sum(S.Fm,na.rm=T) - sum(exp.S.Fm,na.rm=T))^2/sum(var.S.Fm,na.rm=T),3)
  }
  pval <- round(1-pchisq(TS,1),3)
  out.stats <- data.frame(TS,pval)
  rownames(out.stats) <- paste("item",seq(1,test.length))
  colnames(out.stats) <- c("Mantel_Chi_Square","p-value")
  return(out.stats)
}

#Mantel, N. (1963). Chi-square tests with one degree of freedom: Extensions of the Mantel-Haenszel procedure. Journal of the American Statistical Association, 58, 690-700.
#Zenisky, A. L., Hambleton, R. K., & Robin, F. (2003). Detection of differential item functioning in large-scale state assessments: A study evaluating a two-stage approach. Educational and Psychological Measurement, 63, 51-64.
