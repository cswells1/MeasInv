#' Standardized Mean Difference (SMD) for DIF detection
#'
#' @param data numeric: either the data matrix only, or the data matrix plus the vector of group membership.
#' @param group factor:  the vector of group membership
#' @param focal.name numeric or character: a single value
#' @param ref.name numeric or character: a single value
#' @param purify logical: Default is FALSE.
#'
#' @return The data frame of the output of SMD has a number of rows equal to the number of items.
#' The columns include the SMD,  SMD/Max_Cat, and  DIF_Classification.
#' SMD is the standardized mean difference estimate. SMD/Max_Cat is the SMD divided by maximum item score.
#' DIF_Classification has three effect sizes (“Negligible”, “Moderate”, or “Large”).
#' The effect size of an item is classified as “negligible” if SMD/Max_Cat < .05,
#' “moderate” .05 ≤ SMD/Max_Cat < .10, or “Large” SMD/Max_Cat  ≥ .10

#' @export
#'

SMD <- function(data,group,focal.name,ref.name,purify){
  test.length <- length(data[1,])
  raw.score.F <- apply(data[group==focal.name,],1,sum)
  raw.score.R <- apply(data[group==ref.name,],1,sum)
  uniq.scores <- sort(unique(c(raw.score.F,raw.score.R)))
  min.score.F <- min(raw.score.F)
  min.score.R <- min(raw.score.R)
  max.score.F <- max(raw.score.F)
  max.score.R <- max(raw.score.R)
  min.score <- min(uniq.scores)
  max.score <- max(uniq.scores)
  num.scores <- max.score - min.score + 1
  w.F <- matrix(-999,num.scores,ncol=1)
  for (j in 1:num.scores){
    w.F[j] <- sum(raw.score.F==uniq.scores[j])/length(raw.score.F)
  }
  SMD.est <- matrix(-999,test.length,ncol=1)
  data.F <- data[group == focal.name,]
  data.R <- data[group == ref.name,]
  for (i in 1:test.length){
    EY.F <- matrix(-999, num.scores, ncol=1)
    EY.R <- matrix(-999, num.scores, ncol=1)
      for (j in 1:num.scores){
        EY.F[j] <- mean(data.F[raw.score.F==uniq.scores[j],i], na.rm = T)
        EY.R[j] <- mean(data.R[raw.score.R==uniq.scores[j],i], na.rm = T)
      }
  SMD.est[i] <- round(sum(w.F*(EY.F-EY.R),na.rm=T),3)
  }
  max.item.score <- apply(data,2,max)
  min.item.score <- apply(data,2,min)
  SMD.t <- round(SMD.est/(max.item.score-min.item.score),3)
  	flag <- ifelse(abs(SMD.t) >= .05,1,0)
	items <- seq(1,test.length)
  	nonDIF <- items[flag==0]
	if(purify==TRUE){
	  anchor.items <- sort(unique(c(nonDIF,i)))
	}
  if(purify==FALSE){
    anchor.items <- items
  }
  raw.score.F <- apply(data[group==focal.name,anchor.items],1,sum)
  raw.score.R <- apply(data[group==ref.name,anchor.items],1,sum)
  uniq.scores <- sort(unique(c(raw.score.F,raw.score.R)))
  min.score.F <- min(raw.score.F)
  min.score.R <- min(raw.score.R)
  max.score.F <- max(raw.score.F)
  max.score.R <- max(raw.score.R)
  min.score <- min(uniq.scores)
  max.score <- max(uniq.scores)
  num.scores <- max.score - min.score + 1
  w.F <- matrix(-999,num.scores,ncol=1)
  for (j in 1:num.scores){
    w.F[j] <- sum(raw.score.F==uniq.scores[j])/length(raw.score.F)
  }
  SMD.est <- matrix(-999,test.length,ncol=1)
  data.F <- data[group == focal.name,]
  data.R <- data[group == ref.name,]
  for (i in 1:test.length){
    EY.F <- matrix(-999, num.scores, ncol=1)
    EY.R <- matrix(-999, num.scores, ncol=1)
      for (j in 1:num.scores){
        EY.F[j] <- mean(data.F[raw.score.F==uniq.scores[j],i], na.rm = T)
        EY.R[j] <- mean(data.R[raw.score.R==uniq.scores[j],i], na.rm = T)
      }
  SMD.est[i] <- round(sum(w.F*(EY.F-EY.R),na.rm=T),3)
  }
  max.item.score <- apply(data,2,max)
  min.item.score <- apply(data,2,min)
  SMD.t <- round(SMD.est/(max.item.score-min.item.score),3)
  class.DIF <- matrix("Large",test.length,ncol=1)
  class.DIF <- ifelse(abs(SMD.t) < .10, "Moderate", class.DIF)
  class.DIF <- ifelse(abs(SMD.t) < .05, "Negligible", class.DIF)
  out.stats <- data.frame(SMD.est,SMD.t,class.DIF)
  rownames(out.stats) <- paste("item",seq(1,test.length))
  colnames(out.stats) <- c("SMD","SMD/Max_Cat","DIF_Classification")
  return(out.stats)
}

#Dorans, N. J. & Holland, P. W. (1993). DIF detection and description: Mantel-Haenszel and standardization. In P. W. Holland & H. Wainer (Eds.), Differential item functioning (pp. 35-66). Hillsdale, NJ: Erlbaum.
#Dorans, N. J. & Kulick, E. (1986). Demonstrating the utility of the standardization approach to assessing unexpected differential item performance on the Scholastic Aptitude Test. Journal of Educational Measurement, 23, 355-368.
#Dorans, N. J. & Schmitt, A. P. (1993). Constructed response and differential item functioning: A pragmatic approach. In R. E. Bennett & W. C. Ward (Eds.), Construction versus choice in cognitive measurement: Issues in constructed response, performance testing, and portfolio assessment (pp. 135-165). Hillsdale, NJ: Erlbaum.
#Dorans, N. J., Schmitt, A. P., & Bleistein, C. A. (1988). The standardization approach to assessing differential speededness (Research Report No. 88-31). Princeton, NJ: Educational Testing Services.
#Zenisky, A. L., Hambleton, R. K., & Robin, F. (2003). Detection of differential item functioning in large-scale state assessments: A study evaluating a two-stage approach. Educational and Psychological Measurement, 63, 51-64.

