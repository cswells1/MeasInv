plot.ICC <- function(par.R, par.F, item, D=1){
  
  theta <- seq(-3,3,by=.1)
  
  probs.R <- traceline(par.R, theta, D)$icc
  probs.F <- traceline(par.F, theta, D)$icc
  if(par.R[item,3]=="GRM" | par.R[item,3]=="GPCM" | par.R[item,3]=="PCM"){
    dev.new(width = 8, height = 8)
    par(family="serif")
    par(mfrow=c(2,1))
    plot(theta, probs.R[,item], xlim=c(-3,3), ylim=c(0,max(c(probs.R,probs.F))), xlab="Proficiency", ylab="Item Score", type="l", main=paste("Item ",item,sep=""))
    lines(theta, probs.F[,item], lty=2)
    legend(-3, max(c(probs.R,probs.F)), lty=c(1:2), legend=c("Reference", "Focal"), bty="n")
    diff.RF <- probs.R[,item]-probs.F[,item]
    min.diff <- -max(abs(diff.RF))
    max.diff <- max(abs(diff.RF))
    plot(theta, diff.RF, xlim=c(-3,3), ylim=c(min.diff, max.diff), xlab="Proficiency", ylab="R - F", type="l", lty=2)
    abline(0,0)
  }
  else {
    dev.new(width = 8, height = 8)
    par(family="serif")
    par(mfrow=c(2,1))
    plot(theta, probs.R[,item], xlim=c(-3,3), ylim=c(0,1), xlab="Proficiency", ylab="Probability", type="l", main=paste("Item ",item,sep=""))
    lines(theta, probs.F[,item], lty=2)
    legend(-3, 1, lty=c(1:2), legend=c("Reference", "Focal"), bty="n")
    diff.RF <- probs.R[,item]-probs.F[,item]
    min.diff <- -max(abs(diff.RF))
    max.diff <- max(abs(diff.RF))
    plot(theta, diff.RF, xlim=c(-3,3), ylim=c(min.diff, max.diff), xlab="Proficiency", ylab="R - F", type="l", lty=2)
    abline(0,0)
  }

}


