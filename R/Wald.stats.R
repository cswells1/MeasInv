Wald.stats <- function(par.Ref, par.Foc, cov.Ref, cov.Foc, model, slope, delta.par, K){
  
    Wald.out <- switch(model,
                       "3PLM"=Wald.3pl(par.Ref[,-c(1:3)], par.Foc[,-c(1:3)], cov.Ref, cov.Foc, slope, delta.par),
                       "3PLM.fix.g"=Wald.3pl.fix.g(par.Ref[,-c(1:3)], par.Foc[,-c(1:3)], cov.Ref[[1]][1:2,1:2], cov.Foc[[1]][1:2,1:2], slope, delta.par),
                       "2PLM"=Wald.2pl(par.Ref[,-c(1:3)], par.Foc[,-c(1:3)], cov.Ref, cov.Foc, slope, delta.par),
                       "1PLM"=Wald.1pl(par.Ref[,-c(1:3)], par.Foc[,-c(1:3)], cov.Ref, cov.Foc, slope, delta.par),
                       "GPCM"=Wald.gpcm(par.Ref[,-c(1:3)], par.Foc[,-c(1:3)], cov.Ref, cov.Foc, slope, delta.par, K),
                       "GRM"=Wald.grm(par.Ref[,-c(1:3)], par.Foc[,-c(1:3)], cov.Ref, cov.Foc, slope, delta.par, K),
                       "PCM"=Wald.pcm(par.Ref[,-c(1:3)], par.Foc[,-c(1:3)], cov.Ref, cov.Foc, slope, delta.par, K))
    
  return(Wald.out)
  
}

