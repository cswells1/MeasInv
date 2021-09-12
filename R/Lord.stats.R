Lord.stats <- function(par.Ref, par.Foc, cov.Ref, cov.Foc, test.length,
                       model=model, slope=1, delta.a=0, delta.b=0, delta.c=0){
  
  Lord.TS <- matrix(nrow=test.length, ncol=4) # Initialize matrix that contains test statistics #
  
  for (i in 1:test.length){
    
    K <- par.Ref[i,2]
    
    Lord.TS[i,] <- switch(model[i],
                          "3PLM"=Lord.chi.3pl(par.Ref[i,4:6], par.Foc[i,4:6], cov.Ref[i], cov.Foc[i],
                                             slope, delta.a, delta.b, delta.c),
                          #"3PLM.fix.g"=Lord.chi.3pl.fix.g(par.Ref[i,-c(1:2)], par.Foc[i,-c(1:2)], cov.Ref[i,], cov.Foc[i,],
                          #                                slope, delta.a, delta.b),
                          "3PLM.fix.g"=Lord.chi.2pl(par.Ref[i,4:5], par.Foc[i,4:5], cov.Ref[[i]][1:2,1:2], cov.Foc[[i]][1:2,1:2],
                                                          slope, delta.a, delta.b),
                          "2PLM"=Lord.chi.2pl(par.Ref[i,4:5], par.Foc[i,4:5], cov.Ref[i], cov.Foc[i],
                                              slope, delta.a, delta.b),
                          "1PLM"=Lord.chi.1pl(par.Ref[i,5], par.Foc[i,5], cov.Ref[i], cov.Foc[i],
                                                 slope, delta.b),
                          "GPCM"=Lord.chi.gpcm(par.Ref[i,4:(3+K)], par.Foc[i,4:(3+K)], cov.Ref[i], cov.Foc[i],
                                               slope, delta.a, delta.b),
                          "GRM"=Lord.chi.grm(par.Ref[i,4:(3+K)], par.Foc[i,4:(3+K)], cov.Ref[i], cov.Foc[i],
                                             slope, delta.a, delta.b),
                          "PCM"=Lord.chi.pcm(par.Ref[i,4:(2+K)], par.Foc[i,4:(2+K)], cov.Ref[i], cov.Foc[i],
                                             slope, delta.b))
    
  }
  
  return(Lord.TS)
  
}

