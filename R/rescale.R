#' IRT Linking to Transform Scale of Parameters
#'
#' @description This function implements various IRT linking methods to transform the IRT scale of the new test form to the
#' IRT scale of the base test form based on unidimensional IRT models using Stocking-Lord method.
#' @param par.Ref
#' @param par.Foc
#' @param anchor.R
#' @param anchor.F
#'
#' @return A list of internal objects.
#' @author Hwanggyu Lim \email{hglim83@@gmail.com}
#'
rescale <- function(par.Ref, par.Foc, anchor.R, anchor.F) {

  # prepare rescaling
  x <- preplink(dat.base=par.Ref, dat.new=par.Foc, common.b=anchor.R, common.n=anchor.F)

  # Extract parameter values for tatal set and anchor set
  total.base <- x$group.b$total.set
  total.new <- x$group.n$total.set
  anc.base <- x$group.b$anchor.set
  anc.new <- x$group.n$anchor.set

  # check the maximum of category numbers
  max.cat.b <- max(anc.base[, 2])
  max.cat.n <- max(anc.new[, 2])

  # modify the columns of item meta data.frame
  if(all(anc.base[, 2] == 2)) {
    anc.base <- anc.base[, c(1:6, ncol(anc.base))]
  }
  if(all(anc.new[, 2] == 2)) {
    anc.new <- anc.new[, c(1:6, ncol(anc.new))]
  }
  if(max.cat.b > 2) {
    anc.base <- anc.base[, c(1:(3 + max.cat.b), ncol(anc.base))]
  }
  if(max.cat.n > 2) {
    anc.new <- anc.new[, c(1:(3 + max.cat.n), ncol(anc.new))]
  }

  # number of common items
  n.common <- nrow(anc.base)

  # listrize the item meta data for common items
  metalist.b <- metalist(anc.base)
  metalist.n <- metalist(anc.new)

  # Weights and nodes for IRT equating: use of standard normal dist'n
  nodes <- seq(-6, 6, 0.1)
  wts <- stats::dnorm(nodes, mean=0, sd=1)
  wts <- wts / sum(wts)
  weights.b <- data.frame(theta=nodes, weight=wts)

  # IRT scaling
  startvals <- c(1, 0)
  constants <- stats::nlminb(startvals, SL, meta.b=metalist.b, meta.n=metalist.n,
                             D=1, weights.b=weights.b)$par
  names(constants) <- c("Slope", "Intercept")

  # Rescale the item parameters in New form
  total.renew <- transpar(x=total.new, constants)

  # Return results
  rst <- list('coefficients' = constants, par.Ref=total.base[, -ncol(total.base)],
              par.Foc=total.new[, -ncol(total.new)], par.NewFoc=total.renew[, -ncol(total.renew)])

  rst

}

# Stocking-Lord method
SL <- function(startvals, meta.b, meta.n, D=1, weights.b){

  # Stocking-Lord
  meta.b <- meta.b
  meta.n <- transpar2(meta.n, startvals)

  # compute TCCs of common items
  tcc.b <- trace2(meta=meta.b, theta=weights.b[, 1], D = D, type="tcc")$trace
  tcc.n <- trace2(meta=meta.n, theta=weights.b[, 1], D = D, type="tcc")$trace

  # compute a weighted sum of SLdiff
  rr <- sum((tcc.b - tcc.n)^2 * weights.b[, 2])

  rr

}

