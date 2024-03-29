#' Dichotomous Response Model Probabilities
#'
#' @description This function computes the probability of correct answers for one or more items for a given set of theta values
#' using the IRT 1PL, 2PL, and 3PL models.
#'
#' @param theta A vector of ability values.
#' @param a A vector of item discrimination (or slope) parameters.
#' @param b A vector of item difficulty (or threshold) parameters.
#' @param g A vector of item guessing parameters.
#' @param D A scaling factor in IRT models to make the logistic function as close as possible to the normal ogive function (if set to 1.7).
#'          Default is 1.
#'
#' @details \code{g} does not need to be specified when the response probabilities of the 1PL and 2PL models are computed.
#'
#' @return This function returns a vector or matrix. When a matrix is returned, rows indicate theta values and columns represent items.
#'
#' @author Hwanggyu Lim \email{hglim83@@gmail.com}
#'
#' @seealso \code{\link{plm}}
#'
#' @examples
#' ## when vectors are used for both theta values and item parameters (3PLM)
#' drm(c(-0.1, 0.0, 1.5), a=c(1, 2), b=c(0, 1), g=c(0.2, 0.1), D=1)
#'
#' ## when vectors are only used for item parameters (2PLM)
#' drm(0.0, a=c(1, 2), b=c(0, 1), D=1)
#'
#' ## when vectors are only used for theta values (3PLM)
#' drm(c(-0.1, 0.0, 1.5), a=1, b=1, g=0.2, D=1)
#'
drm <- function(theta, a, b, g=NULL, D=1) {

  # check the numbers of examinees and items
  nstd <- length(theta)
  nitem <- length(a)

  # check the guessing parmaters
  if(is.null(g)) g <- rep(0, nitem)

  # when both the numbers of examiness and items are greater than 1
  if(nstd > 1 & nitem > 1) {
    a <- matrix(a, nrow=nstd, ncol=nitem, byrow=TRUE)
    b <- matrix(b, nrow=nstd, ncol=nitem, byrow=TRUE)
    g <- matrix(g, nrow=nstd, ncol=nitem, byrow=TRUE)
  }

  # calculate probability of correct answer
  Da <- D * a
  z <- Da * (theta - b)
  P <- g + (1 - g) / (1 + exp(-z))

  P

}

#' Polytomous Response Model Probabilities (GRM and GPCM)
#'
#' @description This function computes the probability of selecting a specific category for an item
#' for a given set of theta values using the graded response model, partial credit model, and generalized
#' partial credit model.
#'
#' @param theta A vector of ability values.
#' @param a A numeric value of item discrimination (or slope) parameter.
#' @param d A vector of item threshold (or step) parameters.
#' @param D A scaling factor in IRT models to make the logistic function as close as possible to the normal ogive function  (if set to 1.7).
#' Default is 1.
#' @param pmodel A character string indicating the polytomous model being used. Available models are "GRM" for
#' the the graded response model and "GPCM" for the (generalized) partial credit model.
#'
#' @details When the category probabilities are computed for an item with the partial credit model, \code{a = 1} for that item.
#' When \code{pmodel = "GPCM"}, \code{d} should include step parameters. Item step parameters are the overall item
#' difficulty (or location) parameter subtracted by the difficulty (or threshold) parameter for each category. Thus, the number of step parameters
#' for an item with m categories is m-1 because a step parameter for the first category does not affect the category probabilities. For example,
#' if an item has five categories under the (generalized) partial credit model, four step parameters should be specified. For more details about
#' the parameterization of the (generalized) partial credit model, see \code{\link{irtlink}}.
#'
#' @return This function returns a vector or matrix. When a matrix is returned, rows indicate theta values and columns represent
#' categories of an item.
#'
#' @author Hwanggyu Lim \email{hglim83@@gmail.com}
#'
#' @seealso \code{\link{drm}}, \code{\link{irtlink}}
#'
#' @examples
#' ## Category probabilities for an item with four categories
#' ## using a generalized partial credit model
#' plm(theta=c(-0.2, 0, 0.5), a=1.4, d=c(-0.2, 0, 0.5), D=1, pmodel='GPCM')
#'
#' ## Category probabilities for an item with five categories
#' ## using a graded response model
#' plm(theta=c(-0.2, 0, 0.5), a=1.2, d=c(-0.4, -0.2, 0.4, 1.5), D=1, pmodel='GRM')
#'
plm <- function(theta, a, d, D=1, pmodel=c("GRM", "GPCM")) {

  pModel <- toupper(pmodel)
  model <- match.arg(pmodel)
  switch(model,
         GRM = grm(theta=theta, a=a, d=d, D=D),
         GPCM = gpcm(theta=theta, a=a, d=d, D=D)
  )

}


# IRT GPC model
gpcm <- function (theta, a, d, D = 1) {

  # include zero for the step parameter of the first category
  d <- c(0, d)

  # check the numbers of examinees and items
  nstd <- length(theta)

  # replicate a parameters
  a <- rep(a, nstd)

  # create a matrix for step parameters
  d <- matrix(d, nrow=nstd, ncol=length(d), byrow=TRUE)

  # calculate category probabilities
  z <- D * a * (theta - d)
  numer <- exp(t(apply(z, 1, cumsum))) # numerator
  denom <- rowSums(exp(t(apply(z, 1, cumsum)))) # denominator
  P <- numer / denom

  if(nstd == 1) P <- as.numeric(P)

  P

}

# IRT GRM model
#' @import purrr
grm <- function(theta, a, d, D = 1) {

  # check the number of step parameters
  m <- length(d)

  # check the numbers of examinees and items
  nstd <- length(theta)

  # calculate all the probabilities greater than equal to each threshold
  allP <- purrr::map_dfc(d, drm, theta=theta, a=a, g=0, D=D)

  # all possible scores
  allScores <- seq(0, m)

  # calculate category probabilities
  p1 <- 1 - allP[, 1]
  p2 <- allP[, 1:(max(allScores)-1)] - allP[, (1:(max(allScores)-1))+1]
  p3 <- allP[, max(allScores)]
  P <- as.matrix(cbind(p1, p2, p3))

  if(nstd == 1) {
    P <- as.numeric(P)
  } else {
    P <- unname(P)
  }

  P

}



