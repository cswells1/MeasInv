#' Transform the IRT Parameters
#'
#' @description This function transforms item parameters using estimated IRT linking coefficients.
#'
#' @param x A data.frame containing the item meta data (e.g., item parameters, number of categories, models ...). This data.frame
#' can be easily obtained using the function \code{\link{shape_df}}. See \code{\link{preplink}}, \code{\link{test.info}},
#' or \code{\link{simdat}} for more details about the item meta data.
#' @param constants A vector of length two containing the IRT linking coefficients (i.e., slope and intercept).
#' The first value is a slope and the second value is an intercept.
#'
#' @return This function returns a data.frame containing the transformed item parameters.
#'
#' @author Hwanggyu Lim \email{hglim83@@gmail.com}
#'
#' @examples
#' ## IRT liking using single-format tests
#' # use "KBneat" data set
#' dat_base <- KBneat$item$dat.base
#' dat_new <- KBneat$item$dat.new
#'
#' # specify common items
#' common.b <- KBneat$common$common.b
#' common.n <- KBneat$common$common.b
#'
#' # prepare a data set for IRT linking
#' x <- preplink(dat_base, dat_new, common.b, common.n)
#'
#' # estimate IRT linking coefficients using the Mean/Sigma method
#' rst <- irtlink(x, method.link = 'SL', D = 1.7)
#'
#' # extract the estimated linking coefficients
#' constants <- takelink(rst, "coefficients")
#'
#' # transform the item parameters in the new test form
#' transpar(dat_new, constants)
#'
#' @import purrr
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
transpar <-function(x, constants) {

  A <- constants[1]
  B <- constants[2]

  # chagne all factor variables into character variables
  x <- purrr::modify_if(x, is.factor, as.character)

  # check the maximum of categories
  max.cat <- max(x[, 2])

  # give column names
  if(is.null(x$anchor)) {
    colnames(x) <- c("id", "cats", "model", paste0("par.", 1:(ncol(x) - 3)))
  } else {
    colnames(x) <- c("id", "cats", "model", paste0("par.", 1:(ncol(x) - 4)), "anchor")
  }

  # transfer item parameters
  var.num <- which(names(x) %in% paste0("par.", 3:100))
  x_new <- x %>%
    dplyr::mutate_at(.vars=dplyr::vars(4), .funs=function(k) k / A) %>%
    dplyr::mutate_at(.vars=dplyr::vars(5), .funs=function(k) (k * A) + B) %>%
    dplyr::mutate_at(.vars=dplyr::vars(var.num),
                     .funs=function(k) dplyr::case_when(x[, 2]  > 2  ~ (k *  A) + B,
                                                        x[, 2]  == 2  ~ k))

  # return the result
  x_new

}


# Transfer item parameters using estimated IRT linking coefficients
# This function is used only for the estimation of linking coefficients
## Arg
# meta: a listrized item meta data
#' @import purrr
#' @import dplyr
transpar2 <-function(meta, constants) {

  A <- constants[1]
  B <- constants[2]

  if(!is.null(meta$drm)) {

    meta$drm$a <- meta$drm$a / A
    meta$drm$b <- (meta$drm$b * A) + B

  }

  if(!is.null(meta$plm)) {

    meta$plm$a <- meta$plm$a / A
    meta$plm$d <- purrr::map(.x=meta$plm$d, .f=function(x) (x * A) + B)

  }

  # return the result
  meta

}

