#' Data Preparation for IRT Linking
#'
#' @description This function creates an object of class \code{prelink} that is used with the
#' \code{\link{irtlink}}, \code{\link{irteq}}, \code{\link{scatter}}, and \code{\link{plot.preplink}}
#' functions.
#'
#' @param dat.base A data.frame containing the item meta data in the base test form (e.g., item parameter,
#' categories, models ...). This data.frame can be easily obtained using the function \code{\link{shape_df}}.
#' See below for details.
#' @param dat.new A data.frame containing meta data for items in the new test form (e.g., item parameter,
#' categories, models ...). This data.frame can be easily obtained using the function \code{\link{shape_df}}.
#' See below for details.
#' @param common.b A numeric vector specifying the position (or location) of common items on the base test form.
#' @param common.n A numeric vector specifying the position (or location) of common items on the new test form.
#'
#' @details A specific form of the data.frame should be specified in \code{dat.base} and \code{dat.new}. In each argument,
#' the first column should have item IDs, the second column should contain the number of score categories for the items, and the third
#' column should include IRT models. The available IRT models are "1PLM", "2PLM", "3PLM", and "DRM" for dichotomous items, and
#' "GRM" and "GPCM" for polytomous items. Note that "DRM" covers all dichotomous IRT models (i.e, "1PLM", "2PLM", and "3PLM") and
#' "GRM" and "GPCM" represent the graded response model and (generalized) partial credit model, respectively. From the fourth column,
#' item parameters should be included. For dichotomous items, the fourth, fifth, and sixth columns represent the item discrimination
#' (or slope), item difficulty, and item guessing parameters, respectively. When "1PLM" or "2PLM" is specified for any items in the
#' third column, NAs should be inserted for the item guessing parameters. For polytomous items, the item discrimination (or slope)
#' parameters should be contained in the fourth column and the item threshold (or step) parameters should be included from the fifth
#' to the last column. When the number of categories differs between items, the empty cells of item parameters should be filled with NAs.
#' In this package, item step parameters should be used for the (generalized) partial credit model. Item step parameters are the overall
#' item difficulty (or location) parameter subtracted by the difficulty (or threshold) parameter for each category. Thus, the number of step
#' parameters for an item with m categories is m-1 because a step parameter for the first category does not affect the category probabilities.
#' For example, if an item has five categories under the (generalized) partial credit model, four step parameters should be specified.
#' An example of a data.frame with a single-format test is as follows:
#' \tabular{lrlrrrrr}{
#'   ITEM1  \tab 2 \tab 1PLM \tab 1.000 \tab  1.461 \tab         NA \cr
#'   ITEM2  \tab 2 \tab 2PLM \tab 1.921 \tab -1.049 \tab         NA \cr
#'   ITEM3  \tab 2 \tab 3PLM \tab 1.736 \tab  1.501 \tab  0.203 \cr
#'   ITEM4  \tab 2 \tab 3PLM \tab 0.835 \tab -1.049 \tab  0.182 \cr
#'   ITEM5  \tab 2 \tab DRM \tab 0.926 \tab  0.394 \tab  0.099
#' }
#' And an example of a data.frame for a mixed-format test is as follows:
#' \tabular{lrlrrrrr}{
#'   ITEM1  \tab 2 \tab 1PLM \tab 1.000 \tab  1.461 \tab         NA \tab         NA \tab         NA\cr
#'   ITEM2  \tab 2 \tab 2PLM \tab 1.921 \tab -1.049 \tab         NA \tab         NA \tab         NA\cr
#'   ITEM3  \tab 2 \tab 3PLM \tab 0.926 \tab  0.394 \tab  0.099 \tab         NA \tab         NA\cr
#'   ITEM4  \tab 2 \tab DRM \tab 1.052 \tab -0.407 \tab  0.201 \tab         NA \tab         NA\cr
#'   ITEM5  \tab 4 \tab GRM  \tab 1.913 \tab -1.869 \tab -1.238 \tab -0.714 \tab         NA \cr
#'   ITEM6  \tab 5 \tab GRM  \tab 1.278 \tab -0.724 \tab -0.068 \tab  0.568 \tab  1.072\cr
#'   ITEM7  \tab 4 \tab GPCM  \tab 1.137 \tab -0.374 \tab  0.215 \tab  0.848 \tab         NA \cr
#'   ITEM8  \tab 5 \tab GPCM  \tab 1.233 \tab -2.078 \tab -1.347 \tab -0.705 \tab -0.116
#' }
#' Although the base and new test forms contain the same common items, the position (or location) of common items could differ by test form.
#' Therefore, it is important to accurately match the position of the same common item for both test forms in the arguments \code{common.b} and
#' \code{common.n}. For example, suppose that three common items on the base test form are located in the 7th, 9th, and 11th positions and the same common
#' items on the new test form are located in the 2nd, 3rd, and 10th positions. Specifically, the 7th item in the base form is matched with the 2nd item
#' in the new form, the 9th item in the base form is matched with the the 3rd item in the new form, and the 11th item in the base form is matched with
#' the the 10th item in the new form. The arguments to indicate that those common items are the same across the forms (in spite of having different positions)
#' would be \code{common.b = c(7, 9, 11)} and \code{common.n = c(2, 3, 10)}.
#'
#' Note that when using the function \code{\link{irtlink}} in this package, the scale of item parameters from the new test form are converted onto the the
#' scale of the item parameters of the base test form.
#'
#' @return This function returns an object of class \code{\link{preplink}}.
#'
#' @author Hwanggyu Lim \email{hglim83@@gmail.com}
#'
#' @seealso \code{\link{shape_df}}, \code{\link{irtlink}}, \code{\link{irteq}}, \code{\link{scatter}}, \code{\link{plot.preplink}}
#'
#' @examples
#' # read 'prm' files of flexMIRT
#' base <- system.file("extdata", "flexmirt_base-prm.txt", package = "irteQ")
#' new <- system.file("extdata", "flexmirt_new-prm.txt", package = "irteQ")
#' dat_base <- bring.flexmirt(file=base, "par")$Group1$full_df
#' dat_new <- bring.flexmirt(file=new, "par")$Group1$full_df
#'
#' # specify common items
#' common.b <- c(41:55)
#' common.n <- c(41:55)
#'
#' # prepare a set of data for the IRT linking
#' preplink(dat_base, dat_new, common.b, common.n)
#'
#' @import purrr
#' @import dplyr
#'
preplink <- function(dat.base, dat.new, common.b, common.n) {

  if(length(common.b) != length(common.n)) {
    stop("Length of common items for both forms are different.", call.=FALSE)
  }
  if(length(unique(common.b)) != length(common.b)) {
    stop("Some common item positions for the base form are over overlapped.", call.=FALSE)
  }
  if(length(unique(common.n)) != length(common.n)) {
    stop("Some common item positions for the new form are over overlapped.", call.=FALSE)
  }

  # chagne all factor variables into character variables
  dat.base <- purrr::modify_if(dat.base, is.factor, as.character)
  dat.new <- purrr::modify_if(dat.new, is.factor, as.character)

  # give column names
  colnames(dat.base) <- c("id", "cats", "model", paste0("par.", 1:(ncol(dat.base) - 3)))
  colnames(dat.new) <- c("id", "cats", "model", paste0("par.", 1:(ncol(dat.new) - 3)))

  # make a list
  setList <- list(group.b=NULL, group.n=NULL)
  groups <- list(group.b=NULL, group.n=NULL)
  groups$group.b <- list(param=dat.base, common=common.b)
  groups$group.n <- list(param=dat.new, common=common.n)

  for(i in 1:2) {
    # Take data
    param <- groups[[i]]$param

    # Transform the type of ID and Model colums as character
    # param[,1] <- as.character(param[,1])
    # param[,3] <- as.character(param[,3])
    param <- purrr::modify_if(param, is.factor, as.character)

    # Add common item index column
    common <- groups[[i]]$common
    ncom <- length(common)
    label <- 1:ncom
    param$anchor <- rep(NA, nrow(param))
    for(j in 1:ncom) {
      param$anchor[common[j]] <- label[j]
    }

    # Creat a data including only common items
    param.com <- param[common, ]
    rownames(param.com) <- 1:ncom

    setList[[i]] <- list(total.set = param, anchor.set = param.com)

  }

  class(setList) = "preplink"
  setList

}


