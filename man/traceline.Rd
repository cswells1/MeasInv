% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trace.R
\name{traceline}
\alias{traceline}
\title{Compute Item/Test Characteristic Functions}
\usage{
traceline(x, theta, D = 1)
}
\arguments{
\item{x}{A data.frame containing the item meta data (e.g., item parameters, number of categories, models ...).
See \code{\link{irtfit}}, \code{\link{test.info}}, or \code{\link{simdat}} for more details about the item meta data.
This data.frame can be easily obtained using the function \code{\link{shape_df}}.}

\item{theta}{A vector of theta values.}

\item{D}{A scaling factor in IRT models to make the logistic function as close as possible to the normal ogive function (if set to 1.7).
Default is 1.}
}
\value{
This function returns an object of class \code{\link{traceline}}. This object contains a list containing
the item category probabilities, item characteristic function, and test characteristic function.
}
\description{
This function computes the item category probabilities, item characteristic function, and
test characteristic function given a set of theta values. The returned object of this function can be used
to draw the item or test characteristic curve using the function \code{\link{plot.traceline}}.
}
\examples{
## example
## using a "-prm.txt" file obtained from a flexMIRT
flex_prm <- system.file("extdata", "flexmirt_sample-prm.txt", package = "irtplay")
test_flex <- bring.flexmirt(file=flex_prm, "par")$Group1$full_df

# set theta values
theta <- seq(-3, 3, 0.5)

# compute the item category probabilities and item/test
# characteristic functions given the theta values
traceline(x=test_flex, theta, D=1)

}
\seealso{
\code{\link{plot.traceline}}
}
\author{
Hwanggyu Lim \email{hglim83@gmail.com}
}
