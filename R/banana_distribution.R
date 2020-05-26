#' Simulate from a Banana Distribution
#'
#' Produces samples from a banana distribution in two dimensions.
#'
#' @details
#' The banana distribution is the distribution of the vector \deqn{(X + R cos \phi,height*X^2 + R sin \phi),}
#'where X is uniform on \eqn{[-1, 1]}, \eqn{\phi} is uniform on \eqn{[0, 2\pi]}, Z is uniform on \eqn{[0, thick]}, X, Z and \eqn{\phi}
#'are independent, and R = 0.2Z(1 + (1 - |X|)/2).
#' @param n number of samples.
#' @param height height of the banana. By default \code{height=1}
#' @param thick thickness of the banana. By default \code{thick=1}
#' @keywords MKdepth
#' @return `n` by `2` matrix with one sample in each row.
#' @author Leo Meurice
#' @importFrom Rdpack reprompt
#' @importFrom stats runif
#' @references
#' \insertRef{chernozhukov2017monge}{MKdepth}
#' @export
#' @examples
#' set.seed(1)
#' u = banana_distribution(1000)
#' plot(u)
#'
#' set.seed(1)
#' u = banana_distribution(1000,4,.5)
#' plot(u)

banana_distribution=function(n,height=1,thick=1){
  if(n<0){stop("n must be a positive integer")}
  if(thick<0){stop("thick must be positive")}
  X=runif(n,min=-1,max=1)
  phi=runif(n,min=0,max=2*pi)
  Z=runif(n,min=0,max=thick)
  R=0.2*Z*(1+(1-abs(X))/2)
  data=matrix(c(X+R*cos(phi),height*X^2+R*sin(phi)),nrow=n,ncol=2)
  return(data)
}
