#' Simulate from a Ball Distribution
#'
#' Produces samples from the uniform distribution on the p-ball.
#' @details The ball distribution is the distribution of Z where
#' \deqn{Z=UX,}
#' where U is the inverse p-th root of a uniform distribution on \eqn{[0,1]} and X is uniform on the (p-1)-sphere which is generated from \code{\link{sphere_distribution}}.
#' @param n number of samples
#' @param p ball dimension
#' @keywords MKdepth
#' @return `n` by `p` matrix with one sample in each row.
#' @author Leo Meurice
#' @seealso \code{\link{sphere_distribution}}
#' @importFrom stats runif
#' @export
#' @examples
#' set.seed(1)
#' u = ball_distribution(1000,2)
#' plot(u)

ball_distribution= function(n,p){
  if(n<1|p<1){stop("n and p must be positive integers")}
  X=sphere_distribution(n,p)
  U=runif(n)^(1/p)
  X=U*X
  return(X)
}
