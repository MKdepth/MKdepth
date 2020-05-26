#' Simulate from a Sphere Distribution
#'
#' Produces samples from the uniform distribution on the p-sphere.
#'
#' @details The sphere distribution is the distribution of Y where
#' \deqn{Y = \frac{X}{\Vert X \Vert},}
#' where X follows a multinomial distribution.
#' @param n number of samples
#' @param p sphere dimension
#' @keywords MKdepth
#' @return `n` by `p` matrix with one sample in each row.
#' @author Leo Meurice
#' @importFrom  stats rnorm
#' @export
#' @examples
#' u = sphere_distribution(100,2)
#' plot(u)


sphere_distribution= function(n,p){
  if(n<1|p<1){stop("n and p must be positive integers")}
  X=matrix(rnorm(n*p),n)
  X=X/sqrt(rowSums(X^2))
  return(X)
}
