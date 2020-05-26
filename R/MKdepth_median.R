#' Monge-Kantorovich Median
#'
#' Computes the Monge-Kantorovich Median of a multivariate data set.
#'
#' @param data \code{n} by \code{p} matrix of data where each row contains a p-variate point, w.r.t which the Monge-Kantorovich depth
#' is calculated.
#' @param depth Numerical vector of depths, one for each row in \code{data}.
#' @keywords MKdepth
#' @return List containing the Monge-Kantorovich median coordinates(\code{$coordinates}) and its depth value(\code{$depth}).
#' @author Leo Meurice
#' @export
#' @examples
#' set.seed(1)
#' data=ball_distribution(1000,2)
#' depth=MKdepth(data)
#' median=MKdepth_median(data,depth)
#' plot(data,pch='.')
#' points(median$coordinates[1],median$coordinates[2],pch=19,col='red')
#'
#' set.seed(1)
#' data=matrix(rnorm(2000),1000)
#' depth=MKdepth(data)
#' median=MKdepth_median(data,depth)
#' plot(data,pch='.')
#' MKdepth_contours(data,depth,contours=5,alpha=2,plot.new=FALSE)
#' points(median$coordinates[1],median$coordinates[2],pch=19,col='red')
#'


MKdepth_median=function(data,depth){
  if(dim(data)[1]!=length(depth)){stop("Dimensions of data and depth are not the same")}
  index=seq_along(depth)[depth==max(depth)]
  if(length(index)>1L){return(list(coordinates=colSums(data[index,])/length(index),depth=max(depth)))}
  else{return(list(coordinates=data[index,],depth=depth[index]))}
}
