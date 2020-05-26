#' The Monge-Kantorovich Depth of specific observations compared a data set
#'
#' Calculates the Monge-Kantorovich depth of each specific observation of a data set.
#' @param x \code{n} by \code{p} matrix of data  whose Monge-Kantorovich depth is to be calculated. Each row contains a p-variate point.
#'  Should have the same dimension as \code{data}.
#' @inheritParams MKdepth_iterative
#' @return Numerical vector of depths, one for each row in \code{x}.
#' @keywords MKdepth
#' @details
#' Call the function \code{\link{MKdepth_iterative}}.
#' @seealso \code{\link{MKdepth_iterative}}
#' @author Leo Meurice
#' @export
#' @examples
#' set.seed(1)
#' data=ball_distribution(100,2)
#' x=rbind(c(-0.2,0.1),c(-.25,-.5),c(.5,0))
#' depth=MKdepth_specific(x,data,iter=20)
#' plot(data,pch=20)
#' points(x,pch=19,col='red')
#' text(x[,1], x[,2], depth,cex=1, pos=3,col="red")


MKdepth_specific=function(x,data,trs_method="assignment",depth_method="Tukey",approx=FALSE,iter=1,eps=NULL,prec=3){
  copy=rbind(x,data)
  depth=MKdepth_iterative(copy,trs_method = trs_method,depth_method = depth_method,approx=approx,iter=iter,eps=eps,prec=prec)
  res=depth$MKdepth[1:dim(x)[1]]
  return(res)
}
