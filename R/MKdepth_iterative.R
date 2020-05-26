#' The Monge-Kantorovich Depth Iterative Function
#'
#' Calculates the Monge-Kantorovich depth
#'  using several iterations in order to obtain  more reliable values.
#' The function \code{\link[MKdepth]{MKdepth}} is computed several times
#'  to return the median of the different results.
#'
#' @param iter Positive integer. Iterations number, i.e , the calls number of the function \code{\link{MKdepth}}.
#' If \code{eps} is specified, \code{iter} is not considered. By default \code{iter=1}.
#' @param eps Positive double. Maximum expected difference between
#'  the depth values of the penultimate iteration and the depth values of the last iteration.
#'  If \code{eps=NULL}, this is \code{iter}, the iterations number, which is considered. By default \code{eps=NULL}.
#' @inheritParams MKdepth
#' @keywords MKdepth
#' @return List containing a numerical vector of depths (\code{$MKdepth}), one for each row in \code{data}, the iterations number (\code{$iteration})
#' and the execution time (\code{$time}).
#' @details Due to the random generate of a ball-distribution, the computed depth values by the
#' \code{\link{MKdepth}} function may not be precisely exact. In order to counter this, the function \code{\link{MKdepth}} is computed
#'  several times and the median of all results is selected. To limit the number of iterations, the parameters
#'  \code{iter} and \code{eps} are used.
#' @importFrom stats median
#' @author Leo Meurice
#' @export
#' @examples
#' data=ball_distribution(100,2)
#' depth=MKdepth_iterative(data,eps=0.01)
#'
#' data=matrix(rnorm(2000),1000)
#' depth=MKdepth_iterative(data,iter=10)






MKdepth_iterative=function(data,trs_method="assignment",depth_method="Tukey",approx=FALSE,iter=1,eps=NULL,prec=3){
  if(iter<1){stop("iter must be a positive integer")}
  depth=NULL
  if(is.null(eps)){
    time.start=proc.time()
    for(i in 1:iter){
      depth=cbind(depth,MKdepth(data,trs_method=trs_method,depth_method = depth_method,approx=approx,prec=prec))
    }
    time.end=proc.time()
    return(list(MKdepth=apply(depth,1,median),iteration=iter,time=(time.end-time.start)[[3]]))
  }
  else{
    err=eps+1
    it=1
    time.start=proc.time()
    depth=cbind(depth,MKdepth(data,trs_method=trs_method,depth_method = depth_method,approx=approx,prec=prec))
    old_median= apply(depth,1,median)
    while(err>eps){
      depth=cbind(depth,MKdepth(data,trs_method=trs_method,depth_method = depth_method,approx=approx,prec=prec))
      new_median=apply(depth,1,median)
      diff=abs(new_median-old_median)
      err=max(diff)
      old_median=new_median
      it=it+1
    }
    time.end=proc.time()
    return(list(MKdepth=old_median,iteration=it-1,time=(time.end-time.start)[[3]]))
  }
}
