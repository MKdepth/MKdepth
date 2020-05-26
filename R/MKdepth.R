#' The Monge-Kantorovich Depth
#'
#' Computes the Monge-Kantorovich depth of each observation from a data set.
#'
#' @param data \code{n} by \code{p} matrix of data where each row contains a p-variate point, w.r.t which the Monge-Kantorovich depth
#' is to be calculated.
#' @param trs_method Character sting which determines the transport algorithm used. By default, \code{trs_method="assignment"}. See details below.
#' @param approx Logical. If dimension is greater than or equal to 3, the Tukey depth can be approximate . Useful when sample size is large.
#' @param depth_method Character string which determines the depth function used for the reference distribution. Method can be "Tukey" (the default), "Liu" or "Oja".
#' @param prec Integer indicating the number of decimal places to be used for depth values. By default, \code{prec=3}.
#' @keywords MKdepth
#' @return Numerical vector of depths, one for each row in \code{data}.
#' @details
#' The Monge-Kantorovich depth is constructed as explained in \insertCite{chernozhukov2017monge}{MKdepth}.
#'
#' In order to calculate the Monge-Kantorovich depth, we need to calculate the Tukey depth of a specific distribution.
#' For this, we use the function \code{\link[depth]{depth}} of the package \link{depth}. For more details on the depth method, please see \link[depth]{depth} details.
#'
#' The parameter \code{trs_method} is the transport method which is used for the Monge-Kantoroch depth.
#' The default method are: \code{"assignement"}, comes from the \code{adagio} package (See References). The other methods which can be used,
#' come from the \code{transport} package and are the same as for the \code{\link[transport]{transport}} function (See References).
#' @importFrom depth depth
#' @importFrom adagio assignment
#' @importFrom transport transport pp
#' @importFrom Rdpack reprompt
#' @author Leo Meurice
#' @references
#' \insertRef{depth}{MKdepth}
#'
#' \insertRef{borchers2012package}{MKdepth}
#'
#' \insertRef{schuhmacher2019package}{MKdepth}
#'
#' \insertRef{chernozhukov2017monge}{MKdepth}
#'
#' @export
#' @examples
#' set.seed(1)
#' data=ball_distribution(1000,2)
#' depth=MKdepth(data)
#'
#' set.seed(1)
#' data=matrix(rnorm(2000),1000)
#' depth=MKdepth(data,trs_method="auction")



MKdepth=function(data,trs_method="assignment",depth_method="Tukey",approx=FALSE,prec=3){
  if(!is.logical(approx)){stop("approx must be logical")}
  if(prec<0){stop("prec must be a positive integer")}
  if(!trs_method%in%c('auction','auctionbf','networkflow','assignment','primaldual','revsimplex','shortsimplex')){
    stop("Transport method is not supported")
  }
  if(!depth_method%in%c('Tukey','Liu','Oja')){stop('depth_method must be equal to "Tukey", "Liu" or "Oja"')}
  data=as.matrix(data)
  n=dim(data)[1]
  dim=dim(data)[2]
  monge=rep(0,len=n)
  if(dim==1){
    data=cbind(data,rep(0))
    for(i in 1:n){
      monge[i]=round(depth(data[i,],data,method="Tukey",approx=FALSE),prec)
    }
    return(monge)
  }
  if(trs_method=="assignment"){
    cmat=matrix(rep(0,len=n^2),nrow=n)
    reference=ball_distribution(n,dim)
    half=rep(0,len=n)
    for(i in 1:n){
      half[i]=depth(reference[i,],reference,method=depth_method,approx=approx)
    }
    for(i in 1:n){
      for(j in 1:n){
        cmat[i,j]=sum((reference[i,]-data[j,])^2)
      }
    }
    cmat=round(round(cmat,2)*100,2)
    trs=assignment(cmat)
    monge[trs$perm]=round(half,prec)
    return(monge)
  }
  else{
    ypp=pp(ball_distribution(n,dim))
    half=rep(0,len=n)
    for(i in 1:n){
      half[i]=depth(ypp$coordinates[i,],ypp$coordinates,method=depth_method,approx=approx)
    }
    trs=transport(ypp,pp(data),method=trs_method)
    monge[trs$to]=round(half,prec)
    return(monge)
  }
}
