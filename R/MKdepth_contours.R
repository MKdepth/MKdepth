#' Monge-Kantorovich Depth Contours
#'
#' Builds the Monge-Kantorovich depth contours for 2-dimensional data.
#'
#' @param data \code{n} by \code{2} matrix of data where each row contains a two dimensional point, w.r.t which the Monge-Kantorovich depth
#' is calculated. \code{n} must be larger than 20.
#' @param depth Numerical vector of depths, one for each row in \code{data}.
#' @param contours Integer. Contours number to be drawn. By default, \code{contours=3}.
#' @param alpha Value of \eqn{\alpha}. This parameter is defined in the function \code{\link[alphahull]{ahull}}
#' from the \code{\link{alphahull-package}}. (See Details below.)
#' @param col Color, used to draw contours. The length must be equal to \code{contours}.
#' @param legend Logical; if \code{TRUE}, the legend is displayed. By default, \code{legend=F}.
#' @param plot.new Logical; if \code{TRUE}, a new plot is displayed and the data points and contours are drawn.
#' @keywords MKdepth
#' @details
#' The depth contours are drawn using the \code{\link[alphahull]{ahull}} function from the \code{\link{alphahull-package}}.
#' The \code{alpha} parameter must be adjusted to conform to the shape of the non-convex depth contours.
#' @importFrom grDevices topo.colors
#' @importFrom graphics plot
#' @importFrom Rdpack reprompt
#' @author Leo Meurice
#' @references
#' \insertRef{alphahull}{MKdepth}
#' @export
#' @examples
#' set.seed(1)
#' data=ball_distribution(100,2)
#' depth=MKdepth_iterative(data,iter=100)
#' MKdepth_contours(data,depth$MKdepth,alpha=1)
#'
#' set.seed(1)
#' data=matrix(rnorm(2000),1000)
#' depth=MKdepth(data)
#' plot(data,pch='.')
#' MKdepth_contours(data,depth,contours=5,alpha=2,plot.new=FALSE)
#'



MKdepth_contours=function(data,depth,alpha,contours=3,col=topo.colors(contours),legend=F,plot.new=T){
  data=as.matrix(data)
  if(dim(data)[2]!=2){stop("The table dimension must be equal to 2 to draw depth contours")}
  if(dim(data)[1]<20){stop("The observations number must be larger than 20 to draw depth contours")}
  if(contours<1){stop("contours must be a positive integer")}
  depth=depth[!duplicated(data)]
  data=unique(data)
  min=min(depth)
  max=max(depth)
  c=seq(from=min,to=max-(((max-min)/(contours))),by=((max-min)/(contours)))
  l=rep(0,contours)
  if(plot.new==T){
    plot(data)
  }
  for(i in 1:contours){
    data=data[depth>=c[i],1:2]
    depth=depth[depth>=c[i]]
    ahull<- ahull(data, alpha = alpha)
    plot(ahull,pch='',lwd=1,add=TRUE,col=col[i])
    l[i]=paste(">=",round(c[i],3))
  }
  if(legend==T){
    legend("topright", legend=l,lty=1,col=col,box.lty=0,cex=0.6,inset=0.02,bg="transparent")
  }
}
