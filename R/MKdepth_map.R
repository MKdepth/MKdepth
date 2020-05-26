#' Level plot for Monge-Kantorovich Depth
#'
#' Produces a contour plot with the areas between the contours
#' filled in solid color. A key showing how the colors map to depth values
#' is shown to the right of the plot. Construct from \code{\link[graphics]{filled.contour}} fonction.
#'
#' @param data \code{n} by \code{2} matrix of data where each row contains a two dimensional point, w.r.t which the Monge-Kantorovich depth
#' is calculated.
#' @param depth Numerical vector of depths, one for each row in \code{data}.
#' @param main an overall title for the plot (optional).
#' @param xlab a title for the x axis (optional).
#' @param ylab a title for the y axis (optional).
#' @inheritParams MKdepth_contours
#'
#' @param col color, used to draw level plot.
#' @param by Interpolation step.
#' @keywords MKdepth
#' @importFrom alphahull ahull
#' @importFrom akima interp
#' @importFrom sp Polygon Polygons SpatialPolygons point.in.polygon
#' @importFrom grDevices heat.colors
#' @importFrom graphics filled.contour
#' @importFrom Rdpack reprompt
#' @author Leo Meurice
#' @references
#' \insertRef{alphahull}{MKdepth}
#' @export
#' @examples
#' set.seed(1)
#' data=ball_distribution(1000,2)
#' depth=MKdepth(data)
#' MKdepth_map(data,depth,alpha=.5,by=.1)
#'
#' set.seed(1)
#' data=matrix(rnorm(2000),1000)
#' depth=MKdepth(data)
#' MKdepth_map(data,depth,alpha=2,by=.1)
#'

MKdepth_map=function(data,depth,alpha,by,col=heat.colors,main="",xlab="",ylab=""){
  depth=depth[!duplicated(data)]
  data=unique(data)
  a=interp(x=data[,1], y=data[,2], z=depth,
           xo=seq(min(data[,1]),max(data[,1]),by=by),
           yo=seq(min(data[,2]),max(data[,2]),by=by), duplicate="mean")
  ahull= ahull(data, alpha = alpha)
  p = Polygon(ahull$xahull[c(ahull$arcs[,7]),])
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  for(i in 1:length(a$x)){
    for(j in 1:length(a$y)){
      check=point.in.polygon(a$x[i],a$y[j],p@coords[,1],p@coords[,2])
      if(check==0){
        a$z[i,j]=NA
      }
    }
  }
  filled.contour(a, color.palette=function(x)rev(col(x)),ylab=ylab,xlab=xlab,main=main)
}
