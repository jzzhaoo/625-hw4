#' Simple linear regression with 1 predictor
#'
#' @param x The predictor variable
#' @param y The response variable
#'
#' @return Scattterplot with fitted line, residual plot, b0, b1, r
#' @export
#'
#' @import graphics
#' @examples
#' simplm(x<-(1:100),y<-(50:149))
simplm <- function(x,y){
  if (length(x) == length(y)){
    n<-length(x)
    xsum <- sum(x)
    ysum <- sum(y)
    xysum <- sum(x*y)
    x2sum <- sum(x*x)
    y2sum <- sum(y*y)
    b1 <- (n*xysum-xsum*ysum)/(n*x2sum-xsum*xsum)
    b0 <- (ysum-b1*xsum)/n
    yhat <- b1*x+b0
    res <- y-yhat
    r <- (n*xysum-xsum*ysum)/(sqrt(n*x2sum-xsum*xsum)*sqrt(n*y2sum-ysum*ysum))
    par(mfrow = c(1,2))
    plot(x,y,main = "Scatterplot")
    lines(x,yhat, col = "darkred" )
    print(paste0("y = ", round(b1,4), "x", " + ",round(b0,4)))
    plot(x,res,main = "Residual Plot")
    abline(h=0,col = "darkred")
    data.frame(b0,b1,r)
  }else{
    print("The length of x is not equal to y, please check your dataset!")
  }
}

