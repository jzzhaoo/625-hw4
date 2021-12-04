#' Multiple Linear Regression with 2 predictor variables
#'
#' @param y The response variable
#' @param x1 The first predictor variable
#' @param x2 The second predictor variable
#'
#' @return b0, b1, b2
#' @export
#'
#' @import graphics
#' @examples
#' mltplm(y<-(50:149),x1<-(1:100),x2<-(100:199))
mltplm <- function(y,x1,x2){
  if (length(x1) == length(x2) && length(x2)== length(y)){
    n<-length(x1)
    x12sum <- sum(x1*x1)
    x22sum <- sum(x2*x2)
    x1ysum <- sum(x1*y)
    x2ysum <- sum(x2*y)
    x1x2sum <- sum(x1*x2)
    x1sum <- sum(x1)
    x2sum <- sum(x2)
    ysum <- sum(y)
    regx12 <- x12sum-x1sum*x1sum
    regx22 <- x22sum-x2sum*x2sum
    regx1y <- x1ysum-x1sum*ysum
    regx2y <- x2ysum-x2sum*ysum
    regx1x2 <- x1x2sum-x1sum*x2sum
    ybar <- mean(y)
    x1bar <- mean(x1)
    x2bar <- mean(x2)
    b1 <- (regx22*regx1y-regx1x2*regx2y)/(x12sum*x22sum-x1x2sum*x1x2sum)
    b2 <- (regx12*regx2y-regx1x2*regx1y)/(x12sum*x22sum-x1x2sum*x1x2sum)
    b0 <- ybar-b1*x1bar-b2*x2bar
    yhat <- b0+b1*x1+b2*x2
    res <- y-yhat
    print(paste0("yhat = ",round(b0,4)," + ", round(b1,4), "x1", " + ",round(b2,4), "x2"))
    data.frame(b0,b1,b2)
  }else{
    print("The length of x1, x2, and y are not equal, please check your dataset!")
  }
}
