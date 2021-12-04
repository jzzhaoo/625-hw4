#' Descriptive Statistics
#'
#' @param x The variable
#'
#' @return Descriptive statistics of x
#' @export
#'
#' @import stats
#' @examples
#' dscpstat(c(5:500))
dscpstat <- function(x){
  par(mfrow=c(1,2))
  hist(x, col = "slategray2")
  boxplot(x, col = "pink3")
  stem(x)
  min=min(x)
  first=quantile(x,0.25)
  mean=mean(x)
  median=median(x)
  third=quantile(x,0.75)
  max=max(x)
  data.frame(min,first,mean,median,third,max)
}
