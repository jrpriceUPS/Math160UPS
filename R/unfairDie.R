#' @export

unfairDie <- function(rolls = 1){
  p = c(1,3,2,5,2,4,1)
  p = p/sum(p)
  p = cumsum(p)

  r = runif(rolls)
  result = rep(0,rolls)

  for(i in seq(1,rolls)){
  result[i] = which(r[i]<p)[1]
  }

  result
}
