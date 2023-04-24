#' T Test Demo
#'
#' This function allows me to run a demo to illustrate the need for a non-normal t-distribution for small samples.
#' @export

t_dist_demo = function(n = 6,samples = 10000){
  students = get("students")
  sleep = students$Study[!is.na(students$Study)]

  trueMean = mean(sleep)
  trueSD = sd(sleep)/sqrt(n)

  ts = rep(0,samples)
  for(i in 1:samples){
    mySample = sample(sleep,n, replace = TRUE)
    ts[i] = (mean(mySample) - trueMean)/(sd(mySample)/sqrt(n))
  }
  xgrid = seq(from = -5,to = 5,length.out = 1000)
  hist(ts,freq = FALSE, breaks = 20, main = "Distribution of t-statistics",ylim = c(0,max(c(dnorm(xgrid,0,1),dt(xgrid, df = n - 1)))))


  lines(xgrid,dnorm(xgrid,0,1))

  lines(xgrid,dt(xgrid, df = n - 1), col = "red")
}
