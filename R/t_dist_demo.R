#' @export

t_dist_demo = function(n = 4,samples = 10000){
  students = get("students")
  sleep = students$Sleep[!is.na(students$Sleep)]

  trueMean = mean(sleep)
  trueSD = sd(sleep)/sqrt(n)

  ts = rep(0,samples)
  for(i in 1:samples){
    mySample = sample(sleep,n, replace = TRUE)
    ts[i] = (mean(mySample) - trueMean)/(sd(mySample)/sqrt(n))
  }

  hist(ts,freq = FALSE, breaks = 20, main = "Distribution of t-statistics")

  xgrid = seq(from = -5,to = 5,length.out = 1000)


  lines(xgrid,dnorm(xgrid,0,1))

  lines(xgrid,dt(xgrid, df = n - 1), col = "red")
}
