t_dist_demo = function(n = 4,samples = 10000){
  students = get("students")
  study = students$Study[!is.na(students$Study)]
  
  trueMean = mean(study)
  trueSD = sd(study)/sqrt(n)
  
  means = rep(0,samples)
  for(i in 1:samples){
    means[i] = mean(sample(study,n,replace = TRUE))
  }
  
  hist(means,freq = FALSE, breaks = 200)
  
  xgrid = seq(from = min(means),to = max(means),length.out = 1000)
  lines(xgrid,dnorm(xgrid,trueMean,trueSD))
  
  tgrid = dt((xgrid - trueMean)/trueSD,df = n - 1)

  lines(xgrid,tgrid/trueSD, col = "red")
}