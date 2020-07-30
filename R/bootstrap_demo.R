bootstrap_demo = function(n = 4,samples = 10000, mu0 = 32){
  students = get("students")
  study = students$Study[!is.na(students$Study)]
  mySample = sample(study,4)
  mySample = mySample - mean(mySample) + mu0
  
  trueMean = mean(study)
  trueSD = sd(study)/sqrt(n)
  
  tlist = rep(0,samples)
  for(i in 1:samples){
    subsample = sample(mySample,n,replace = TRUE)
    myMean = mean(subsample)
    mySD = sd(subsample)
    tlist[i] = (myMean - mu0)/mySD
  }
  
  hist(tlist,freq = FALSE, breaks = 20,xlim = c(-8,8))
  
  xgrid = seq(from = -8,to = 8,length.out = 1000)
  lines(xgrid,dnorm(xgrid,0,trueSD))
  
  lines(xgrid,dt(xgrid,df = n - 1), col = "red")
}