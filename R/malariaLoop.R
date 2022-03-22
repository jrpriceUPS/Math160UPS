#' Update Code to Latest Version
#'
#' This function updates your library to the newest version and sets those to become available.
#' @export
#' > marlariaLoop()

data(malaria)

obs = 5/14 - 6/6
reps = 10000
pDiff = rep(0,reps)

for(i in seq(1,reps)){
x = prop.table(table(malaria$Treatment,sample(malaria$Infected)),1)
pDiff[i] = x[2] - x[1]
}

result = table(pDiff)/reps
rownames(result) = round(sort(unique(pDiff)),2)

barplot(result/reps)
print(paste("In",toString(reps),"replications, we got our observed result with frequency", toString(mean(pDiff == obs))))