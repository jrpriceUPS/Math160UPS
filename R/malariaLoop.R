#' Run a randomization simulation of the malaria activity in the text
#'
#' This function runs a randomization simulation to show the 
#' likelihood of different scenarios under the null hypothesis
#' for the malaria example from the text. It produces a bar plot
#' and an assessment of the frequency of the observed result.
#' @export
#' @examples
#' > malariaLoop(reps = 10000)
malariaLoop <- function(reps = 10000){
data(malaria)

obs = 5/14 - 6/6
pDiff = rep(0,reps)

for(i in seq(1,reps)){
x = prop.table(table(malaria$Treatment,sample(malaria$Infected)),1)
pDiff[i] = x[2] - x[1]
}

result = table(pDiff)/reps
rownames(result) = round(sort(unique(pDiff)),2)

barplot(result/reps,col = c("skyblue",rep("gray",6)))
print(paste("In",toString(reps),"replications, we got our observed result with frequency", toString(mean(pDiff == obs))))
}