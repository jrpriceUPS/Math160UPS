#' Creates tables for the hospital contingency table problem
#' @export
#' 

hospitalProblem = function() {

matrix1 = matrix(c(63,2037,16,784),nrow = 2)
colnames(matrix1) = c("Hospital A","Hospital B")
rownames(matrix1) = c("Died","Survived")
partA = as.table(matrix1)

matrix2 = matrix(c(6,594,8,592),nrow = 2)
colnames(matrix2) = c("Hospital A","Hospital B")
rownames(matrix2) = c("Died","Survived")
partBGood = as.table(matrix2)

matrix3 = matrix(c(57,1443,8,192),nrow = 2)
colnames(matrix3) = c("Hospital A","Hospital B")
rownames(matrix3) = c("Died","Survived")
partBBad = as.table(matrix3)

return(partA,partBGood,partBBad)

}