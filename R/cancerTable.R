#' Creates breast cancer table
#'
#' Creates table for students to use for breast cancer activity
#' @export
#' 

cancerTable = function() {
x=matrix(c(5046,754,109362,884838),nrow = 2)
breastCancerTable = as.table(x)
rownames(breastCancerTable) = c("Positive","Negative")
colnames(breastCancerTable) = c("Sick","Healthy")

return(breastCancerTable)
}