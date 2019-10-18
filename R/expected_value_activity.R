#' Flip a coin three times for every student
#'
#' Gives everyone in class a number of heads flipped out of three (in case they don't have coins for the expected value activity).


expected_value_activity <- function(){
  
  data(section)
  num_heads = rbinom(length(section),3,0.5)
  
  for (i  in 1:length(section)){
    
    if (num_heads[i]==1){
    cat(paste(section[i],": You flipped ",toString(num_heads[i])," head.\n",sep=""))
    } else{
      cat(paste(section[i],": You flipped ",toString(num_heads[i])," heads.\n",sep=""))
    }
    
  }
  
}
