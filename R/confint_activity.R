#' Guide the class in constructing confidence intervals
#'
#' Allows students to calculate their own sample proportion and use it to make a confidence interval.
#' @export
confint_activity <- function(n = 20,numstudents = "N",conf = .95,section_name = "section"){

  if (numstudents == "N"){
    section = get(section_name)
    students = get("students")
    coffee = droplevels(subset(students,Coffee == "Yes" | Coffee == "No"))$Coffee



    for (i  in 1:length(section)){

      current = table(sample(coffee,n))
      cat(paste(section[i],": ",toString(current[1])," No, ",toString(current[2])," Yes\n",sep=""))


    }
    myPropTable = prop.table(table(coffee))
    cat(paste("\nThe actual proportion is ",toString(round(myPropTable[2],6)),".\n",sep=""))


  }

  if(numstudents != "N"){

    data(students)
    coffee = droplevels(subset(students,Coffee == "Yes" | Coffee == "No"))$Coffee

    myPropTable = prop.table(table(coffee))
    trueP = myPropTable[2]

    list = rep(0,numstudents)
    for (i  in 1:numstudents){

      current = prop.table(table(sample(coffee,n)))
      list[i] = current[2]

    }

    zstar = -qnorm((1-conf)/2)

    upper_bound = list + zstar*sqrt(list*(1-list)/n)
    lower_bound = list - zstar*sqrt(list*(1-list)/n)

    percent = round(sum(trueP>lower_bound & trueP<upper_bound)/numstudents*100,3)

    cat(paste(toString(percent),"% of students created a confidence interval that included the actual proportion of ",toString(round(myPropTable[2],6)),".\n",sep=""))

  }

}
