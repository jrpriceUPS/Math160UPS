#' Guide the class in conducting hypothesis tests
#'
#' Allows students to calculate their own sample mean and use it to do a significance test.
#' @export
hyp_test_activity <- function(n = 20,numstudents = "N",conf = .95,section_name = "section"){


  if (numstudents == "N"){
    section = get(section_name)
    students = get("students")
    coffee = droplevels(subset(students,Coffee == "Yes" | Coffee == "No"))$Coffee



    for (i  in 1:length(section)){

      current = table(sample(coffee,n))
      cat(paste(section[i],": ",toString(current[1])," No, ",toString(current[2])," Yes\n",sep=""))


    }


  }

  if(numstudents != "N"){

    data(students)
    coffee = droplevels(subset(students,Coffee == "Yes" | Coffee == "No"))$Coffee
    coffee = factor(coffee,c("Yes","No"))

    myPropTable = prop.table(table(coffee))
    trueP = myPropTable[2]

    list = rep(0,numstudents)
    for (i  in 1:numstudents){

      current = binom.test(table(sample(coffee,n)))
      list[i] = current$p.value

    }

    percent = sum(list<0.05)/numstudents

    cat(paste(toString(round(percent*100,2)),"% of students found their result had a probability of below 0.05 under the null hypothesis.\n",sep=""))

  }

}
