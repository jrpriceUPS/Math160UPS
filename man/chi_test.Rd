% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chi_test.R
\name{chi_test}
\alias{chi_test}
\title{Chi-Squared Test}
\usage{
chi_test()
}
\description{
This function asks you a sequence of questions in order to execute a proportion test. It finds a p-value, produces a plot, and indicates how this could be queried directly from R.
}
\examples{
> chi_test()
Are you comparing two distributions or checking goodness of fit?
  Possible answers are 'comparing' and 'goodness'.
comparing
How many rows are there in your table?
  3
How many columns are there in your table?
  2
What is the entry in row 1 and column 1?
  10
What is the entry in row 2 and column 1?
  9
What is the entry in row 3 and column 1?
  8
What is the entry in row 1 and column 2?
  5
What is the entry in row 2 and column 2?
  6
What is the entry in row 3 and column 2?
  19
The expected data were:
  [,1]      [,2]
[1,]  7.105263  7.894737
[2,]  7.105263  7.894737
[3,] 12.789474 14.210526

but you observed:
  [,1] [,2]
[1,]   10    5
[2,]    9    6
[3,]    8   19

Your chi-squared-statistic is:
  X^2  = 6.60856

The degrees of freedom are:
  df  = 2

The probability of getting this result or more extreme
if there really is no relationship is
p =  0.03672565

You can get this result by:

  Inputting the data in the table in a list that goes column-by-column:
  data  = c(10,9,8,5,6,19)

Then converting that into a matrix:
  A  = matrix(data,nrow = 3)

Then using that to run the test:
  chisq.test(A)






> chi_test()
Are you comparing two distributions or checking goodness of fit?
  Possible answers are 'comparing' and 'goodness'.
goodness
How many categories are there in your distribution?
  6
What is entry number 1 in your sample?
  10
What is entry number 2 in your sample?
  8
What is entry number 3 in your sample?
  14
What is entry number 4 in your sample?
  9
What is entry number 5 in your sample?
  5
What is entry number 6 in your sample?
  16
Is your hypothesis that all categories are equally likely?
  yes
The expected data were:
  [1] 10.33333 10.33333 10.33333 10.33333 10.33333 10.33333

but you observed:
  [1] 10  8 14  9  5 16

Your chi-squared-statistic is:
  X^2  = 7.870968

The degrees of freedom are:
  df  = 5

The probability of getting this result or more extreme if the distribution
is really the theoretical one
p =  0.1634917

You can get this result by:

  Inputting the sample data in a list:
  data  = c(10,8,14,9,5,16)

and also recording the theoretical data:
  prob  = c(0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667)

Then using that to run the test:
  chisq.test(data,p = prob)
}
