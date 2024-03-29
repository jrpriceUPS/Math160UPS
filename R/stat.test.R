#' Statistical Test
#'
#' This function asks you a sequence of questions in order to discern which statistical test to employ. It then directs you to the proper test and gathers information in order to deliver a result! This can execute z-tests, t-tests, two-sample t-tests, matched-pairs t-tests, one sample proportion tests, two-sample proportion tests, chi-squared tests, and chi-squared godness-of-fit tests.
#' @export
#' @examples
#'
#'
#' ****************
#' Proportion Tests
#' ****************
#'
#' > stat.test()
#' Are you considering a population mean (or means), a population proportion (or proportions), or a table of values?
#'   Possible answers are 'xbar', 'phat', or 'table'.
#' phat
#' Do you have a single population or are you comparing populations?
#'   Possible answers are 'single' and 'comparing'.
#' single
#' How many trials were there in your experiment?
#'   10
#' How many successes were there?
#'   5
#' The statistics for your dataset are:
#'   phat = 0.5
#' s = sqrt(0.5*(1-0.5)/10) = 0.1581139
#'
#' What is the theoretical proportion you are testing against (called p_0)?
#'   (If you only want a confidence interval, type 'NA')
#' .2
#' What is your desired confidence level?
#'   .9
#' The probability of getting this result or more extreme for phat
#' if the proportion really is 0.2 is
#' p =  0.0327935
#'
#' The 90% confidence interval for the population proportion is
#' 0.187086  < p <  0.812914
#'
#'
#' You can get this result by typing:
#'   binom.test(x = 5, n = 10, p = 0.2, alternative = 'two.sided', conf.level = 0.9)
#'
#'
#'
#'
#'
#'
#' > stat.test()
#' Are you considering a population mean (or means), a population proportion (or proportions), or a table of values?
#'   Possible answers are 'xbar', 'phat', or 'table'.
#' phat
#' Do you have a single population or are you comparing populations?
#'   Possible answers are 'single' and 'comparing'.
#' comparing
#' How many trials were there in your first sample?
#'   15
#' How many successes were there in your first sample?
#'   10
#' How many trials were there in your second sample?
#'   20
#' How many successes were there in your second sample?
#'   10
#' The statistics for your dataset are:
#'   phat1 = 0.6666667
#' phat2 = 0.5
#' s = sqrt(0.6666667*(1-0.6666667)/15+0.5*(1-0.5)/20) = 0.1652719
#' What is your desired confidence level?
#'   .95
#' The probability of getting this result or more extreme for phat2 - phat1
#' if there really is no difference is
#' p =  0.521582
#'
#' The 95% confidence interval for the difference in proportions is
#' -0.5489271  < p2 - p1 <  0.2155937
#'
#'
#' You can get this result by typing:
#'   prop.test(c(10,10), c(20,15), alternative = 'two.sided', conf.level = 0.95)
#'
#' *******
#' t-Tests
#' *******
#'
#' > stat.test()
#' Are you considering a population mean (or means), a population proportion (or proportions), or a table of values?
#'   Possible answers are 'xbar', 'phat', or 'table'.
#' xbar
#' Do you have a single population or are you comparing populations?
#'   Possible answers are 'single' and 'comparing'.
#' single
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)?
#'   Possible answers are 'whole' or 'stats'.
#' whole
#' What is the name of your variable?
#'   x
#' The statistics for your dataset are:
#'   xbar =  4
#' s =  2.160247
#' n =  7
#' df =  7 - 1 =  6
#'
#' What is the theoretical mean you are testing against (called mu_0)?
#'   (If you only want a confidence interval, type 'NA')
#' 3
#' What is your desired confidence level?
#'   .95
#' Your t-statistic is:
#'   t  = (4-3)/(2.160247/sqrt(7)) = 1.224745
#'
#' The probability of getting this result or more extreme for xbar
#' if mu really is 3 is
#' p =  0.2665697
#'
#' You can get this result by typing:
#'   2*(1-pt(1.22474487139159,6))
#'
#'
#' The 95% confidence interval for the population mean is
#' 2.002105  < mu <  5.997895
#'
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,6) = 2.446912
#'
#' and then calculating:
#'   4 - 2.446912 x 2.160247/sqrt(7)  and  4 + 2.446912 x 2.160247/sqrt(7)
#'
#'
#' Or, since you have the whole dataset, you could just type:
#'   t.test(x,mu = 3,conf.level = 0.95)
#'
#'
#'
#'
#'
#'
#' > stat.test()
#' Are you considering a population mean (or means), a population proportion (or proportions), or a table of values?
#'   Possible answers are 'xbar', 'phat', or 'table'.
#' xbar
#' Do you have a single population or are you comparing populations?
#'   Possible answers are 'single' and 'comparing'.
#' single
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)?
#'   Possible answers are 'whole' or 'stats'.
#' stats
#' What is your sample mean?
#'   4
#' What is your sample standard deviation?
#'   2.16
#' What is your sample size?
#'   7
#' What is the theoretical mean you are testing against (called mu_0)?
#'   (If you only want a confidence interval, type 'NA')
#' 3
#' What is your desired confidence level?
#'   .95
#' Your t-statistic is:
#'   t  = (4-3)/(2.16/sqrt(7)) = 1.224885
#'
#' The probability of getting this result or more extreme for xbar
#' if mu really is 3 is
#' p =  0.2665206
#'
#' You can get this result by typing:
#'   2*(1-pt(1.22488486623361,6))
#'
#'
#' The 95% confidence interval for the population mean is
#' 2.002333  < mu <  5.997667
#'
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,6) = 2.446912
#'
#' and then calculating:
#'   4 - 2.446912 x 2.16/sqrt(7)  and  4 + 2.446912 x 2.16/sqrt(7)
#'
#'
#'
#'
#'
#'
#' > x = c(1, 2, 3, 4, 5, 6, 7)
#' > y = c(2.5,  5.1,  6.4,  8.4, 10.8, 13.4, 15.3)
#' > stat.test()
#' Are you considering a population mean (or means), a population proportion (or proportions), or a table of values?
#'   Possible answers are 'xbar', 'phat', or 'table'.
#' xbar
#' Do you have a single population or are you comparing populations?
#'   Possible answers are 'single' and 'comparing'.
#' comparing
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)?
#'   Possible answers are 'whole' or 'stats'.
#' whole
#' Is this a matched-pairs comparison in which the same subjects are measured twice?
#'   yes
#' What is the name of the variable for the first set of measurements?
#'   x
#' What is the name of the variable for the second set of measurements?
#'   y
#' The statistics for your datasets are:
#'   n =  7
#' xbar1 =  4
#' s1 =  2.160247
#'
#' xbar2 =  8.842857
#' s2 =  4.595236
#'
#' The statistics for the difference are:
#'   xbar =  4.842857
#' s =  2.445988
#' n =  7
#' df =  7 - 1 =  6
#'
#' What is your desired confidence level?
#'   .95
#' Your t-statistic is:
#'   t  = 4.842857/(2.445988/sqrt(7)) = 5.238372
#'
#' The probability of getting this result or more extreme for xbar2 - xbar1 if there really is no difference is
#' p =  0.001941435
#'
#' You can get this result by typing:
#'   2*(1-pt(5.23837230565063,6))
#'
#'
#' The 95% confidence interval for the difference in population means is
#' 2.580696  < mu2 - mu1 <  7.105019
#'
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,6) = 2.446912
#'
#' and then calculating:
#'   (8.84285714285714-4) - 2.446912 x 2.445988/sqrt(7)  and  (8.84285714285714-4) + 2.446912 x 2.445988/sqrt(7)
#'
#'
#' Or, since you have the whole dataset, you could just type:
#'   t.test(y,x, paired = TRUE, conf.level = 0.95)
#'
#'
#'
#'
#'
#'
#' > stat.test()
#' Are you considering a population mean (or means), a population proportion (or proportions), or a table of values?
#'   Possible answers are 'xbar', 'phat', or 'table'.
#' xbar
#' Do you have a single population or are you comparing populations?
#'   Possible answers are 'single' and 'comparing'.
#' comparing
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)?
#'   Possible answers are 'whole' or 'stats'.
#' whole
#' Is this a matched-pairs comparison in which the same subjects are measured twice?
#'   no
#' What is the name of the variable for the first set of measurements?
#'   x
#' What is the name of the variable for the second set of measurements?
#'   y
#' The statistics for your datasets are:
#'   n1 =  7
#' xbar1 =  4
#' s1 =  2.160247
#'
#' n2 =  7
#' xbar2 =  8.842857
#' s2 =  4.595236
#'
#' The statistics for the difference are:
#'   xbar =  4.842857
#' s = sqrt(2.160247^2/7 + 4.595236^2/7) = 1.919183
#' df = 8.5285
#'
#' What is your desired confidence level?
#'   .95
#' Your t-statistic is:
#'   t  = (4.84285714285714)/(1.919183) = 2.523395
#'
#' The probability of getting this result or more extreme for xbar2 - xbar1 if there really is no difference is
#' p =  0.03391985
#'
#' You can get this result by typing:
#'   2*(1-pt(2.52339452856832,8.52849965837585))
#'
#'
#' The 95% confidence interval for the difference in population means is
#' 0.4644978  < mu2 - mu1 <  9.221216
#'
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,8.5285) = 2.281366
#'
#' and then calculating:
#'   (8.84285714285714-4) - 2.281366 x 1.919183  and  (8.84285714285714-4) + 2.281366 x 1.919183
#'
#'
#' Or, since you have the whole dataset, you could just type:
#'   t.test(y,x, conf.level = 0.95)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' *****************
#' Chi-Squared Tests
#' *****************
#'
#' > stat.test()
#' Are you considering a population mean (or means), a population proportion (or proportions), or a table of values?
#'   Possible answers are 'xbar', 'phat', or 'table'.
#' table
#' Are you comparing two distributions or checking goodness of fit?
#'   Possible answers are 'comparing' and 'goodness'.
#' comparing
#' How many rows are there in your table?
#'   3
#' How many columns are there in your table?
#'   2
#' What is the entry in row 1 and column 1?
#'   10
#' What is the entry in row 2 and column 1?
#'   9
#' What is the entry in row 3 and column 1?
#'   8
#' What is the entry in row 1 and column 2?
#'   5
#' What is the entry in row 2 and column 2?
#'   6
#' What is the entry in row 3 and column 2?
#'   19
#' The expected data were:
#'   [,1]      [,2]
#' [1,]  7.105263  7.894737
#' [2,]  7.105263  7.894737
#' [3,] 12.789474 14.210526
#'
#' but you observed:
#'   [,1] [,2]
#' [1,]   10    5
#' [2,]    9    6
#' [3,]    8   19
#'
#' Your chi-squared-statistic is:
#'   X^2  = 6.60856
#'
#' The degrees of freedom are:
#'   df  = 2
#'
#' The probability of getting this result or more extreme
#' if there really is no relationship is
#' p =  0.03672565
#'
#' You can get this result by:
#'
#'   Inputting the data in the table in a list that goes column-by-column:
#'   data  = c(10,9,8,5,6,19)
#'
#' Then converting that into a matrix:
#'   A  = matrix(data,nrow = 3)
#'
#' Then using that to run the test:
#'   chisq.test(A)
#'
#'
#'
#'
#'
#'
#' > stat.test()
#' Are you considering a population mean (or means), a population proportion (or proportions), or a table of values?
#'   Possible answers are 'xbar', 'phat', or 'table'.
#' table
#' Are you comparing two distributions or checking goodness of fit?
#'   Possible answers are 'comparing' and 'goodness'.
#' goodness
#' How many categories are there in your distribution?
#'   6
#' What is entry number 1 in your sample?
#'   10
#' What is entry number 2 in your sample?
#'   8
#' What is entry number 3 in your sample?
#'   14
#' What is entry number 4 in your sample?
#'   9
#' What is entry number 5 in your sample?
#'   5
#' What is entry number 6 in your sample?
#'   16
#' Is your hypothesis that all categories are equally likely?
#'   yes
#' The expected data were:
#'   [1] 10.33333 10.33333 10.33333 10.33333 10.33333 10.33333
#'
#' but you observed:
#'   [1] 10  8 14  9  5 16
#'
#' Your chi-squared-statistic is:
#'   X^2  = 7.870968
#'
#' The degrees of freedom are:
#'   df  = 5
#'
#' The probability of getting this result or more extreme if the distribution
#' is really the theoretical one
#' p =  0.1634917
#'
#' You can get this result by:
#'
#'   Inputting the sample data in a list:
#'   data  = c(10,8,14,9,5,16)
#'
#' and also recording the theoretical data:
#'   prob  = c(0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667)
#'
#' Then using that to run the test:
#'   chisq.test(data,p = prob)

stat.test <- function(){

  cat("Are you considering a t-test (one or two sample), a proportion test (one or two sample), or a chi-squared test?\n")
  cat("Possible answers are 't', 'p', or 'chi'. ")
  x_or_p = readline()

  if(x_or_p=="t" | x_or_p == "T" | x_or_p == "t-test" | x_or_p == "T-test" | x_or_p == "t test" | x_or_p == "T test"){
      t_test()
  }

  if(x_or_p=="phat" | x_or_p == "p" | x_or_p == "proportion" | x_or_p == "prop" | x_or_p == "P" | x_or_p == "Proportion" | x_or_p == "Prop"){
    prop_test()
  }

  if(x_or_p=="chi" | x_or_p == "chi-squared" | x_or_p == "chi2"){
    chi_test()
  }
}
