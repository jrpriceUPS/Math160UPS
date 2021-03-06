% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normal_p.R
\name{normal_p}
\alias{normal_p}
\title{Find the Proportion of Observations Above or Below a Point for a Normal Distribution}
\usage{
normal_p(x, mu, sigma, type, print = TRUE)
}
\arguments{
\item{x}{the point of interest}

\item{mu}{the mean of the normal distribution}

\item{sigma}{the standard deviation of the normal distributions}

\item{type}{whether we are looking for the proportion above or below the chosen point. Options are 'less', 'greater', or 'both'. 'both' is used by the z-test function to find more extreme points.}

\item{print}{whether or not to print output to the screen.  Defaults to TRUE. Typically we want to print the results, but this is also called by the z-test function, in which case we wish to print something else.}
}
\value{
the probability of finding a value in the desired interval and the pnorm() command to find that result.
}
\description{
This function is one of those chosen through prompts by normal(). If one is looking for the proportion of observations expected to fall above or below a particular value for a distribution that is normal, one can use this function to calculate and visualize that.
}
\examples{

> output = normal_p(-1.5,0,1,'less',TRUE)
The proportion of observations with a value of -1.5 or more extreme is 0.0668072

You can get this result by typing:
 pnorm(-1.5,0,1)


> output = normal_p(-1.5,0,1,'greater',TRUE)
The proportion of observations with a value of -1.5 or more extreme is 0.9331928

You can get this result by typing:
  1-pnorm(-1.5,0,1) 
}
