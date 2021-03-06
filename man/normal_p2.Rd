% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normal_p2.R
\name{normal_p2}
\alias{normal_p2}
\title{Find the Proportion of Observations Between Two Points in a Normal Distribution}
\usage{
normal_p2(x, y, mu, sigma)
}
\arguments{
\item{x}{the left endpoint of the interval}

\item{y}{the right endpoint of the interval}

\item{mu}{the mean of the normal distribution}

\item{sigma}{the standard deviation of the normal distributions}
}
\description{
This function is one of those chosen through prompts by normal(). If one is looking for the proportion of observations expected to fall between two values for a distribution that is normal, one can use this function to calculate and visualize that.
}
\examples{

> normal_p2(-1.2,-0.8,0,1)
The proportion of observations between -1.2 and -0.8 is 0.09678573

You can get this result by typing:
  pnorm(-0.8,0,1)-pnorm(-1.2,0,1)
}
