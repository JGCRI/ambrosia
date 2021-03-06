% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{assign.sigma.Q}
\alias{assign.sigma.Q}
\title{Assign observational errors to observed demand quantities}
\usage{
assign.sigma.Q(input.data, min.group = 5)
}
\arguments{
\item{input.data}{Data frame of observational input.}

\item{min.group}{Minimum group size for clustering}
}
\value{
A dataframe updated with the calculated observational error (sig2Qs and sig2Qn columns)
}
\description{
Assign sigma (observational error) values for Qs and Qn in an input data set.
We do this by clustering the input on Ps, Pn, and Y and then taking the
variance of the Qs and Qn in each cluster.
}
\details{
Observational errors are estimated by clustering observed data by the demand
model input values (i.e., prices for staple and nonstaple foods) and
calculating the variance of observations in each cluster.  In order for this
to work, you have to ensure that there are enough observations in each
cluster to produce a variance that is at least somewhat reliable.  The
tradeoff here is that the larger you make the clusters, the better the
variance estimate is, but less alike the observations in the cluster actually
are (meaning some of the variance is not observational error, but actual
difference in demand.  This tradeoff is controlled by the \code{min.group}
argument.
}
