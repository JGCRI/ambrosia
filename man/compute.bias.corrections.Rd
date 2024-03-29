% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/food-demand.R
\name{compute.bias.corrections}
\alias{compute.bias.corrections}
\title{Compute regional bias corrections.}
\usage{
compute.bias.corrections(params, obs.trn)
}
\arguments{
\item{params}{Model parameter structure (described in
\code{\link{food.dmnd}})}

\item{obs.trn}{Data frame of training observations.}
}
\value{
calculated bias corrected values
}
\description{
Compute regional bias correction for a set of parameters and a training set of observations.
Bias correction factors are \emph{multiplied} by model data to get bias
corrected model output.
}
