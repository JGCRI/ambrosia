% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_parameters.R
\name{calculate_ambrosia_params}
\alias{calculate_ambrosia_params}
\title{Calculates the 11 parameters for ambrosia using data calculated by `create_dataset_for_parameter_fit` by maximizing log likelihood}
\usage{
calculate_ambrosia_params(
  optim_method = "BFGS",
  original_param_vector = c(1.28, 1.14, -0.19, 0.21, -0.33, 0.5, 0.1, 16, 5.06, 100,
    20),
  datadir = "outputs/Processed_Data_for_MC.csv",
  outdir = "tests/testthat/test_outputs/",
  max_iterations = 100,
  print_progress = FALSE,
  trace_param = FALSE
)
}
\arguments{
\item{optim_method}{The optimization method to be used for maximization of the log likelihood. The default is set to BFGS}

\item{original_param_vector}{Original parameter vector to be used as the starting point for the optimization.These parameters are taken from Edmonds et al 2017}

\item{datadir}{Directory to the data calculated by `create_dataset_for_parameter_fit`.}

\item{outdir}{Directory to store output csv. Default is set to test_output folder.}

\item{max_iterations}{A maximum number of iterations that can be passed to optim. This is largely meant for testing purposes.Default is set to 100 for BFGS.}

\item{print_progress}{A parameter that allows the user to track progress of function.}

\item{trace_param}{Setting this to TRUE will generate a .dat file with all parameter samples and log likelihhod value during a parameter fitting exercise (eg. MCMC)}
}
\value{
A vector with the 11 parameters which are as follows,

A_s : A scaling term for staple commodities

A_n : A scaling perm for nonstaples commodities

xi_ss : Price elasticity for staples

xi_cross : Cross price elasticity

xi_nn : Price elasticity for non-staples

nu_1n : Income elasticity for non-staples

lambda_s : Income elasticity for staples

k_s : Income level at which total demand begins saturating

Pm : Scaling parameter for price of materials (everything other than staples and non-staples)

psscl : Price scaling parameter for staples that is applied to the Price of staples (Ps) and the alpha of staples

pnscl : Price scaling parameter for staples that is applied to the Price of non-staples (Pn) and the alpha of non-staples
}
\description{
Calculates the 11 parameters for ambrosia using data calculated by `create_dataset_for_parameter_fit` by maximizing log likelihood
}
\details{
The following steps are involved in the parameter estimation function.

1) First a log-likelihood function is set up with the data.

2) Next, the value returned by the log-likelihood function will be maximized using optim(). The user can provide
 a seed of initial parameters to begin the optimization process (the lowest possible seed would be the lowest
 values of all 11 parameters). The default seed is set to the original parameters from Edmonds et al (2017). The user
 can now specify the optimization method to be used. The default is set to the "BFGS" method, but the user can
 also run the optimization using methods such as "Neldor-Mead".

3) Finally, the function will now return a vector of parameters that can be used to derive estimates of food demand (using the \code{\link{food.dmnd}} function). The function also prints out the maximized value of the log-likelihood function, so that the user can verify the efficiency and effectiveness of the parameter estimation.
}
\author{
KBN 2020
}
