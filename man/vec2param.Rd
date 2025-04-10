% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/food-demand-mc.R
\name{vec2param}
\alias{vec2param}
\title{Convert a vector of parameters into a params structure.}
\usage{
vec2param(
  x = NULL,
  A_s = 1.28,
  A_n = 1.14,
  xi_ss = -0.19,
  xi_cross = 0.21,
  xi_nn = -0.33,
  nu1_n = 0.5,
  lambda_s = 0.1,
  k_s = 16,
  Pm = 5.06,
  psscl = 100,
  pnscl = 20
)
}
\arguments{
\item{x}{Vector of 11 model parameters using the statistical formulation. If this is NULL, a vector is generated based on default parameter values for 11 parameters.}

\item{A_s}{A scale term used to derive expenditure share for staple demand.}

\item{A_n}{A scale term used to derive expenditure share for non-staple demand.}

\item{xi_ss}{Price elasticity of staple goods. Unit change in per capita demand for staples as a result of unit increase in price (in $ per person per day).}

\item{xi_cross}{Cross price elasticity between staples and non-staples which in combination with the other price elasticities is used to derive substitution elasticity.}

\item{xi_nn}{Price elasticity of non-staple goods. Unit change in per capita demand for non-staples as a result of unit increase in price (in $ per person per day).}

\item{nu1_n}{Income elasticity for non-staple goods. Unit change in per capita demand for non-staples for unit change in income (in thousand USD).}

\item{lambda_s}{Income elasticity for staple goods. Unit change in per capita demand for staples for unit change in income (in thousand USD).}

\item{k_s}{Exponent of Income level at which staple demand is anticipated to be at its highest. Log of this term (log(k_s)) is the income level in Thousand USD.k_s and lamda_s are used in conjunction to derive the income elasticity for staples.}

\item{Pm}{Price of materials in $ per capita per day. This is basically the price consumers pay for goods in the economy others than food. The range based on the statistical formulation is 1.94 - 5.91.}

\item{psscl}{Additional scaling term used to derive the expenditure shares for staples. This is applied to price of staples (Ps/Pm * psscl), where Ps is the price of staples and Pm is the price of materials and to the expenditure shares of staples (alpha_s).}

\item{pnscl}{Additional scaling term used to derive the expenditure shares for non-staples. This is applied to price of non-staples (Pn/Pm * pnscl), where Pn is the price of non-staples and Pm is the price of materials and to the expenditure shares of non-staples (alpha_n).}
}
\value{
Parameter structure suitable for use in \code{\link{food.dmnd}}.
}
\description{
This function allows the user to pass in a vector of each of the 11 parameters for ambrosia.
The user can set individual parameter values described below or can pass in a direct vector of 11 parameters.
We also look at the number of
parameters passed in.  If it is 10, we assume you want a constant income elasticity for staples (etas = constant).  If
it's 11, we assume you want to use a dynamic income elasticity for staples (etas = eta.s(lambda, k)).  If it's anything else,
we throw an error. The table in the details section provides a description of the parameters, the units and the acceptable range.
}
\details{
The documentation provides an explanation of the parameters along with acceptable ranges for the parameters.
The default parameters are the parameter values that yielded the maximum log likelihood values duing the statistical fitting procedure.
The range of the parameters is derived by filtering the range of parameters in the MCMC for the 95th percentile confidence interval.
The parameters are not independent of each other.
Note that xi_cross is used for both xi_sn and xi_ns, forcing them to be equal.

If there are only 10 parameters, then the first 8 are as above,
and the next to last is eta_s.

The parameters in the vector are described in the table below:\tabular{lllll}{
   Parameter name \tab Description \tab Units \tab Value \tab Range of parameter values (Based on 95th percentile confidence interval) \cr
   \code{A_s} \tab A scale term used to derive expenditure share for staple demand \tab Unitless \tab 1.28 \tab 1.25 -1.40 \cr
   \code{A_n} \tab A scale term used to derive expenditure share for non-staple demand. \tab Unitless \tab 1.14 \tab 0.9 - 1.16 \cr
   \code{xi_ss} \tab Price elasticity of staple goods. Unit change in per capita demand for staples as a result of unit increase in price \code{Ps} (in $ per person per day). \tab Elasticity \tab -0.19 \tab -0.27 - -0.07 \cr
   \code{xi_cross} \tab Cross price elasticity between staples and non-staples which in combination with the other price elasticities is used to derive substitution elasticity. \tab Elasticity \tab 0.21 \tab 0.09 - 0.27 \cr
   \code{xi_nn} \tab Price elasticity of non-staple goods. Unit change in per capita demand for non-staples as a result of unit increase in price \code{Pn} (in $ per person per day). \tab Elasticity \tab -0.3 \tab -0.46 - -0.10 \cr
   \code{nu1_n} \tab Income elasticity for non-staple goods. Unit change in per capita demand for non-staples for unit change in income \code{Y} (in thousand USD). \tab Elasticity \tab 0.5 \tab 0.46 - 0.61 \cr
   \code{lambda_s} \tab Income elasticity for staple goods. Unit change in per capita demand for staples for unit change in income \code{Y} (in thousand USD) \tab Elasticity \tab 0.1 \tab 0.075 - 0.16 \cr
   \code{k_s} \tab Exponent of Income level at which staple demand is anticipated to be at its highest \tab Thousand USD \tab 16 \tab 10 -17 \cr
   \code{psscl} \tab Additional scaling term used to derive the expenditure shares for staples. This is applied to price of staples (\code{Ps}/\code{Pm} * \code{psscl}), where Ps is the price of staples and Pm is the price of materials and to the expenditure shares of staples (\code{alpha_s}). \tab Unitless \tab 100 \tab 80 - 120 \cr
   \code{pnscl} \tab Additional scaling term used to derive the expenditure shares for non-staples. This is applied to price of non-staples (\code{Pn}/\code{Pm} * \code{pnscl}), where Pn is the price of non-staples and Pm is the price of materials and to the expenditure shares of non-staples (\code{alpha_n}). \tab Unitless \tab 20 \tab 18 - 25 \cr
}
}
