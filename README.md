![R-CMD](https://github.com/JGCRI/ambrosia/workflows/R-CMD/badge.svg) ![build](https://github.com/JGCRI/ambrosia/workflows/build/badge.svg) [![codecov](https://codecov.io/gh/JGCRI/ambrosia/branch/master/graph/badge.svg)](https://codecov.io/gh/JGCRI/ambrosia)


# `ambrosia`: An R package for calculating and analyzing food demand and in accordance with the Edmonds et al. food demand model

## Summary
The `ambrosia` R package was developed to calculate food demand for staples and non-staple commodities that is responsive to changing levels of incomes and prices. Ambrosia implements the framework to quantify food demand as established by Edmonds et al. (2017) and allows the user to explore and estimate different variables related to the food demand system. Currently `ambrosia` provides three main functions:
1. calculation of food demand for any given set of parameters including income levels and prices,
2. estimation of parameters within a given a dataset.  Note:  `ambrosia` is used to calculate parameters for the food demand model implemented in the Global Change Analysis Model (GCAM; Calvin et al. 2019)
3. exploration and preparation of raw data before starting a parameter estimation.


## Getting Started with `ambrosia`
...


## Examples
```r
ps <- 0.2
pn <- 0.5
y <- seq(0.2, 10.0, 0.2)
rslt <- food.dmnd(ps, pn, y, samp.params)

```
Note that the  are parameters available in the folder parameter_data/parameter_data.csv

The parameters can be re-calculated using the Calcute_parameters.R script. 
The input data can be recalculated using the script Process_Demand_Data.R

## Contributing to `ambrosia`
We welcome contributions to `ambrosia` from the development community. Please contact us if you want to collaborate! The `ambrosia` GitHub repository is accessible here: [GitHub Repository](https://github.com/JGCRI/ambrosia)
