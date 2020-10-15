# Example of fitting parameters using an MCMC implemented in C++

This code shows how we used to use RInside to run the `ambrosia` from a C++ program.  At one time this was how we used our C++
Markov Chain Monte Carlo code with the model, but these days we've
switched over to R-native MCMC packages. If someone really wants to run the model from C++, it
should be easy to rework this example to run with the new
version.

Using this code, the user can fit a log-likelihood function in ambrosia and then use the same to run a Markov Chain Monte Carlo parameter fit.
