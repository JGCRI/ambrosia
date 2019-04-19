# Running the GCAM food demand model from C++

This code shows how we used to use RInside to run the GCAM food demand
model from a C++ program.  At one time this was how we used our C++
Markov Chain Monte Carlo code with the model, but these days we've
switched over to R-native MCMC packages.  On top of that, this code is
still configured to work with the bad old "pile of scripts" code
instead of the new package code, so it doesn't even work anymore.
Nevertheless, if someone really wants to run the model from C++, it
should be pretty easy to rework this example to run with the new
version.

