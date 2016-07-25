#include <iostream>
#include <string>
#include <RInside.h>

int main(int argc, char *argv[])
{
  const int nparam=9;           // number of parameters in the model.
  float p0[] = {0.5, 0.35, -0.03, 0.01, 0.05, -0.4, 0.5, 0.1442695, 5.436564}; // first set of parameters to eval
  float p1[] = {0.5, 0.35, -0.03, 0.01, 0.05, -0.4, 1.0, 0.1442695, 5.436564}; // second set of parameters to eval
  float p2[] = {0.3, 0.1, -0.05, 0.1, 0.2, -0.5, 1.0, 0.2936423, 4.5304697};   // third set of parameters to eval
  
  RInside R(argc, argv);        // initialize the embedded R instance
  std::string Rcode("../R/food-demand.R");
  std::string obsdata("../../data/obsdata-test.csv");

  std::string Rsrccmd = "source('" + Rcode + "')";
  
  // load the R source
  R.parseEvalQ(Rsrccmd); // eval the command; do not return a value

  // set up a parameter vector with all three parameter sets.
  Rcpp::NumericVector x(3*nparam);

  for(int i=0; i<nparam; ++i) {
    x[i]          = p0[i];
    x[i+nparam]   = p1[i];
    x[i+2*nparam] = p2[i];
  }
  R["input.params"] = x;        // store x in R instance as input.params

  // set up the likelihood function
  std::string lfsetup = "mc.setup('" + obsdata + "')";
  R.parseEvalQ(lfsetup);

  // evaluate the likelihood function and return values
  std::string lfcmd("mc.likelihood(input.params, 3)");
  Rcpp::NumericVector lf = R.parseEval(lfcmd);

  std::cout << "Likelihood function values:\n";
  for(int i=0; i<lf.size(); ++i) {
    std::cout << i << ":  " << lf[i] << "\n";
  }

  return 0;
}
