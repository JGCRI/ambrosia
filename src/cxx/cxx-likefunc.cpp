#include <iostream>
#include <string>
#include <vector>
#include <RInside.h>

int main(int argc, char *argv[])
{
  const int nparam=8;           // number of parameters in the model.
  float p0[] = {0.5, 0.35, -0.03, 0.01, -0.4, 0.5, 0.1442695, 5.436564}; // first set of parameters to eval
  float p1[] = {0.7502500,  0.2507500, -0.5000000,  0.5000000, -1.5000000,  1.1375000,  1.2500000,  2.5007498};
  float p2[] = {0.2507500,  0.7502500, -1.5000000,  0.5000000, -0.5000000, 0.4125000,  3.7500000,  7.5002494};
  float p3[] = {0.2507500,  0.7502500, -1.5000000,  0.5000000, -0.5000000, 0.4125000,  -3.7500000,  7.5002494}; // should be invalid
  
  RInside R(argc, argv);        // initialize the embedded R instance
  std::string Rcode("../R/food-demand-mc.R");
  std::string obsdata("../../data/food-dmnd-price-allrgn.csv");
  if(argc > 1) 
    obsdata = argv[1];

  std::string Rsrccmd = "source('" + Rcode + "')";
  
  // load the R source
  R.parseEvalQ(Rsrccmd); // eval the command; do not return a value

  // set up a parameter vector with all three parameter sets.
  Rcpp::NumericVector x(4*nparam);

  for(int i=0; i<nparam; ++i) {
    x[i]          = p0[i];
    x[i+nparam]   = p1[i];
    x[i+2*nparam] = p2[i];
    x[i+3*nparam] = p3[i];
  }
  R["input.params"] = x;        // store x in R instance as input.params
  R["input.npset"] = 4;
  R["input.mpi.rank"] = 0;

  std::cout << "Ready to setup:\n";
  // set up the likelihood function
  std::string lfsetup = "mc.setup('" + obsdata + "')";
  Rcpp::NumericMatrix plohi = R.parseEval(lfsetup); 
  std::cout << "Setup complete:  nparam = "
            << plohi.ncol() << "\n";
  std::cout << "plohi = \n";
  for(int i=0; i<plohi.nrow(); ++i) {
    std::cout << "\t";
    for(int j=0; j<plohi.ncol(); ++j)
      std::cout << plohi(i,j) << "  ";
    std::cout << "\n";
  }

  // evaluate the likelihood function and return values
  std::string lfcmd("mc.likelihood(input.params, input.npset)");
  std::vector<float> lf = Rcpp::as<std::vector<float> >(R.parseEval(lfcmd));

  std::cout << "Likelihood function values:\n";
  for(int i=0; i<lf.size(); ++i) {
    std::cout << i << ":  " << lf[i] << "\n";
  }

  return 0;
}
