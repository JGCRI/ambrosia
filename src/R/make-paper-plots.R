## This script clears out the workspace, loads the food demand data
## sets, and runs the functions in paper-plots.R, leaving the results
## in the workspace.

## WARNING: This script starts by deleting everything in the
## workspace.  It also messes around with your current working
## directory.  Don't run it unless that's what you want to do.  If you
## just want to load the functions for making the paper plots, look at
## paper-plots.R.

rm(list = ls())
srcdir <- dirname(normalizePath(sys.frame(1)$ofile))
source(file.path(srcdir,'paper-plots.R'))

## read the observational data
print('Reading data.')
datadir <- normalizePath(file.path(srcdir, '../../data'))
setwd(datadir)
alldata <- read.csv('food-dmnd-price-allrgn.csv')
xval.rgn.trn <- read.csv('xval-byrgn-trn.csv')
xval.rgn.tst <- read.csv('xval-byrgn-tst.csv')
xval.yr.trn <- read.csv('xval-byyear-trn.csv')
xval.yr.tst <- read.csv('xval-byyear-tst.csv')

## read the mc results:  we assume that the data has all been gzipped
setwd('../runs')
mcrslt.all <- read.mc.data('mc-food-dmnd.allrgn.dat.gz')
mcrslt.rgn <- read.mc.data('mc-food-dmnd.xval-byrgn.dat.gz')
mcrslt.yr <- read.mc.data('mc-food-dmnd.xval-byyear.dat.gz')

## find the maximum likelihood parameter sets
pv.all <- mcparam.ML(mcrslt.all)
p.all <- mc2param(pv.all)

pv.rgn <- mcparam.ML(mcrslt.rgn)
p.rgn <- mc2param(pv.rgn)

pv.yr <- mcparam.ML(mcrslt.yr)
p.yr <- mc2param(pv.yr)

### Make the plots.  This is a bit of overkill, since we don't actually
### want all of these.

## Histogram of sigma values:  full obs data set only
print('Running:  obs plots (output = hist.sigma)')
hist.sigma <- make.paper1.obs.plots(alldata)

## Parameter plots.  For now, make only the 'all' data set.  All we
## want from the xval data sets is the scatter plot, and we really
## want to make it a little differently than what we have been doing.
print('Running: parameter plots (output = plts.param.all)')
plts.param.all <- make.paper1.param.plots(p.all, alldata)

## Monte Carlo result plots.  Make for each experiment
print('Running:  MC plots (output = plts.mc.{all, rgn, yr})')
plts.mc.all <- make.paper1.mc.plots(mcrslt.all, alldata)
plts.mc.rgn <- make.paper1.mc.plots(mcrslt.rgn, xval.rgn.trn, xval.rgn.tst)
plts.mc.yr <- make.paper1.mc.plots(mcrslt.yr, xval.yr.trn, xval.yr.tst)

## Scatter plots for cross-validation experiment
print('Running:  Cross-validation scatter plots and RMSE. (output=resid.stats)')
resid.stats <- paper1.residual.analysis(mcrslt.rgn, mcrslt.yr,
                                         xval.rgn.trn, xval.rgn.tst, xval.yr.trn, xval.yr.tst)
## Also get the RMSE for the primary run (RMSE fro cross-validation sets was included in resid.stats)
print('Running: All-data RMSE (output=rmse.all)')
rmse.all <- paper1.rmse.all(alldata, p.all)

### Calculate the chi-squared values.  Use testing set only for xval
### experiments.
chisq.all <- paper1.chisq(p.all, alldata, 9)
chisq.rgn <- paper1.chisq(p.rgn, xval.rgn.tst)
chisq.yr <- paper1.chisq(p.yr, xval.yr.tst)

cat('Chi-squared, All data:\n')
print(chisq.all)
cat('Chi-squared, Regional xval:\n')
print(chisq.rgn)
cat('Chi-squared, Year xval:\n')
print(chisq.yr)

### Compute the bias correction for the yearly crossval
plts.bc <- paper1.bc.plots(p.yr, xval.yr.trn, xval.yr.tst)
