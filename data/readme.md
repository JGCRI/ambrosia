# About the input data

## Processing

The data in this directory was processed from the data in the
`rawdata` subdir using the `FAOStat_commod_matching.R` script.  The
table produced by this script was run through the function
`assign.sigma.Q` (located in `util.R`) to produce the `sig2Qn` and
`sig2Qs` columns, and the resulting table was saved as the primary
input data file, `food-dmnd-price-allrgn.csv`.  Previous versions of
the data set also had a collection of data files with the data for
each individual region (i.e., the data in the main file, split by the
value of the `GCAM_region_name` column).  These files were removed
when it became apparent that individual regional modeling was not a
useful thing to do.  The function for producing the split dataset is
called `write.fddata` and is retained in `util.R`, in case someone
wants to analyze the regional tables again for some reason.

## Cross-validation data sets

The cross validation data sets were produced by splitting the data
into a training set (approximately 2/3 of the data) and a testing set
(approximately 1/3).  The purpose of this split is to allow us to
estimate model parameters using only the data in the training set and
then to evaluate model performance on the data that was held back
during the parameter estimation step.

We chose two methods for splitting the data to make these data sets.
The first uses the year to determine the split.  All years prior to
2004 were added to the training set, and 2004--2011 were added to the
testing set.  We chose this method instead of random selection because
we expect that the data are not entirely independent from year to
year; therefore, randomly selected test years (or, for that matter,
randomly selected rows from the data set) are unlikely to provide
an independent measure of the model's performance.  This cross
validation split tests the model's robustness over time.

The second cross validation method splits the training and testing
sets by region.  Of the thirty regions in the data set, 10 were
selected at random to form the testing set.  The remining regions were
assigned to the training set.  The 10 regions in the testing set are:  
* Mexico
* Hong Kong and Macau
* Pakistan
* Africa, Northern
* Central Asia
* Europe, Eastern
* South Korea
* Australia and New Zealand
* Brazil
* South America, Northern  


