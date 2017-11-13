---
title: "Paper1-figs-tables"
author: "Robert Link"
date: "August 7, 2017"
output: word_document
---


```r
knitr::opts_chunk$set(echo = FALSE)

source('paper-plots.R')
```

```
## [1] "srcdir=  ."
```

```r
## read the observational data
datadir <- '../../data'
setwd(datadir)
alldata <- read.csv('food-dmnd-price-allrgn.csv')
xval.rgn.trn <- read.csv('xval-byrgn-trn.csv')
xval.rgn.tst <- read.csv('xval-byrgn-tst.csv')
xval.yr.trn <- read.csv('xval-byyear-trn.csv')
xval.yr.tst <- read.csv('xval-byyear-tst.csv')

## read the mc results:  we assume that the data has all been gzipped
setwd('../runs')
# mcrslt.all <- read.mc.data('mc-food-dmnd.allrgn.dat.gz')
# mcrslt.rgn <- read.mc.data('mc-food-dmnd.xval-byrgn.dat.gz')
# mcrslt.yr <- read.mc.data('mc-food-dmnd.xval-byyear.dat.gz')
```

## Maximum Likelihood Parameter Sets

All data:


Cross-validation by region:


Cross-validation by year:


## Figure 1


