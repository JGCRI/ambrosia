## mcpar-analysis.R: A set of (hopefully) useful functions for
## analyzing the results of Monte Carlo calculations.

## Many of these are built specifically around the format that the old C++ Monte
## Carlo code used to store and represent its data.  Newer samplers may format
## their data a little differently, so to deal with those we may need to come up
## with some sort of conversion function, if it seems like these functions are
## worth keeping.

#' Read the Monte Carlo results file
#'
#' @param filename Name of the file where the results are stored
#' @param varnames Names of the variables stored in the file (the MC output
#' format didn't have column headers). If omitted, a default set of names is
#' supplied.
#' @export
read.mc.data <- function(filename, varnames=namemc())
{
    ## This is just a thin wrapper around the read.table function.  If
    ## names are given, we assign them; if not, then we just tag the
    ## last column as the log likelihood value.
    data <- utils::read.table(filename)

    if(!is.null(varnames))
        names(data) <- varnames
    else
        names(data)[ncol(data)] <- "LL"
    data
}


#' Create a density plot for all of the MC variables
#'
#' @param mc.data Data frame of Monte Carlo output.
#' @export
mcparam.density <- function(mc.data)
{
    value <- NULL

    data.m <- reshape2::melt(mc.data)

    ggplot2::ggplot(data.m, ggplot2::aes(x=value)) +
      ggplot2::facet_wrap(~variable, scales='free') +
      ggplot2::geom_density(fill='grey')
}

#' Sample the MC results using bootstrap sampling.
#'
#' Optionally, apply a function to the sampled values.
#'
#' If \code{func} is \code{NULL}, the return value will be a data frame of
#' sampled parameter values.  Otherwise the return value will be a list with the
#' data frame just described in the first element and the output of \code{func}
#' in the second.
#'
#' @param mc.data Data frame of Monte Carlo output
#' @param nsamp Number of samples to draw
#' @param func Optional function to apply to the samples drawn from the MC
#' distribution.
#' @return Data frame or list (see details)
#' @export
mcparam.sample <- function(mc.data, nsamp=100, func=NULL)
{
    mcsamp <- mc.data[sample.int(nrow(mc.data), size=nsamp, replace=TRUE),]

    if(!is.null(func)) {
        fvals <- apply(mcsamp, 1, func)
        list(mcsamp, fvals)
    }
    else {
        mcsamp
    }
}

#' Get the maximum a-posteriori (MAP) parameters
#'
#' The MAP parameters are those that produced the largest posterior probability
#' density.
#'
#' @param mc.data Data frame of Monte Carlo output
#' @export
mcparam.ML <- function(mc.data)
{
    ## Return the maximum likelihood parameters as a vector
    nparam <- if('iter' %in% names(mc.data))
                  ncol(mc.data) - 2          # assumes iter column is at the end
              else
                  ncol(mc.data) - 1
    v <- mc.data[which.max(mc.data$LL),] %>% as.matrix %>% as.vector
    v[1:nparam]
}


#' Filter a Monte Carlo distribution by quantiles
#'
#' This filters on all parameter simultaneously, so any sample that is outside
#' the requested quantile range in the marginal distribution of any parameter
#' will be excluded.
#'
#' The log-posterior column is handled a bit differently; it isn't filtered on
#' the high end, just the low end.
#'
#' The purpose of this function was to trim values far out on the tails that
#' made the plot scales impossible to read.
#'
#' @param mc.data Data frame of Monte Carlo output
#' @param qlo Lower quantile for filtering
#' @param qhi Upper quantile for filtering.
#' @return Logical vector with \code{TRUE} for rows that are in the main body of
#' the distribution, \code{FAlSE} for those that aren't.
mcparam.clip.tails <- function(mc.data, qlo=0.01, qhi=0.99)
{
    quants <- apply(mc.data, 2, function(x) {stats::quantile(x,probs=c(qlo,qhi))})
    ## We want to make an exception for the log-likelihood.  Don't
    ## clip its upper end.  Log-likelihood is in the last column, and
    ## its theoretical maximum is zero.
    quants[2,ncol(quants)] <- 0.0

    keep <- apply(mc.data, 1, function(x){all(x>quants[1,] & x<quants[2,])})

    mc.data[keep,]
}


#' Get the iteration count for a monte carlo dataset.
#'
#' This is only necessary for the C++ MC code, due to some limitations in the
#' way it records its output.
#'
#' @param niter Total number of iterations in the mcpar loop.
#' @param nproc Number of processors allocated to the calculation.
#' @param npset Number of parameter sets per processor
#' @export
mcparam.itercount <- function(niter, nproc, npset)
{
    ## Return a vector giving the iteration count for a dataset output
    ## by the monte carlo calc.  This is less straightforward than
    ## just numbering the rows of the dataset sequentially for two
    ## reasons.  First, because of the parallel markov chains, there
    ## are many data points at each iteration.  Second, the iterations
    ## are not output sequentially; they are gathered together into
    ## batches and dumped periodically.
    ##
    ## TODO: This is really brittle.  It would be better just to
    ## record the iteration number in the output.
    ##
    ## niter:  number of iterations in the mcpar loop
    ## nproc:  number of processors allocated to the calculation
    ## npset:  number of parameter sets per processor
    ##
    ## Return value: vector of iteration sequence numbers

    ntot <- niter*nproc*npset
    nchain <- nproc*npset
    outstep <- if(niter > 50)
                   niter/10
               else
                   5
    nbatch <- niter %/% outstep           # This will probably cause a problem if niter is not divisible by outstep.

    out.batch <- seq(0,ntot-1) %/% (nbatch*nchain) # output batch number for each output slot (start count at 0)

    ## A batch is a series of nproc blocks, each originating from one
    ## processor.  The structure within each block is identical, so we
    ## can build up the structure of a block and repeat it.  The
    ## batches themselves will be repeated below, so it isn't
    ## necessary to construct the batch structure; we can just repeat
    ## the block structure as required.
    nblock <- outstep*npset
    seq.block <- 1 + seq(0,nblock-1) %/% npset

    ## Now the iteration number is the batch sequence number plus outstep*(batch #)
    out.batch * outstep + seq.block
}


#' Return the list of names for the parameters in the model.
#'
#' Supports both the 7 and 9 parameter version.  Adds the "LL" tag to the end to
#' cover the log likelihood column appened by the monte carlo code.
#'
#' The 9 parameter version of the model is the canonical version (and the one
#' published in the paper)
#'
#' @param nparam Number of parameters.  Legal values are either 8 or 9 or 11.
#' @return A vector of parameter names for the selected version of the model.
#' @export
namemc <- function(nparam=9)
{
    ##
    ##
    if(nparam == 8) {
        c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n", "eps.s", "Pm", "LL")
    }
    else if(nparam == 9) {
        c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n", "lambda", "ks", "Pm", "LL")
    }
    else if(nparam == 11) {
    c("As", "An", "xi.ss", "xi.cross", "xi.nn", "eps1n", "lambda", "ks", "Pm","psscl","pnscl", "LL")
  }
    else {
        stop("namemc:  nparam must be 8 or 9 or 11.  nparam= ", nparam)
    }
}


