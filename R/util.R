
#' Partition input data into clusters with a minimum number of members
#'
#' Partition the input data into clusters with a given minimum number of members
#' per cluster.  This turns out to be kind of hard to do because the structure
#' produced by the clustering algorithm isn't conducive to finding the cluster
#' that a too-small cluster was split from.  Instead, we find a dendrogram cut
#' that produces all clusters larger than the minimum.  Then we recursively
#' partition all of the clusters that are large enough that they could be split
#' in two.
#'
#' The purpose of this function is to allow us to estimate the observational
#' error in historical food demand observations.  By clustering observations
#' that have similar input values (prices, GDP) we can get something
#' approximating repeated measurements of similar situations.  Setting a minimum
#' number of members per cluster allows us to have enough measurements per
#' grouping to get a resasonable estimate of the variance.
#'
#' This function returns a list of data frames, with each list item being a
#' single cluster.  This irretrievably scrambles the order of the rows, so if
#' recovering the original order is important, include an ID column.
#'
#' @param input.data Data frame of data to be clustered.
#' @param cluster.vars Vector of names of variables to use in the clustering.
#' @param min.members Desired minimum number of members per cluster.
#' @return List of data frames, one data frame for each cluster.
#' @importFrom dplyr %>%
#' @export
recursive.partition <- function(input.data, cluster.vars, min.members=5)
{
    . <- NULL

    minsplit <- 2*min.members           # clusters smaller than this can't be split

    if(nrow(input.data) < minsplit) {
        ## no split possible.  We will just
        cluster.list <- list(input.data)
    }
    else {
        ## run the clustering analysis on the input data
        dv <- cluster::diana(input.data[,cluster.vars], metric='manhattan', stand=TRUE)
        ## find the largest number of clusters that will ensure that
        ## no cluster has less than min.members.
        k <- floor(nrow(input.data) / min.members)
        count <- stats::cutree(dv,k) %>% table %>% min    # count number in each cluster and find the smallest.
        while(count < min.members) {
            ## This loop is guaranteed to terminate because when k=1
            ## all the rows go into a single cluster.  Also, we could
            ## do a binary search here, but cutting the tree is fast,
            ## so it's easier just to scan.
            k <- k-1
            count <- stats::cutree(dv,k) %>% table %>% min
        }

        if(k==1) {
            ## can't split this cluster without creating orphans
            cluster.list <- list(input.data)
        }
        else {
            cluster.list <- stats::cutree(dv,k) %>% split(input.data, .)

            ## check if any clusters have more than minsplit members
            if(any(sapply(cluster.list, nrow) >= minsplit)) {
                ## apply this function recursively to everything in the
                ## list.  It will be a no-op on clusters that are too
                ## small to split recursively.
                cluster.list.list <- lapply(cluster.list,
                                            . %>% recursive.partition(cluster.vars, min.members))
                cluster.list <- unlist(cluster.list.list, recursive=FALSE)
            }
        }
    }

    ## We now have a list of one or more data frames, as promised.  As
    ## an added bonus, the names() attribute of the list is the
    ## cluster number.  This number gets appended in a predictable way
    ## when we recurse, so if the first cluster gets split into 3
    ## subclusters, they will be 1.1, 1.2, and 1.3.  If the second of
    ## those is further split in two, those will end up being 1.2.1
    ## and 1.2.2, and so on.
    cluster.list
}


#' Assign observational errors to observed demand quantities
#'
#' Assign sigma (observational error) values for Qs and Qn in an input data set.
#' We do this by clustering the input on Ps, Pn, and Y and then taking the
#' variance of the Qs and Qn in each cluster.
#'
#' Observational errors are estimated by clustering observed data by the demand
#' model input values (i.e., prices for staple and nonstaple foods) and
#' calculating the variance of observations in each cluster.  In order for this
#' to work, you have to ensure that there are enough observations in each
#' cluster to produce a variance that is at least somewhat reliable.  The
#' tradeoff here is that the larger you make the clusters, the better the
#' variance estimate is, but less alike the observations in the cluster actually
#' are (meaning some of the variance is not observational error, but actual
#' difference in demand.  This tradeoff is controlled by the \code{min.group}
#' argument.
#'
#' @param input.data Data frame of observational input.
#' @param min.group Minimum group size for clustering
#' @return A dataframe updated with the calculated observational error (sig2Qs and sig2Qn columns)
#' @importFrom dplyr %>%
#' @export
assign.sigma.Q <- function(input.data, min.group=5)
{
    . <- sig2Qn <- sig2Qs <- ns_cal_pcap_day_thous <- s_cal_pcap_day_thous <-
        var <- ID <- NULL

    ## assign a sequence number to each row so that we can sort them
    ## back into their original order when we're done.
    input.data$ID <- seq(1,nrow(input.data))
    cluster.vars <- c('gdp_pcap_thous2005usd', 'ns_usd_p1000cal', 's_usd_p1000cal')
    cluster.list <- recursive.partition(input.data, cluster.vars, min.group)

    ## Add the cluster identifier to each data frame in the list.  See
    ## note at the end of recursive.partition().
    for(clus in names(cluster.list)) {cluster.list[[clus]]$clusterID <- clus}

    ## calcuate the desired variances for each group
    cluster.list <- lapply(cluster.list,
                           . %>% dplyr::mutate(sig2Qn = var(ns_cal_pcap_day_thous),
                                               sig2Qs = var(s_cal_pcap_day_thous)))
    ## put back in master list and rearrange
    new.data <- do.call(rbind,cluster.list) %>% dplyr::arrange(ID)

    ## Drop the ID variable
    new.data$ID <- NULL

    new.data
}

#' Calculate a weight factor based on the population.
#'
#' These weight factors can be used to give high population regions more
#' influence in the model fit relative to low population regions.
#'
#' @param input.data Data frame of food demand input data from FAO
#' @return Data frame of input data with a population weight column added.
#' @export
calc.pop.weight <- function(input.data)
{
    pop_thous <- NULL

    popmax <- max(input.data$pop_thous)
    dplyr::mutate(input.data, weight=pop_thous/popmax)
}


#' Launch the interactive GCAM food demand model
#'
#' The interactive model allows users to adjust model parameters and see how
#' they affect the model output.  To run the interactive model you must have the
#' R "shiny" package installed.
#'
#' @export
runapp <- function()
{
    if(!requireNamespace('shiny', quietly=TRUE)) {
        stop('Running the interactive model requires the R "shiny" package')
    }
    appdir <- system.file('app', package='ambrosia', mustWork=TRUE)
    shiny::runApp(appdir)
}

