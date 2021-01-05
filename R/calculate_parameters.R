##Functions to estimate parameters


#' Calculates the 11 parameters for ambrosia using data calculated by \code{\link{create_dataset_for_parameter_fit()}} by maximizing log likelihood
#'
#'@details The following steps are involved in the parameter estimation function.
#'
#'1) First a log-likelihood function is set up with the data.
#'
#'2) Next, the value returned by the log-likelihood function will be maximized using optim(). The user can provide
#'  a seed of initial parameters to begin the optimization process (the lowest possible seed would be the lowest
#'  values of all 11 parameters). The default seed is set to the original parameters from Edmonds et al (2017). The user
#'  can now specify the optimization method to be used. The default is set to the "BFGS" method, but the user can
#'  also run the optimization using methods such as "Neldor-Mead".
#'
#'3) Finally, the function will now return a vector of parameters that can be used to derive estimates of food demand (using the \code{\link{food.dmnd}} function). The function also prints out the maximized value of the log-likelihood function, so that the user can verify the efficiency and effectiveness of the parameter estimation.
#'
#' @return A vector with the 11 parameters which are as follows,
#'
#' A_s : A scaling term for staple commodities
#'
#' A_n : A scaling perm for nonstaples commodities
#'
#' xi_ss : Price elasticity for staples
#'
#' xi_cross : Cross price elasticity
#'
#' xi_nn : Price elasticity for non-staples
#'
#' nu_1n : Income elasticity for non-staples
#'
#' lambda_s : Income elasticity for staples
#'
#' k_s : Income level at which total demand begins saturating
#'
#' Pm : Scaling parameter for price of materials (everything other than staples and non-staples)
#'
#' psscl : Price scaling parameter for staples that is applied to the Price of staples (Ps) and the alpha of staples
#'
#' pnscl : Price scaling parameter for staples that is applied to the Price of non-staples (Pn) and the alpha of non-staples
#' @param original_param_vector Original parameter vector to be used as the starting point for the optimization.These parameters are taken from Edmonds et al 2017
#' @param optim_method The optimization method to be used for maximization of the log likelihood. The default is set to BFGS
#' @param datadir Directory to the data calculated by \code{\link{create_dataset_for_parameter_fit()}}
#' @param outdir Directory to store output csv. Default is set to test_output folder.
#' @param max_iterations A maximum number of iterations that can be passed to optim. This is largely meant for testing purposes.Default is set to 100 for BFGS.
#' @param print_progress A parameter that allows the user to track progress of function.
#' @param trace_param Setting this to TRUE will generate a .dat file with all parameter samples and log likelihhod value during a parameter fitting exercise (eg. MCMC)
#' @author KBN 2020
#' @importFrom  stats optim
#' @importFrom utils write.csv write.table
#' @export
calculate_ambrosia_params <- function(optim_method = "BFGS",
                                     original_param_vector= c(1.28,1.14,-0.19,0.21,-0.33,0.5,0.1,16,5.06,100,20),
                                     datadir= "outputs/Processed_Data_for_MC.csv",
                                     outdir="tests/testthat/test_outputs/",
                                     max_iterations= 100,
                                     print_progress= FALSE,
                                     trace_param = FALSE){

    #Step 1: Read in raw data


    #Read in original co-efficents as vector. These were taken directly from the paper.
    original_param_vector <- original_param_vector



    #Now create likelihood function with new data
    func_MC <- mc.setup(datadir,trace_param = trace_param)


    #Now calculate new parameters
    if(print_progress){
    print("Fitting parameters by maximizing log likelihood")}
    params_vector <- optim(original_param_vector, func_MC, control=list(fnscale=-1, maxit=max_iterations), method = optim_method)

    #If the optimizer does not work, or runs out of iterations, try again. It works in the first try.
    if(params_vector$convergence != 0){
        params_vector <- optim(c(params_vector$par), func_MC, control=list(fnscale=-1,maxit=max_iterations),method = "BFGS")

    }


    #Check probability density of original likelihood function
    func_MC(params_vector$par) -> New_probability_density

    if(print_progress){
        print(paste0("Maximum value of log likelihood function is ",params_vector$value))}


    parameter_names<-c('A_s', 'A_n', 'xi_ss', 'xi_cross', 'xi_nn', 'nu1_n',
                       'lambda_s', 'k_s', 'Pm','psscl','pnscl')
    parameter_data <- data.frame(parameter_names, params_vector$par)
    write.csv(parameter_data, paste0(outdir,"parameter_data.csv"), row.names = FALSE)

    return(params_vector$par)

}

#'Create dataset with observational error for log likelihood calculation using clustering.
#'
#'
#'There are a few steps that the function will perform on a sample dataset (see details).
#'
#' The steps that will be performed by the function are :
#'
#'1) It will ensure that the user's dataset contains all columns required for parameter estimation.
#'
#'2) It will filter out anomalies and outliers using parameterized cutoff values selected by the user.
#'   This step is necessary since data on food consumption and prices are often incomplete which may lead to
#'   unrealistically high or low values of consumption or prices in the dataset.
#'
#'3) After this, the function will create clusters of observations from the dataset based on income levels,
#'   and prices of staples and non-staples. This step is necessary because this being economic data,
#'   the observational error can only be calculated within different clusters. The code will also check for a  user
#'   specified minimum number of clusters (if there are anomalies within the dataset, the clustering can be incorrect leading in a small number of clusters).
#'   The clustering is implemented using the Divisive Analysis Clustering Algorithm (DIANA).
#'
#'4) Once the clustering is completed, the code will calculate the observational error
#'   which is the variance in food demand for staples and non-staples .Note that the user can chose a
#'   lower limit on the observational error calculated. The default value of the lower limit is 0.01.
#'
#'
#' @return A dataframe  called Processed_Data_for_MC with the following columns
#'
#' sig2Qn- which is the observational error for non-staples
#'
#' sig2Qs- which is the observational error for staples
#'
#' @param min_cal_fd Minimum calories for the food demand model
#' @param min_clusters Minimum number of clusters to be generated by clustering algoritm. It is recommended to not lowewr this parameter below 20.
#' @param lower_limit_sigma Lower limit for sigma values calculated
#' @param data A data.frame or data.table with the raw data. Data should contain following names,
#'
#' {s_cal_pcap_day_thous} (Containing 1000 calories per capita per day for staples)
#'
#' {ns_cal_pcap_day_thous} (Containing 1000 calories per capita per day for non-staples)
#'
#' {gdp_pcap_thous} (Containing GDP per capita)
#'
#' {s_usd_p1000cal} (Price of 1000 calories for staples per person per day)
#'
#' {ns_usd_p1000cal} (Price of 1000 calories for non-staples per person per day)
#'
#' @param outdir Directory to store output csv. Default is set to test_output folder.
#' @param print_progress A parameter that allows the user to track progress of function.
#' @importFrom dplyr mutate filter rename
#' @author KBN 2020
#' @export
create_dataset_for_parameter_fit <- function(
                                             min_cal_fd = 1000,
                                             min_clusters = 300,
                                             lower_limit_sigma = 0.01,
                                             data=NULL,
                                             outdir="tests/testthat/test_outputs/",
                                             print_progress = FALSE){

    s_cal_pcap_day_thous <- ns_cal_pcap_day_thous <- ns_usd_p1000cal <- Tot_cal <- gdp_pcap_thous <-
        s_usd_p1000cal <- Ps <- sig2Qs <- sig2Qn <- gdp_pcap_thous2005usd <- Pn <- NULL

    `%notin%` <- Negate(`%in%`)
    data <- as.data.frame(data)
    req_colnames <- c("s_cal_pcap_day_thous","ns_cal_pcap_day_thous","gdp_pcap_thous","s_usd_p1000cal","ns_usd_p1000cal","pop_thous")

    #Always check that we have the required columns
    for (i in req_colnames){
        if(i %notin% c(colnames(data))){

            stop("Error: Required columns s_cal_pcap_day_thous, ns_cal_pcap_day_thous, gdp_pcap_thous,s_usd_p1000cal,ns_usd_p1000cal,pop_thous are not in the dataset.")

        }}
    #If income is below 0.1 USD, you cannot assign a budget for food demand. This code checks for the same.
    if(any(data$gdp_pcap_thous < 0.1)){
        stop("Income levels for some observations are below 0.1 USD. This will cause a crash on the final food demand calculations.")
    }

    #Currently adding a status column to be transparent about the observations that we are including and excluding.
    raw_data <- data %>%
        #Calculate total calories
        mutate(Tot_cal=s_cal_pcap_day_thous+ns_cal_pcap_day_thous) %>%
        #Only keep prices less than 20 dollars per 1000 USD
        filter(ns_usd_p1000cal< min_price_pd) %>%
        #Minimum calories = 1700
        filter(Tot_cal>min_cal_fd/1000) %>%
        rename(gdp_pcap_thous2005usd= gdp_pcap_thous) %>%
        mutate(gdp_pcap_thous2005usd=gdp_pcap_thous2005usd/1000)


    #Calculate population weights
    raw_data <- calc.pop.weight(raw_data)

    #Calculate observational errors
    raw_data <- assign.sigma.Q(raw_data)

    #Check number of observational errors. Anything under 20 means that there are a few observations skewing the distribution.
    if (length(unique(raw_data$sig2Qn)) < min_clusters){stop("Number of clusters for observational errors for non-staples are under the threshold. Please revisit data.")}
    if (length(unique(raw_data$sig2Qn)) < min_clusters){stop("Number of clusters for observational errors for non-staples are under the threshold. Please revisit data.")}

    #Some processing for data.
    # We need the following columns- Ps,Pn,Qs,Qn,Y,sig2Qn,sig2Qs,weight.
    raw_data %>% rename(Ps=s_usd_p1000cal,Pn=ns_usd_p1000cal,Qs=s_cal_pcap_day_thous,Qn=ns_cal_pcap_day_thous,
                        Y=gdp_pcap_thous2005usd) %>%
        #We want prices in thousands of USD for a year's consumption at 1000 calories per day (Hence, multiply this by 0.365). Lower limit sig2Qn , sig2Qs to 0.01
        mutate(Ps= Ps*0.365, Pn= Pn*0.365, sig2Qs=pmax(lower_limit_sigma,sig2Qs)
               ,sig2Qn=pmax(lower_limit_sigma,sig2Qn)) -> processed_data_for_mc


    #Write to a csv. We need this for calculating the likelihood function.
    write.csv(processed_data_for_mc, paste0(outdir,"Processed_Data_for_MC.csv"),row.names = FALSE)

    if(print_progress){
        print("Completed processing raw data and created clusters. Check output folder for Processed_Data_for_MC.csv")}
    return(processed_data_for_mc)

}
