library(dplyr)
library(testthat)
library(ggplot2)

#Constant for diagnostics. Set this to true to view diagnostics.
diagnostics<-"FALSE"

#Load utility functions, mc functions
source("R/util.R")
source("R/food-demand-mc.R")
source("R/food-demand.R")

#Step 1: Read in raw data
#Currently adding a status column to be transparent about the observations that we are including and excluding.
Raw_Data<-read.csv("paper1/Latest_Iteration/Food_Demand_Data.csv") %>%
          filter(Status=="Include")


#Calculate population weights
Raw_Data<-calc.pop.weight(Raw_Data)

#Calculate observational errors
Raw_Data<-assign.sigma.Q(Raw_Data)

#Check number of observational errors. Anything under 20 means that there are a few observations skewing the distribution.
test_that("Number of clusters for observational errors are reasonable",{

    expect(length(unique(Raw_Data$sig2Qn))>20,"Number of clusters for observational errors for non-staples are under 20. Please revisit data.")
    expect(length(unique(Raw_Data$sig2Qs))>20,"Number of clusters for observational errors for staples are under 20. Please revisit data.")
})

#Some processing for data.
# We need the following columns- Ps,Pn,Qs,Qn,Y,sig2Qn,sig2Qs,weight.
Raw_Data %>% rename(Ps=s_usd_p1000cal,Pn=ns_usd_p1000cal,Qs=s_cal_pcap_day_thous,Qn=ns_cal_pcap_day_thous,
                    Y=gdp_pcap_thous2005usd) %>%
    #We want prices in thousands of USD for a year's consumptiona at 1000 calories per day. Lower limit sig2Qn,sig2Qs to 0.01
             mutate(Ps= Ps*0.365, Pn= Pn*0.365, sig2Qs=pmax(0.01,sig2Qs)
                    ,sig2Qn=pmax(0.01,sig2Qn))->Processed_Data_for_MC

#Plot non-staple, staples demand vs income
if(diagnostics=="TRUE"){

    #Qn
    ggplot()+geom_point(data=Processed_Data_for_MC,aes(x=Y,y=Qn))+ggtitle("Quantity of non-staples vs income")->a
    a
    #Qs
    ggplot()+geom_point(data=Processed_Data_for_MC,aes(x=Y,y=Qs))+ggtitle("Quantity of staples vs income")->b
    b
    #sig2Qn
    c<-ggplot(data=Processed_Data_for_MC,mapping=aes(sig2Qn))+
        geom_histogram(bins=15)
    c
    #sig2Qs
    d<-ggplot(data=Processed_Data_for_MC,mapping=aes(sig2Qs))+
        geom_histogram(bins=15)
    d
}


#Write to a csv. We need this for calculating the likelihood function.
write.csv(Processed_Data_for_MC,"Processed_Data_for_MC.csv",row.names = FALSE)

#Read in original co-efficents as vector. These were taken directly from the paper.
original_param_vector<-c(1.28,1.14,-0.19,0.21,-0.33,0.5,0.1,16,5.06)

#Create original likelihood function
func_MC_Original<-mc.setup("paper1/data/food-dmnd-price-allrgn.csv")

#Check probability density output with original function and original parameters
func_MC_Original(original_param_vector)->Original_prob_density

#Now create likelihood function with new data
func_MC<-mc.setup("Processed_Data_for_MC.csv")

#Check probability density output with new function and original parameters
test_that("New probability density is unreasonable",{
    expect(abs(func_MC(original_param_vector))<3000," The probability density calculated with the probability density function and original parameters is too high.Check Processed_Data_for_MC")
})


#Now calculate new parameters
# Note that the method used here as BFGS since it solves better than the Nelder-Mead simplex. Don't
#let the errors throw you. rpl mentioned that this might happen, its not an error,Only a warning. I need to fix the
#ifelse in this function at some point.
params_vector<-optim(original_param_vector, func_MC, control=list(fnscale=-1),method = "BFGS")

#If the optimizer does not work, or runs out of iterations, try again. It works in the first try.
if(params_vector$convergence != 0){
    params_vector<-optim(x0, func_MC, control=list(fnscale=-1),method = "BFGS")

}


#Check probability density of original likelihood function
func_MC_Original(params_vector$par)->New_probability_density

test_that("New probability density for original function is unreasonable.",{
    expect(abs(New_probability_density)-abs(Original_prob_density)<100,"New likelihood probability density is too different
           from original.")
})

parameter_names<-c('A_s', 'A_n', 'xi_ss', 'xi_cross', 'xi_nn', 'nu1_n',
                     'lambda_s', 'k_s', 'Pm')
parameter_data<-data.frame(parameter_names,params_vector$par)
write.csv(parameter_data,"parameter_data/parameter_data.csv",row.names = FALSE)

print("The parameters stored in parameter_data/parameter_data.csv are the  parameters for the food demand model. The rest of the script just tests the new parameters.")


#Start diagnostics for food demand model
#Read in test data
Test_Data<-data.frame(Y=seq(0.1,60, by=0.1))
#Add sample values of Ps and Pn. These are in accordance with Robert's original samp,e
Test_Data %>% mutate(Ps=0.1,Pn=0.2)->Test_Data

#Food demand outputs

Food_Demand<-food.dmnd(Test_Data$Ps,Test_Data$Pn,Test_Data$Y,params=vec2param(params_vector$par))
Food_Demand$Total_Demand<-Food_Demand$Qs+Food_Demand$Qn
Food_Demand$Y<-seq(0.1,60, by=0.1)

Food_Demand_Original<-food.dmnd(Test_Data$Ps,Test_Data$Pn,Test_Data$Y,params=vec2param(original_param_vector))
Food_Demand_Original$Y<-seq(0.1,60, by=0.1)
Food_Demand_Original %>%
    rename(Qs_original=Qs, Qn_original=Qn, Qm_original=Qm) %>%
    left_join(Food_Demand %>% select(Qs,Qn,Qm,Y),by=c("Y"))->Food_Demand_Consolidated

write.csv(Food_Demand_Consolidated,paste0("parameter_data/Test_Demand_Output",Sys.Date(),".csv"))
