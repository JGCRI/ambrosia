## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
#load libraries
library(ambrosia)
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(knitr))

## ------------------------------------------------------------------------

c(1.28,1.14,-0.19,0.21,-0.33,0.5,0.1,16,5.06,100,20)->original_param_vector

parameter_names<-c('A_s', 'A_n', 'xi_ss', 'xi_cross', 'xi_nn', 'nu1_n',
                     'lambda_s', 'k_s', 'Pm', 'psscl','pnscl')
parameter_data<-data.frame(parameter_names,original_param_vector)

tmp_param<-vec2param(original_param_vector)

kable(parameter_data ,col.names = c("parameter_name","value"),format = "pandoc")


## ------------------------------------------------------------------------
#Get a sample data set
Test_Data <- data.frame(Y=seq(0.1,30, by=0.1))

#Add sample values of Ps and Pn
Test_Data %>% mutate(Ps=0.1,Pn=0.2)->Test_Data

#Add some sample parameters
sample_parameters <- c(1.28,1.14,-0.19,0.21,-0.33,0.5,0.1,16,5.06,100,20)

#Calculate food demand
Food_Demand<-food.dmnd(Test_Data$Ps,Test_Data$Pn,Test_Data$Y,params=vec2param(sample_parameters))

