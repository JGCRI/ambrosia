context('Tests for shiny app, figures')
library(dplyr)

test_that("shiny is installed",{
expect(requireNamespace('shiny', quietly=TRUE),"Running the ineractive version of the model requires shiny.")
})

test_that(" main demand plot function results in plots",{


    ps <- 0.1
    pn <- 0.5
    y <- seq(0.1, 10, 0.1)
    samp.params <- vec2param(c(1.28,1.14,-0.19,0.21,-0.33,0.5,0.1,16,5.06,100,20))
    expect_silent(rslt <- food.dmnd(ps, pn, y, samp.params))
    rslt$Y <- y

    expect_error(make.demand.plot(rslt,xdata=y,"GDP per capita in thous USD"),NA,info=("Failure to plot demand data."))

})

test_that("by year plot function results in plots",{

    parameter_data <- read.csv("test_outputs/parameter_data.csv")
    params <- vec2param(c(parameter_data$params_vector.par))

    raw_data <- read.csv("test_outputs/Training_Data.csv") %>% dplyr::filter(year %in% (2013:2015))

    (food_demand <- food.dmnd.byyear(raw_data,params = params))

    food_demand$rgn <- raw_data$iso
    food_demand$income <- raw_data$gdp_pcap_thous/1000

    expect_error(make.byyear.plot(food_demand,pltrgn=c("afg","usa")),NA,info=("Failure to plot demand data by year."))

})


