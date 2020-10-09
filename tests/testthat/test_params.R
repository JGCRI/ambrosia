context('Test validity of new parameters and data')

test_that('New food demand parameters (vector) are valid', {
    parameter_data <- read.csv("test_outputs/parameter_data.csv")
    params <- (c(parameter_data$params_vector.par))

    expect(validate.params(params),"New vector of parameters calculated are invalid.")
})


test_that('Optimization when fitting parameters is accurate', {
    func_MC <- mc.setup("test_outputs/Processed_Data_for_MC.csv")
    original_param_vector <- c(c(1.28,1.14,-0.19,0.21,-0.33,0.5,0.1,16,5.06,100,20))

    Old_prob_density <- func_MC(original_param_vector)

    parameter_data <- read.csv("test_outputs/parameter_data.csv")
    params <- (c(parameter_data$params_vector.par))

    New_prob_density <- func_MC(params)

    expect(New_prob_density > Old_prob_density,"Optimization when fitting model parameters was not successful.
           New probability density is lower than old probability density.")
})

test_that('Check that clustering algorithm is generating enough clusters',{

    Data <- read.csv("test_outputs/Training_Data.csv") %>% filter(year %in% c(2000,2010))

    create_dataset_for_parameter_fit(data=Data,min_clusters = 20,outdir = tempdir())->tmp

    expect(length(unique(tmp$clusterID))>20,"Clustering algorithm is generating skewed results. Parameters calculated may not be valid.")
})




test_that('Clusters created are valid ', {
    MC_data <- read.csv("test_outputs/Processed_Data_for_MC.csv")

    unique_clusters <- unique(MC_data$clusterID)

    min_clusters <- 20

    expect(length(unique_clusters) >= min_clusters,"Clusters created in the processed data are under 20. sigma^2 values generated may be unreliable.")
})


test_that('MC function can be set up correctly with old data', {
    old_data <- read.csv("test_outputs/food-dmnd-price-allrgn.csv")

    expect_silent(func_MC <- mc.setup("test_outputs/food-dmnd-price-allrgn.csv"))
    original_param_vector <- c(c(1.28,1.14,-0.19,0.21,-0.33,0.5,0.1,16,5.06,100,20))

    expect_silent(original_probability_density <- func_MC(original_param_vector))

    expect_equal(original_probability_density,-195,tolerance=0.01,info= paste("Unable to generate original probability density from Edmonds et al."))
})
