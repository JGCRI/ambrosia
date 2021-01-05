context('Test Monte Carlo exploration functions')

test_that("Monte Carlo density plots are generated",{

    expect_silent(mc_example <- read.mc.data("test_outputs/mc_data_example.dat",varnames = namemc(nparam = 11)))

    expect_error(mcparam.density(mc_example),NA,info=("Failure to plot densities from MC data."))

})

test_that("Monte Carlo maximum density value is valid",{

    expect_silent(mc_example <- read.mc.data("test_outputs/mc_data_example.dat",varnames = namemc(nparam = 11)))

    mcparam.ML(mc_example) -> ml_parameters

    expect_equal(length(ml_parameters),11,tolerance=0.001,info=("wrong number of parameters are generated from MC data."))

    func_MC <- mc.setup("test_outputs/Processed_Data_for_MC_11_param.csv")

    #Sample data is created with 9 parameters. Adding in the scaling parameters below.
    probability_density <- func_MC(c(ml_parameters))

    expect_equal(probability_density,-962.6254,tolerance=0.01,info=("Maximum probability density generated with sample MC data is invalid."))
})


test_that("Tails from MC distribution can be clipped",{

    expect_silent(mc_example <- read.mc.data("test_outputs/mc_data_example.dat",varnames = namemc(nparam = 11)))

    expect_silent(tmp <- mcparam.clip.tails(mc_example))
})
