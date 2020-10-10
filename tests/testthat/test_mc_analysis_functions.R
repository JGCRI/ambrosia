context('Test Monte Carlo exploration functions')

test_that("Monte Carlo density plots are generated",{

    expect_silent(mc_example <- read.mc.data("test_outputs/mc_data_example.dat",varnames = namemc(nparam = 9)))


    expect_error(mcparam.density(mc_example),NA,info=("Failure to plot densities from MC data."))

})

test_that("Monte Carlo maximum density value is valid",{

    expect_silent(mc_example <- read.mc.data("test_outputs/mc_data_example.dat",varnames = namemc(nparam = 9)))

    mcparam.ML(mc_example) -> ml_parameters

    expect_equal(length(ml_parameters),9,tolerance=0.001,info=("wrong number of parameters are generated from MC data."))

    func_MC <- mc.setup("test_outputs/food-dmnd-price-allrgn.csv")

    #Sample data is created with 9 parameters. Adding in the scaling parameters below.
    probability_density <- func_MC(c(ml_parameters,100,20))

    expect_equal(probability_density,-195,tolerance=0.01,info=("Maximum probability density generated with sample MC data is invalid."))
})


test_that("Tails from MC distribution can be clipped",{

    expect_silent(mc_example <- read.mc.data("test_outputs/mc_data_example.dat",varnames = namemc(nparam = 9)))

    expect_silent(tmp <- mcparam.clip.tails(mc_example))
})
