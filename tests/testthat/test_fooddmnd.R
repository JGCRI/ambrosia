context('Basic model functionality')


test_that('Food demand model runs and produces the expected result', {
    ps <- 0.1
    pn <- 0.5
    y <- seq(0.1, 10, 0.1)
    expect_silent(rslt <- food.dmnd(ps, pn, y, samp.params))
    expect_equal_to_reference(rslt, 'food_demand_basic.rds', update=FALSE)
})
