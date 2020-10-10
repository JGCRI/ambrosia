context('Tests for shiny app')


test_that("shiny is installed",{
expect(requireNamespace('shiny', quietly=TRUE),"Running the ineractive version of the model requires shiny.")
})
