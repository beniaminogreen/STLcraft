context('Matrix Utilities')
test_that('in_bounds works',{
	expect_equal(in_bounds(volcano,c(0,0)), FALSE)
	expect_equal(in_bounds(volcano,c(-90,0)), FALSE)
	expect_equal(in_bounds(volcano,c(99999,1)), FALSE)
	expect_equal(in_bounds(volcano,c(1,99999)), FALSE)
	expect_equal(in_bounds(volcano,c(1,1)), TRUE)
	expect_equal(in_bounds(volcano,c(50,1)), TRUE)
})
