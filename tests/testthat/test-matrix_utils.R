context('Matrix Utilities')
test_that('in_bounds works',{
	expect_equal(in_bounds(volcano,c(0,0)), FALSE)
	expect_equal(in_bounds(volcano,c(-90,0)), FALSE)
	expect_equal(in_bounds(volcano,c(99999,1)), FALSE)
	expect_equal(in_bounds(volcano,c(1,99999)), FALSE)
	expect_equal(in_bounds(volcano,c(1,1)), TRUE)
	expect_equal(in_bounds(volcano,c(50,1)), TRUE)
})
test_that('check_valid works',{
	test_matrix <- matrix(c(NA,NA,NA,3,
			 3, NA, NA, 3,
			 NA, 2, NA, 3,
			3, 3, NA, NA) , byrow = 3, nrow = 4)
	expect_equal(check_valid(test_matrix,4,2),TRUE)
	expect_equal(check_valid(test_matrix,3,2),TRUE)
	expect_equal(check_valid(test_matrix,2,4),FALSE)
	expect_equal(check_valid(test_matrix,1,2),FALSE)
	expect_equal(check_valid(test_matrix,2,2),FALSE)
})
