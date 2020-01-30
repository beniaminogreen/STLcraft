context('neighbor counter tests')
test_that('n_neighbiors gives correct answers',{
	expect_equal(n_neighbors(1,1,volcano), 3)
	expect_equal(n_neighbors(10,10,volcano), 8)
})
test_that('neighbor_counter gives correct answers',{
	test_input <- matrix(c(NA,NA,NA,12,
			NA,NA,12,12,
			NA,12,NA,NA),
			nrow=3, byrow=TRUE)
	expected_output <- matrix(c(0,0,0,2,
				0,0,3,2,
				0,1,0,0),
			nrow=3, byrow=TRUE)
	expect_identical(neighbor_counter(test_input), expected_output)
})
