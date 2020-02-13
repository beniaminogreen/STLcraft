context("matrix_cleaner tests")
test_that('matrix_cleaner gives right answers', {
	test_1 <- matrix(c(NA,NA,3,3,
			 4, 3, NA, 3,
			 3, 5, 3, 3,
			3, 3, NA, NA) , byrow = 3, nrow = 4)
	test_1_result <- matrix(c(NA,NA,3,3,
			 4, 3, NA, 3,
			 3, 5, 3, 3,
			3, 3, NA, NA) , byrow = 3, nrow = 4)
	test_2 <- matrix(c(NA,NA,NA,3,
			 3, NA, NA, 3,
			 NA, 2, NA, 3,
			3, 3, NA, NA) , byrow = 3, nrow = 4)
	test_2_result <- matrix(c(NA,NA,NA,NA,
			 NA, NA, NA, NA,
			 NA, 2, NA, NA,
			3, 3, NA, NA) , byrow = 3, nrow = 4)
	test_3 <- matrix(c(NA,5,5,NA,
			 NA, NA, 2, NA,
			 NA, 2, 2, NA,
			3, 3, NA, NA) , byrow = 3, nrow = 4)
	test_3_result <- matrix(c(NA,5,5,NA,
			 NA, NA, 2, NA,
			 NA, 2, 2, NA,
			3, 3, NA, NA) , byrow = 3, nrow = 4)
	#
	  expect_true(identical(matrix_cleaner(test_1), test_1_result))
	  expect_true(identical(matrix_cleaner(test_2), test_2_result))
	  expect_true(identical(matrix_cleaner(test_3), test_3_result))
})
