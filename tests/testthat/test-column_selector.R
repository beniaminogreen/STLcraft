context("column partition tests")
test_that("case_vpartition works", {
	test <- matrix(c(3,3,3,3,
			 4, 3, 5, 3,
			 3, 5, 3, 3,
			 3, 3, 3, 5) , byrow = 3, nrow = 4)
	test_1_res <- matrix(c("three","three","three","three",
			       "four", "three", "five", "three",
			       "three", "five", "three", "three",
			       "three", "three", "three", "five") , byrow = 3, nrow = 4)
	test_2_res <- matrix(c("blue","blue","blue","blue",
				 "blue", "blue", "blue", "blue",
				 "red", "red", "red", "red",
				 "red", "red", "red", "red") , byrow = 3, nrow = 4)
	test_3_res <- matrix(c("left","left","right","right",
			       "left", "left", "right", "right",
			       "left", "left", "right", "right",
			       "left", "left", "right", "right") , byrow = 3, nrow = 4)
	expect_equal(case_vpartition(test,
				     z == 3 ~ "three",
				     z == 4 ~ "four",
				     z == 5 ~ "five"), test_1_res)
	expect_equal(case_vpartition(test,
				     y <= 2 ~ "blue",
				     TRUE ~ "red"), test_2_res)
	expect_equal(case_vpartition(test,
				     x <= 2 ~ "left",
				     TRUE ~ "right"), test_3_res)
})
