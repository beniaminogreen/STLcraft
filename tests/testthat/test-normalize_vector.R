context('Normalize Vector Tests')
test_that('Normalize Vector Gives Correct Results',{
	expect_equal(normalize_vector(c(3,4)),c(.6,.8))
	expect_equal(normalize_vector(c(5,4)),c(5/sqrt(41),4/sqrt(41)))
	expect_equal(normalize_vector(c(10,10,10)),c(1/sqrt(3),1/sqrt(3),1/sqrt(3)))
})
test_that('Normailze Vector Fails When Not Numeric Input',{
	expect_error(normalize_vector(c('1', 'A')),'vector not numeric')
		  })
