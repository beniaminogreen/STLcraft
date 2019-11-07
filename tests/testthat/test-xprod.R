context("xprod tests")
test_that("xprod gives correct result",{
	expect_equal(xprod(3:5, 4:6), c(-1,2,-1))
	expect_equal(xprod(2:4, c(-1,3,4)), c(0,-12,9))
	expect_equal(xprod(c(5,4,4), c(-1,3,4)), c(4,-24,19))
	expect_equal(xprod(c(-5,-4,4), c(-1,3,4)), c(-28,16,-19))
})

test_that('Xprod rejects different length Vectors',{
	expect_error(xprod(3:5,3:6),"Vectors Must be of Same Length")
})
