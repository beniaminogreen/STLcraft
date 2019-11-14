context('Geometry Calculations')
test_that('Normalize Vector Gives Correct Results',{
	expect_equal(normalize_vector(c(3,4)),c(.6,.8))
	expect_equal(normalize_vector(c(5,4)),c(5/sqrt(41),4/sqrt(41)))
	expect_equal(normalize_vector(c(10,10,10)),c(1/sqrt(3),1/sqrt(3),1/sqrt(3)))
})
test_that('Normailze Vector Fails When Not Numeric Input',{
	expect_error(normalize_vector(c('1', 'A')),'vector not numeric')
		  })

test_that("xprod gives correct result",{
	expect_equal(xprod(3:5, 4:6), c(-1,2,-1))
	expect_equal(xprod(2:4, c(-1,3,4)), c(0,-12,9))
	expect_equal(xprod(c(5,4,4), c(-1,3,4)), c(4,-24,19))
	expect_equal(xprod(c(-5,-4,4), c(-1,3,4)), c(-28,16,-19))
})

test_that('Xprod rejects different length Vectors',{
	expect_error(xprod(3:5,3:6),"Vectors Must be of Same Length")
})
test_that("find_normals gives correct results",{
	expect_equal(find_normal(c(0,1,0),c(0,0,0),c(0,0,2)),c(-1,0,0))
	expect_equal(find_normal(c(0,1,0),c(0,0,0),c(0,0,-2)),c(1,0,0))
	expect_equal(find_normal(c(0,1,0),c(1,1,0),c(0,1,2)),c(0,-1,0))
	expect_equal(find_normal(c(0,1,0),c(0,0,0),c(0,3,2)),c(-1,0,0))
	expect_equal(find_normal(c(0,-1,0),c(-1,0,0),c(0,0,2)),c(2/3,2/3,-1/3))
})
test_that("find_normals throws error if vectors of not length 3",{
	expect_error(find_normal(c(0,-1),c(-1,0,0),c(0,0,2)),'vertexes must all have 3 points')
	expect_error(find_normal(c(0,-1,0),c(0,0),c(0,0,2)),'vertexes must all have 3 points')
	expect_error(find_normal(c(0,-1,0),c(-1,0,0),c(0,2)),'vertexes must all have 3 points')
})
test_that('find_normals will not work with non-numeric arguments',{
	expect_error(find_normal(c("a","b","c"),c("a","b","c"),c("a","b","c")),'all arguments must be numeric vectors')
	expect_error(find_normal(c(0,-1,0),c(0,0,"a"),c(0,0,2)),'all arguments must be numeric vectors')
	expect_error(find_normal(c("0",-1,0),c(0,0,0),c(0,0,2)),'all arguments must be numeric vectors')
	expect_error(find_normal(c(0,-1,0),c(0,0,0),c(0,"a",2)),'all arguments must be numeric vectors')
})
