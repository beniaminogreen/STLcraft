context("find_normals tests")
test_that("find_normals gives correct results",{
	expect_equal(find_normal(c(0,1,0),c(0,0,0),c(0,0,2)),c(-2,0,0))
	expect_equal(find_normal(c(0,1,0),c(0,0,0),c(0,0,-2)),c(2,0,0))
	expect_equal(find_normal(c(0,1,0),c(1,1,0),c(0,1,2)),c(0,-2,0))
	expect_equal(find_normal(c(0,1,0),c(0,0,0),c(0,3,2)),c(-2,0,0))
	expect_equal(find_normal(c(0,-1,0),c(-1,0,0),c(0,0,2)),c(2,2,-1))
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
