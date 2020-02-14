context('write_stl tests')

tmp <- tempfile()
teardown({
  unlink(tmp)
})

test_that("write_triangle works",{
	write_file <- file(tmp,open="w")
	vert_1 <- c(0,0,0)
	vert_2 <- c(0,1,0)
	vert_3 <- c(0,0,1)
	write_triangle(vert_1, vert_2, vert_3, write_file)
	expect_true(all.equal(readLines(tmp), readLines("./test_files/triangle_test_output.txt")))
	close(write_file)
})
test_that("write_stl fails if filename not a string",{
	expect_error(write_stl(volcano, 6))
})
