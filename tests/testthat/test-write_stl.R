context('write_stl tests')

tmp <- tempfile()
teardown({
  unlink(tmp)
})

test_that("write_stl fails if filename not a string",{
	expect_error(write_stl(volcano, 6))
})
