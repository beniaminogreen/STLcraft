context('class STL tests')
setup({data(volcano)})
test_that('SetValidity works for STLs', {
	expect_error(STL(volcano, volcano, 1), 'the size must be specified in 3 dimensions')
	expect_error(STL(volcano, volcano +1), 'Bottom surface may never be above top surface')
	expect_error(STL(volcano, matrix()), 'Top and Bottom surface must have same size')
	expect_error(STL(matrix(c('a','b','c')),matrix(c(1,2,3))), 'Top surface should be a numeric matrix')
	expect_error(STL(matrix(c(1,2,3)),matrix(c('a','b','c'))), 'Bottom surface should be a numeric matrix')
	expect_error(STL(volcano, color = c('a','b')), 'color must be a string')
})
test_that('STL nrow/ncol/dim methods work', {
	stl_1 <- STL(volcano)
	expect_equal(nrow(STL(volcano)), nrow(volcano))
	expect_equal(ncol(STL(volcano)), ncol(volcano))
	expect_equal(dim(STL(volcano)), dim(volcano))
})
test_that('show_stl works',{
	expect_equal(capture.output(STL(volcano)),
c("size       : 1 by 1 by 1 (x,y,z) ", "size : 87 by 61 (nrow, ncol)", "values     : 195, 94 (max, min) "))
})
