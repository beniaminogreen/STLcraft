context('class STL tests')
test_that('SetValidity works for STLs', {
	expect_error(STL(volcano, volcano, 1), 'the size must be specified in 3 dimensions')
	expect_error(STL(volcano, matrix()), 'Top and Bottom surface must have same size')
	expect_error(STL(matrix(c('a','b','c')),matrix(c(1,2,3))), 'Top surface should be a numeric matrix')
	expect_error(STL(matrix(c(1,2,3)),matrix(c('a','b','c'))), 'Bottom surface should be a numeric matrix')
	expect_error(STL(volcano, color = c('a','b')), 'color must be a string')
})

