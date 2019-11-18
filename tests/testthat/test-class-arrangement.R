context('Arrangement Constructor Methods Work')
test_that('Arrangement Constructor Fails Fast And Loud', {
	large_STL <- STL(volcano)
	small_STL <- STL(matrix(1:9, nrow=3))
	fake_size_STL <- STL(volcano, size = c(1,2,3))
	expect_error(Arrangement(small_STL, large_STL, 'All STLs must have the same dimensions'))
	expect_error(Arrangement(fake_size_STL, large_STL, 'All STLs must have the same size'))
	expect_error(Arrangement(matrix(1:9, nrow =3), large_STL, 'All STLS must be of class STL'))
})
