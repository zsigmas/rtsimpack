context('Testing test files are there')
test_that('Test_data is there',{
  d = get_test_data()
  expect_equal(dim(d), c(423040, 4))
  expect_equal(names(d), c('rt', 'hit', 'condition', 'id'))
})
