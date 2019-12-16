context('Simulation tests')

test_that("Simulation Shuffling", {
  set.seed(1)
  ni = 2
  np = 4
  ntc = 10
  d = get_dummy_df(ni, np, ntc)
  ds = shuffle_cond(d, ni, np, ntc)
  nts = d %>% dplyr::group_by(iter, id, condition) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  expect_false(all(d[['condition']]==ds[['condition']]))
  expect_true(all(nts==ntc))
})

test_that("T test works",{
  np = 100
  x = rnorm(np)
  p_base = t.test(x)$p.value
  p_ours = t_test(x, np)
  expect_equal(p_base, p_ours, tolerance=1e-3)

  x = rnorm(np)
  p_base = t.test(x)$p.value
  p_ours = t_test(x, np)
  expect_equal(p_base, p_ours, tolerance=1e-3)

  x = rnorm(np)
  p_base = t.test(x)$p.value
  p_ours = t_test(x, np)
  expect_equal(p_base, p_ours, tolerance=1e-3)

  x = rnorm(np)
  p_base = t.test(x)$p.value
  p_ours = t_test(x, np)
  expect_equal(p_base, p_ours, tolerance=1e-3)
})
