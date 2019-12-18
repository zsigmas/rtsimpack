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

test_that('Getting number of trials',
          {
            same_it_even = expand.grid(id=1:3, iter=1:2, t=1:10)
            same_no_it_even = expand.grid(id=1:3, t=1:10)
            same_it_odd = expand.grid(id=1:3, iter=1:2, t=1:7)
            same_no_it_odd = expand.grid(id=1:3, t=1:7)
            diff_it_even = dplyr::bind_rows(expand.grid(id=1:3, iter=1:2, t=1:10),
                                       expand.grid(id=1:2, iter=1:2, t=1:10))
            diff_no_it_even = dplyr::bind_rows(expand.grid(id=1:3, iter=1:2, t=1:10),
                                            expand.grid(id=1:2, iter=1:2, t=1:10))
            diff_it_odd = dplyr::bind_rows(expand.grid(id=1:3, iter=1:2, t=1:10),
                                            expand.grid(id=1:2, iter=1:2, t=1:7))
            diff_no_it_odd = dplyr::bind_rows(expand.grid(id=1:3, iter=1:2, t=1:10),
                                               expand.grid(id=1:2, iter=1:2, t=1:7))
            expect_equal(get_ntc(same_it_even), 5)
            expect_equal(get_ntc(same_no_it_even), 5)
            expect_error(get_ntc(same_it_odd))
            expect_error(get_ntc(same_no_it_odd))
            expect_error(get_ntc(diff_it_even))
            expect_error(get_ntc(diff_no_it_even))
            expect_error(get_ntc(diff_it_odd))
            expect_error(get_ntc(diff_no_it_odd))
          })

test_that("Sampling provides different participants across iterations and invokations", {
  # Bug provoked
  set.seed(1)
  ni = 2
  np = 10
  d = rtsimpack::dummy_original
  ids = rtsimpack::get_ids(d)
  s1 = rtsimpack::sample_data(d, ni, np, ids, replace_id = FALSE) %>%
    dplyr::select(iter, id) %>% dplyr::distinct()
  s2 = rtsimpack::sample_data(d, ni, np, ids, replace_id = FALSE) %>%
    dplyr::select(iter, id) %>% dplyr::distinct()

  it1 = s1 %>% dplyr::filter(iter==1) %>% dplyr::pull(id)
  it2 = s1 %>% dplyr::filter(iter==2) %>% dplyr::pull(id)

  id1 = s1 %>% dplyr::pull(id)
  id2 = s2 %>% dplyr::pull(id)

  expect_false(all(id1==id2))
  expect_false(all(it1==it2))
})

