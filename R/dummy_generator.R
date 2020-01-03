#' Generate dummy datasets for testing
#'
#' The naming makes reference to the function in which they must be used, for example dummy_agg can be used as a parameter for compute_agg

gen_dummy = function(ni=4, np=3, nipi=2){
  set.seed(1)
  file = "/home/zsigmas/Dropbox/CienciaSobreCiencia/rtsimpack/inst/test_data/test_comma.csv"

  dummy_sample = readr::read_csv(file = file, col_types = 'illi') %>%
    dplyr::select(rt_raw, id) %>%
    dplyr::mutate(rt_log = log(rt_raw),
                  rt_inv = 1/rt_raw)

  ntc = get_ntc(dummy_sample)
  ids = get_ids(dummy_sample)

  dummy_shuffle = sample_data(dummy_sample, nipi, np, ids)
  dummy_pp = shuffle_cond(dummy_shuffle, nipi, np, ntc)
  dummy_agg = compute_pp(dummy_pp)
  dummy_test = compute_agg(dummy_agg)

  use_data(dummy_sample, dummy_pp, dummy_agg, dummy_test, overwrite = TRUE)
}


#' Generate a dataset to be used as parameter in sample_data
gen_dummy_sample = function(ni=4, np=3, nipi=2){
  file = "/home/zsigmas/CienciaSobreCiencia/rtsimpack/inst/test_data/test_comma.csv"

  dummy_sample = readr::read_csv(file = file, col_types = 'illi') %>%
    dplyr::select(rt_raw, id) %>%
    dplyr::mutate(rt_log = log(rt_raw),
                  rt_inv = 1/rt_raw)
}

#' Generate a dataset to be used as parameter in shuffle_cond
gen_dummy_shuffle = function(ni=4, np=3, nipi=2){
  d = gen_dummy_sample(ni, np, nipi)
  dummy_shuffle = sample_data(dummy_sample, ni, np, get_ids(d))
}

gen_dummy_pp = function(ni=4, np=3, nipi=2){
  d = gen_dummy_shuffle(ni, np, nipi)
  dummy_pp = shuffle_cond(d, nipi, np, ntc = get_ntc(d))
}

#' Generate a dataset to be used as parameter in compute_agg
gen_dummy_agg = function(ni=4, np=3, nipi=2){
  d = gen_dummy_pp(ni, np, nipi)
  dummy_agg = compute_pp(d)
}

#' Generate a dataset to be used as parameter in compute_test
gen_dummy_test = function(ni=4, np=3, nipi=2){
  d = gen_dummy_pp(ni, np, nipi)
  dummy_test = compute_agg(d)
}

