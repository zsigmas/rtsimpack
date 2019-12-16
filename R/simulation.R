#' Shuffle condition labels by iteration and participant
#'
#' Recieves a dataframe with at least the columns id, iter and condition
#'
#' We assume that:
#' 1. We have the same amount of trials per participant
#' 2. The same number of participants per iteration
#' 3. The same number of trials per condition
#' 4. That the dataframe is ordered first by iteration and participant id
#' 5. Even number of trials in total so they can be split in half
#'
#' 6. NP IS NUMBER OF TRIALS PER CONDITION!!!
#'
#' We also assume that the condition label means nothing so we will assign As or Bs at our discretion
#'

shuffle_cond = function(d, ni, np, ntc){
  d[['condition']] = unlist(lapply(1:np*ni, function(x){sample(rep(c('A', 'B'), ntc))}))
  return(d)
}

#' Compute pp preprocessing pipelines
#'
#'

compute_pp = function(d){
  d = d %>% dplyr::group_by(iter, id) %>%
    dplyr::mutate(filter_avg = mean(rt_raw), filter_stdev = sd(rt_raw),
                  filter_2.0 = ifelse(rt_raw <= 2.0 * filter_stdev + filter_avg &
                                      rt_raw >= -2.0 * filter_stdev + filter_avg,
                                    1, NA),
                  filter_2.5 = ifelse(rt_raw <= 2.5 * filter_stdev + filter_avg &
                                        rt_raw >= -2.5 * filter_stdev + filter_avg,
                                      1, NA),
                  filter_3.0 = ifelse(rt_raw <= 3.0 * filter_stdev + filter_avg &
                                        rt_raw >= -3.0 * filter_stdev + filter_avg,
                                      1, NA),
                  rt_raw2.0 = rt_raw*filter_2.0,
                  rt_raw2.5 = rt_raw*filter_2.5,
                  rt_raw3.0 = rt_raw*filter_3.0,
                  rt_log2.0 = rt_log*filter_2.0,
                  rt_log2.5 = rt_log*filter_2.5,
                  rt_log3.0 = rt_log*filter_3.0,
                  rt_inv2.0 = rt_inv*filter_2.0,
                  rt_inv2.5 = rt_inv*filter_2.5,
                  rt_inv3.0 = rt_inv*filter_3.0) %>%
    dplyr::select(-dplyr::starts_with('filter_'))
}

compute_agg = function(d){
  na_mean = purrr::partial(mean, na.rm=TRUE)
  na_median = purrr::partial(median, na.rm=TRUE)
  d = d %>% dplyr::group_by(iter, id, condition) %>%
    dplyr::summarise_all(.funs=list('mean'=na_mean, 'median'=na_median))
}

compute_test  = function(d){
  np = nrow(d)/length(unique(d[['iter']])) # Number of participants, maybe pass as parameter
  t_test_part = purrr::partial(t_test, np=np)
  x = d %>% group_by(iter, id) %>%
    summarise_if(is.numeric, .funs = list(~(.[1]-.[2]))) %>%
    group_by(iter) %>% summarise_all(t_test_part)
}

t_test = function(x, np){
  s = sd(x)
  m = mean(x)
  t = m/(s/sqrt(np))
  p = 2 * pt(-abs(t), df=np-1)
  return(p)
}


#' One iteration
#'
#'
#'

compute_one_iteration = function(d){
  d = shuffle_cond(d)
  d = compute_pp(d)
  d = compute_agg(d)
  d = compute_test(d)
  return(d)
}

sample_data = function(x, d, np, ids){
  s =
}

get_ids = function(d){
  return(unique(d[['id']]))
}

#' Get file sample and launch iterations
run_simulation = function(file, ni, np, nipi){

  d = readr::read_csv(file = file, col_types = 'illi') %>%
    dplyr::select(rt_raw, id)

  ids = get_ids(d)

  s = dplyr::bind_rows(pbapply::pblapply(1:nipi, function(x, d, np){

  }))

  #Run iteration

  #Save iteration

}
