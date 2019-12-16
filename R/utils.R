#' Create dummy data frame
get_dummy_df = function(ni, np, ntc, random_seed=1){
  d = expand.grid(iter=1:ni, id=1:np, nt=1:ntc, condition=c('A', 'B'))
  d[['rt_raw']] = abs(rnorm(nrow(d)))
  d[['rt_log']] = log(d[['rt_raw']])
  d[['rt_inv']] = 1/d[['rt_raw']]
  d = dplyr::select(d, -nt)
  d = dplyr::arrange(d, iter, id)
  d = shuffle_cond(d, ni, np, ntc)
  return(d)
}
