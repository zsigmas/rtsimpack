#' Check that all participants have the same number of trials
#'

check_ntrial = function(d){
  if(!('iter' %in% names(d))){
    d = dplyr::mutate(d, iter = 1)
  }
  nt = d %>% dplyr::group_by(iter, id) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull(n)
  return(all(nt==nt[1]))
}
