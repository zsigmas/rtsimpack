#' Adds shuffled condition
#'
#' This function add a condition variable that contains random condition labels to a dataframe.
#'
#' The condition labels will be c('A', 'B'). As it is used in a simulation environment this labels are arbitrary.
#'
#' It assumes:
#' @details
#' \itemize{
#'   \item The same number of participants across iterations
#'   \item The same number of trials across all participants
#'   \item The same number of trials per condition
#'   \item That the dataframe is sorted by iteration and participant (It would be enough for each participant to be in consecutive rows)
#'   \item An even number of trials in the whole participant. (Must be if it has two conditions and the same number of trials per condition)#'
#' }
#'
#' @param d a dataset to which a shuffled condition variable will be added
#' @param ni the number of iterations included in the dataframe
#' @param np the number of participants included in the dataframe
#' @param ntc number of trials per condition
#'
#' @examples
#'  shuffle_cond(d, ni, np, ntc)
#'
#' @export

shuffle_cond = function(d, ni, np, ntc){
  a=1
  d[['condition']] = unlist(lapply(1:np*ni, function(x){sample(rep(c('A', 'B'), ntc))}))
  return(d)
}

#' Compute preprocessing pipelines
#'
#' This function computes the designed preprocessing pipelines described in the manuscript and adds them as new variables
#'
#' The dataframe must contain the following variables:
#' \describe{
#'   \item{iter}{# of the iteration}
#'   \item{id}{id of the participant}
#'   \item{rt_raw}{Reaction time}
#'   \item{rt_log}{Log-transform of the reaction time}
#'   \item{rt_inv}{Inverse of the reaction time}
#' }
#'
#' @param d a dataframe in which we will compute the different pipelines
#'
#'
#' @export

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

#' Compute aggregate measures
#'
#' This function computes the mean and median values of all the variables in a dataframe grouped by the variable
#' \emph{iter}, \emph{id} and \emph{condition}
#'
#' @param d a dataframe in which we will compute the aggregate measures
#'
#' @export

compute_agg = function(d){compute_agg_dt(d)}

#' Compute aggregate measures using dplyr
#' @export
compute_agg_dp = function(d){
  na_mean = purrr::partial(mean, na.rm=TRUE)
  na_median = purrr::partial(median, na.rm=TRUE)
  d = d %>% dplyr::group_by(iter, id, condition) %>%
    dplyr::summarise_all(.funs=list('mean'=na_mean, 'median'=na_median))
}

#' Helper function to compute_agg_dt
#' @export
compute_mean_median = function(x) {list(mean = mean(x, na.rm=TRUE), median = median(x, na.rm=TRUE))}

#' Compute aggregate measures using data.table
#' Tidy can manage data.tables so no need to convert back
#' @export
compute_agg_dt = function(d){
  d = data.table::as.data.table(d)
  d = d[, as.list(unlist(lapply(.SD, compute_mean_median))), by = c('iter', 'id', 'condition')]
  setorder(d, iter, id, condition)
}

#' Compute t-test
#'
#' This function computes the t-test values of all the variables in a dataframe grouped by the variables
#' \emph{iter}
#'
#' It computes the t-test by calculating the difference between condition per participant and then computing a one-sample t-test.
#'
#' It assumes that \emph{d} is condition ordered within the participant A first row and B second row. Albeit it doesn't matter due to random allocation of conditions
#' it is important to be aware so it is not used to calculate real t-test or if used this should be taken into consideration.
#'
#' A better approach not implemented here would be to move condition to play with some separate gather, but it is not worth it here at the moment.
#'
#' @param d a dataframe in which we will compute the tests
#' @export

compute_test = function(d){compute_test_dt(d)}

#' Compute aggregate measures using dplyr
#' @export
compute_test_dp  = function(d){
  np = (nrow(d)/length(unique(d[['iter']])))/2 # Number of participants, maybe pass as parameter
  t_test_part = purrr::partial(t_test, np=np)
  x = d %>% dplyr::group_by(iter, id) %>%
    dplyr::summarise_if(is.numeric, .funs = list(~(.[1]-.[2]))) %>%
    dplyr::select(-id) %>%
    dplyr::group_by(iter) %>%
    dplyr::summarise_all(t_test_part)
}

#' Compute aggregate measures using data.table
#' Tidy can manage data.tables so no need to convert back
#' @export
compute_test_dt  = function(d){
  d = rtsimpack::dummy_test
  d = data.table::as.data.table(d) # Not needed as when using data.table we should use all the functions but just in case. It is very cheap
  np = length(unique(d[['id']])) # Trusting that replace_id is TRUE in sample_data function
  d = d[, condition:=NULL]
  d = d[, as.list(unlist(lapply(.SD, function(x){x[1]-x[2]}))), by = c('iter', 'id')]
  d = d[, id:=NULL]
  d = d[, lapply(.SD, function(x){t_test(x, np)}), by = c('iter')]
}

#' t-test lite version
#'
#' It computes a one sample t-test but \emph{quicker} than the t.test function because it does less things.
#'
#' @param x vector of values
#' @param np number of observations (df=np-1)
#' @export

t_test = function(x, np){
  s = sd(x)
  m = mean(x)
  t = m/(s/sqrt(np))
  p = 2 * pt(-abs(t), df=np-1)
  return(p)
}

#' Sample data
#'
#' This function generates \emph{ni} samples of \emph{np} participants in each sample.
#' To avoid the case in which the same participant is sampled more than once in the same sample, and then all repetitions will be
#' grouped together when aggregated, the ids of the participants are replaced by numbers from 1 to np in each of the samples if
#' the \emph{replace_id} flag is set to true.
#'
#' We give the option of including several samples (i.e. iterations) in a single dataframe to speed up the computations to compute several
#' iterations at once.
#'
#' @param d full dataset
#' @param ni the number of iterations we will perform in one chunk
#' @param np number of participants to sample
#' @param ids a vector with the ids of each of participant
#' @param replace_id flag that indicates if participants ids will be replaced (1:np) or not. When running simulations it must always be set to TRUE!
#'
#'
#' @export

sample_data = function(d, ni, np, ids, replace_id=TRUE){
  s = data.frame(iter = rep(1:ni, times=1, each=np),
                 id = sample(ids, np*ni, replace = TRUE),
                 nid = rep(1:np, times=ni))
  s = dplyr::inner_join(s, d, by='id')
  if(replace_id){
    s = dplyr::mutate(s, id=nid)
  }
  s = dplyr::select(s, -nid)
  return(s)
}


#' Computes one iteration of the simulation
#'
#' @param d full dataset
#' @param ni the number of iterations to be computed at once
#' @param np the number of participants to be used in the iteration
#' @param ntc number of trials per condition
#' @param ids unique ids of each participant in the dataset
#'
#' @export

compute_one_iteration = function(d, ni, np, ntc, ids){
  d = sample_data(d, ni, np, ids)
  d = shuffle_cond(d, ni, np, ntc)
  d = compute_pp(d)
  d = compute_agg(d)
  d = compute_test(d)
  return(d)
}

#' Get unique ids of the dataset
#'
#' It will return a vector with the unique participant id in the dataset
#'
#' @param d a dataset
#'
#' @export

get_ids = function(d){
  return(unique(d[['id']]))
}

#' Get the number of trials per condition
#'
#' This function returns the number of trial per condition
#'
#' It also asserts that the number of trial is equal for all participants, and that the number of trials is even for all participants.
#'
#' @param d a dataset
#'
#' @export

get_ntc = function(d){
  if(!('iter' %in% names(d))){
    d = dplyr::mutate(d, iter = 1)
  }

  ntv = d %>% dplyr::group_by(iter, id) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull(n)

  assertthat::assert_that(all(ntv==ntv[1]), msg="Participants have different amount of trials")
  assertthat::assert_that((ntv[1]%%2)==0, msg="Number of trials is not even")

  return(ntv[1]/2)
}

#' Get file and launch iterations
#'
#' The main function to be called when invoking the simulation.
#'
#' The difference between \emph{ni} and \emph{nipi} is that \emph{ni} is the total number of iterations to be run during the simulation,
#' the relevant value, while \emph{nipi} has computational purposes as we will run iterations in batches of size \emph{nipi}
#' this way we speed computations a bit. If confusing it can be set to 1, slowing things down, but only one iteration will be run per
#' loop making the process easier to understand.
#'
#' @param file path to the file containing the data over which we will run the simulations
#' @param ni number of iterations that will be performed
#' @param np number of participants to be included in each of the simulation iterations
#' @param nipi number of iterations per iteration
#'
#' @export

run_simulation = function(file, ni, np, nipi){

  d = readr::read_csv(file = file, col_types = 'illi') %>%
    dplyr::select(rt_raw, id) %>%
    dplyr::mutate(rt_log = log(rt_raw),
                  rt_inv = 1/rt_raw)

  ntc = get_ntc(d)
  ids = get_ids(d)

  r = pbapply::pblapply(1:round(ni/nipi),
                        function(x, d, ni, np, ntc, ids){
                          oi = compute_one_iteration(d, ni, np, ntc, ids)
                          return(oi)
                        },d=d, ni=nipi, np=np, ntc=ntc, ids=ids)
  r = dplyr::bind_rows(r)
  return(r)
}
