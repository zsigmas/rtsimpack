#' Format the simulated data csv's originally generated using python
#'
#' It changes names and selects and renames the relevant columns, id and rt_raw
#'
#' @export

format_synth = function(in_file, out_file=NULL){
  d = readr::read_delim(in_file,
                        col_names = c('rt_raw', 'id'),
                        col_types = 'd--d',
                        delim = ';')

  d = dplyr::mutate(d, id = as.integer(id+1),
                    rt_raw = as.integer(rt_raw))

  if(!is.null(out_file)){
    readr::write_excel_csv(d, out_file)
  }
  return(d)
}


#' Format flexicon data
#'
#' It changes names and selects and renames the relevant columns, id and rt_raw
#'
#' @export

format_flexicon = function(in_file, out_file=NULL){
  load(in_file)
  d = flp %>% dplyr::select( rt_raw = rt, id = participant)
  if(!is.null(out_file)){
    readr::write_excel_csv(d, out_file)
  }
  return(d)
}


#' Format Stroop data
#'
#' It changes names and selects and renames the relevant columns, id and rt_raw
#'
#' @export

format_stroop = function(in_file, out_file=NULL){
  d = readr::read_csv(in_file, col_names = c('rt_raw', 'id'),
                      col_types = '----------i-i--',
                      skip = 1) # Skip header line, we use our own names
  if(!is.null(out_file)){
    readr::write_excel_csv(d, out_file)
  }
  return(d)
}

#' Clean real datasets
#'
#' Removes selected percentage of top and low trials and equalizes the number of trials by resampling trials until the desired number is achieved
#'
#' @param dirty_d a dirty dataset (Stroop of Flexicon)
#' @param target_nt we will resample trials from the participant until we reach the desired number of trials
#' @param trim_top percentage of trials to be trimmed from top of the dataset (calculated using all participants)
#' @param trim_bottom percentage of trials to be trimmed from the bottom of the dataset (calculated using all participants)
#'

clean_real = function(dirty_d, target_nt, min_nt, trim_top, trim_bottom){

  # Remove percentiles
  top_cut = quantile(dirty_d[['rt_raw']], trim_top)
  bot_cut = quantile(dirty_d[['rt_raw']], trim_bottom)
  clean_d = dplyr::filter(dirty_d, bot_cut<rt_raw & rt_raw<top_cut)
  logging::logdebug(paste('trimming upper: ', paste0(trim_top*100, '%')),logger='clean_real')
  logging::logdebug(paste('top_cut', top_cut),logger='clean_real')
  logging::logdebug(paste('trimming lower: ', paste0(trim_bottom*100, '%')),logger='clean_real')
  logging::logdebug(paste('bot_cut', bot_cut),logger='clean_real')
  logging::logdebug(paste('rows after trimming', nrow(clean_d)),logger='clean_real')

  # Remove participant that have lost too many trials
  accepted_ids = clean_d %>% dplyr::group_by(id) %>%
    dplyr::summarise(nt=dplyr::n()) %>%
    dplyr::filter(nt>=min_nt) %>%
    dplyr::pull(id)
  clean_d = clean_d %>% dplyr::filter(id %in% accepted_ids)
  logging::logdebug(paste('Initial ids', length(unique(dirty_d[['id']]))),logger='clean_real')
  logging::logdebug(paste('Remaining ids', length(accepted_ids)),logger='clean_real')

  # Add the trials until target_nt is met
  low_nt_ids = clean_d %>% dplyr::group_by(id) %>%
    dplyr::summarise(nt=target_nt-dplyr::n()) %>%
    dplyr::filter(nt>0)

  resample_id = function(d, this_id, this_nt){
    t = d %>% dplyr::filter(id==this_id) %>%
      dplyr::sample_n(this_nt, replace=TRUE)
  }

  added_rows = dplyr::bind_rows(mapply(resample_id,
                                       this_id=low_nt_ids[['id']],
                                       this_nt=low_nt_ids[['nt']],
                                       MoreArgs = list(d=clean_d),
                                       SIMPLIFY = FALSE),
                                clean_d)

}
