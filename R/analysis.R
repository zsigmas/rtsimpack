#' This function creates a summary of the false positive rate in each of the single preprocessing pipelines
#'
#' Basically it needs a dataframe with p values and calculates the proportion of p values below the specified
#' alpha level for each column
#'
#' @param d a dataframe containing p values
#' @param alpha the specified alpha level
#'

summarise_fp_single = function(d, alpha=.05){

  d = dplyr::group_by(d, dataset)

  r_ni = dplyr::summarise(d, ni = n())
  r_p = d %>% dplyr::summarise_all(list(fp = function(x){mean(x<=alpha)}))

  r = dplyr::inner_join(r_ni, r_p, by=c('dataset'))

}



#' Tidy and rename the dataset computed during the simulations
#'
#' The dataset returned by the simulation functions is very ugly for printing and showing in a report
#' This functions tidy it and rename it in a prettier way as well and in wide format.
#'
#' @param

tide_rename = function(d){

  d = d %>% dplyr::mutate(iter=1:nrow(d)) %>% # Iter number is forced, because of chunk system used in drake only assumption here is that each row is an iteration
    tidyr::pivot_longer(cols = tidyr::starts_with('rt_'), names_to = 'pp', values_to = 'fp') %>%
    dplyr::mutate(pp = stringr::str_remove(pp, pattern = 'rt_'),
                  pp = stringr::str_remove(pp, pattern = '_fp'),
                  pp = stringr::str_replace(pp, pattern = '([a-z]{3})', replacement = '\\1_'),
                  pp = stringr::str_replace(pp, pattern = '.(me)', replacement = '_\\1'),
                  dataset = stringr::str_replace(dataset, pattern = '^s_', replacement = 'Synth_'),
                  dataset = stringr::str_replace(dataset, pattern = '^flexicon', replacement = 'Flexicon'),
                  dataset = stringr::str_replace(dataset, pattern = '^stroop', replacement = 'Stroop'),

    ) %>%
    tidyr::pivot_wider(names_from = 'pp', values_from = 'fp') %>%
    dplyr::select(-iter)
}

#' This function creates a summary of the false positive rate in each of the groups of used in the paper preprocessing pipelines
#'
#' It works in a similar way to summarise_fp_single, but in this case instead of calculating the false positive rate for
#' each pipeline it calculates it for groups. If any of the preprocessing pipelines considered in the group is below alpha
#' then it is considered a false positive.
#'
#' Groups are hard coded in the function as they are unlikely to change
#'
#' @param d a dataset containing p-values
#' @param alpha the desired alpha level
#'

  summarise_fp_group = function(d, alpha=.05){
  d %>%
    dplyr::mutate_at(dplyr::vars(-tidyr::matches('dataset')), list(~(.<alpha))) %>%
    dplyr::transmute(
      dataset = dataset,

      `Raw/Log/Inv - NO SD - Median` =
        raw_0.0_median | log_0.0_median | inv_0.0_median,

      `Raw/Log/Inv - NO SD - Mean` =
        raw_0.0_mean | log_0.0_mean | inv_0.0_mean,

      `Raw - NO SD - Mean/Median` =
        raw_0.0_mean | log_0.0_median,

      `Raw/Log/Inv - NO SD - Mean/Median` =
        raw_0.0_median | log_0.0_median | inv_0.0_median |
        raw_0.0_mean | log_0.0_mean | inv_0.0_mean,

      `Raw - NO/2.0/2.5/3.0 SD - Mean` =
        raw_0.0_mean | raw_2.0_mean |raw_2.5_mean | raw_3.0_mean,

      `All` =
        raw_0.0_mean | raw_0.0_median |
        raw_2.0_mean | raw_2.0_median |
        raw_2.5_mean | raw_2.5_median |
        raw_3.0_mean | raw_3.0_median |
        log_0.0_mean | log_0.0_median |
        log_2.0_mean | log_2.0_median |
        log_2.5_mean | log_2.5_median |
        log_3.0_mean | log_3.0_median |
        inv_0.0_mean | inv_0.0_median |
        inv_2.0_mean | inv_2.0_median |
        inv_2.5_mean | inv_2.5_median |
        inv_3.0_mean | inv_3.0_median
    ) %>%
    dplyr::group_by(dataset) %>%
    dplyr::summarise_all(mean) %>%
    tidyr::pivot_longer(-dataset, names_to = 'pp', values_to = 'fp') %>%
    tidyr::pivot_wider(names_from = 'dataset', values_from = 'fp')
}


#' False positive Summary Bar graph
#'
#' This function generates the figure#1 of the manuscript

gen_bar_plot = function(fp_single, fp_group){
  ds = fp_single %>%
    dplyr::select(-ni) %>%
    tidyr::pivot_longer(-dataset, names_to = 'pp', values_to='fp') %>%
    dplyr::rename(Dataset=dataset) %>%
    dplyr::group_by(Dataset) %>%
    dplyr::summarise(`Single PP` = mean(fp))

  dg = fp_group %>%
    tidyr::pivot_longer(-pp, names_to = 'Dataset', values_to = 'fp') %>%
    tidyr::pivot_wider(names_from = 'pp', values_from = 'fp')

  d = dplyr::inner_join(dg, ds, by=c('Dataset')) %>%
    tidyr::pivot_longer(-Dataset, names_to = 'Set', values_to = 'Proportion') %>%
    dplyr::mutate(Dataset = dplyr::if_else(stringr::str_starts(Dataset, 'Synth'),
                                           'Simulated',
                                           Dataset)) %>%
    dplyr::group_by(Dataset, Set) %>%
    dplyr::summarise(Proportion = mean(Proportion)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Dataset = forcats::fct_relevel(Dataset, levels=c('Flexicon', 'Stroop', 'Simulated')),
                  Set = forcats::fct_rev(forcats::fct_relevel(Set, levels=c(
                    'Single PP',
                    'Raw - NO SD - Mean/Median',
                    'Raw/Log/Inv - NO SD - Mean',
                    'Raw/Log/Inv - NO SD - Median',
                    'Raw/Log/Inv - NO SD - Mean/Median',
                    'Raw - NO/2.0/2.5/3.0 SD - Mean',
                    'All'))))


  ggplot2::ggplot(d, ggplot2::aes(x=Set, y=Proportion, fill=Dataset))+
    ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(),width = 0.4)  +
    ggplot2::geom_hline(yintercept=.05, color='grey50', linetype=2, size=.5)+
    ggplot2::geom_hline(yintercept=.1, color='grey50', linetype=2, size=.5)+
    ggplot2::geom_hline(yintercept=.15, color='grey50', linetype=2, size=.5)+
    ggplot2::labs(x='Preprocessing Pipeline', y="Proportion of false positives", fill=NULL)+ggthemes::theme_tufte()+
    ggplot2::theme(text=ggplot2::element_text(family = "Gill Sans MT"),
          axis.text.y = ggplot2::element_text(size=10),
          axis.text.x = ggplot2::element_text(size=10, face=c('plain','bold','plain','plain')),
          legend.position=c(.3, -.22),
          legend.background = ggplot2::element_rect(fill=NA, color=NA),
          legend.text = ggplot2::element_text(size=10),
          legend.direction = 'horizontal',
          legend.key = ggplot2::element_rect(fill=NA, color=NA),
          panel.grid.major.y = ggplot2::element_blank(),
          aspect.ratio = 2/3,
          plot.margin=grid::unit(c(0,0,0,0), "mm")
    )+
    ggplot2::coord_flip()+
    ggplot2::scale_fill_grey()


  }

