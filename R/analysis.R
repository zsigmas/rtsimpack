#' Summarise results
#'
#'
#'
#'

summarise_fp_single = function(d, alpha=.05){

  d = dplyr::group_by(d, dataset)

  r_ni = dplyr::summarise(d, ni = n())
  r_p = d %>% dplyr::summarise_all(list(fp = function(x){mean(x<=alpha)}))

  r = dplyr::inner_join(r_ni, r_p, by=c('dataset'))

}


#' Bar graph with final results
#'

bar_graph = function(d){



}

#' Tidy and rename the dataset
#'
#'

tide_rename = function(d){
  d2 = d %>%
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
    dplyr::select(-one_of(c('iter', 'ni')))
}

#' Group results by pipelines
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


#' Figures for the paper

gen_figures = function(fp_single, fp_group){
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
                  Se = forcats::fct_relevel(Set, levels=c('Single PP',
                                                     'Raw - NO SD - Mean/Median',
                                                     'Raw/Log/Inv - NO SD - Mean',
                                                     'Raw/Log/Inv - NO SD - Median',
                                                     'Raw/Log/Inv - NO SD - Mean/Median',
                                                     'Raw - NO/2.0/2.5/3.0 SD - Mean',
                                                     'All')))


  ggplot(d, aes(x=Set, y=Proportion, fill=Dataset))+
    geom_bar(stat="identity", position=position_dodge(),width = 0.4)  +
    geom_hline(yintercept=.05, color='grey50', linetype=2, size=.5)+
    geom_hline(yintercept=.1, color='grey50', linetype=2, size=.5)+
    geom_hline(yintercept=.15, color='grey50', linetype=2, size=.5)+
    labs(x='Preprocessing Pipeline', y="Proportion of false positives", fill=NULL)+theme_tufte()+
    theme(text=element_text(family = "Gill Sans MT"),
          axis.text.y = element_text(size=10),
          axis.text.x = element_text(size=10, face=c('plain','bold','plain','plain')),
          legend.position=c(.3, -.22),
          legend.background = element_rect(fill=NA, color=NA),
          legend.text = element_text(size=10),
          legend.direction = 'horizontal',
          legend.key = element_rect(fill=NA, color=NA),
          panel.grid.major.y = element_blank(),
          aspect.ratio = 2/3,
          plot.margin=grid::unit(c(0,0,0,0), "mm")
    )+
    coord_flip()+
    scale_fill_grey()


}

