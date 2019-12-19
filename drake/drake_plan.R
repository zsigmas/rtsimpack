library(drake)
library(rtsimpack)
library(purrr)

expose_imports(rtsimpack)

# Abbreviations
# st: stroop
# fl: flexicon
# _f: _file
# tmpl: template
# cl: clean
# r_: raw
# rep: report
# _p: parameters
# _drk: drake objects

# Paths

## Directories
base_dir = '/home/zsigmas/Dropbox/CienciaSobreCiencia/rtsimpack'
setwd(base_dir) # Make sure we are running the project in the correct place

# Drake does not support variables inside file_in(), file_out, or knit_in
# This is no longer used but keep it as reference for adding files
# r_dir = file.path(base_dir, 'inst/raw_data')
# cl_dir = file.path(base_dir, 'inst/clean_data/')
# tmpl_dir = file.path(base_dir, 'tmpl')
# rep_dir = file.path(base_dir, 'reports')

# ## Templates
# cl_tmpl_f = 'tmpl/clean_template.Rmd'
#
# ## Files
# ### Raw
# r_st_f = 'inst/raw_data/StroopCleanSet.csv'
# r_fl_f = 'inst/raw_datafrench_lexicon_project_rt_data.RData'
#
# ### Clean files
# cl_st_f = 'inst/clean_data/cl_st.csv'
# cl_fl_f = 'inst/clean_data/cl_fl.csv'
#
# ## Report files
# ### Cleaning
# cl_st_rep = 'reports/cl_st.html'
# cl_fl_rep = 'reports/cl_fl.html'


# Plan parameters

## Cleaning
cl_st_p = list(target_nt = 64,
                    min_nt = 32,
                    top_cut= .99,
                    bot_cut= -01)

cl_fl_p = list(target_nt = 2000,
                    min_nt = 1000,
                    top_cut= .99,
                    bot_cut= .01)


# Dependence breaker function

my_render = function(input, output_file, ...){
  # We need this function because render do funny things with the directories, we cannot specify it in input only
  rmarkdown::render(input = input,
                    output_file = basename(output_file),
                    output_dir = dirname(output_file),
                    ...)
}

plan = drake_plan(

  ################################################### Cleaning stage ###########################################################

  #Clean Stroop
  format_st_drk = format_stroop(file_in('inst/raw_data/StroopCleanSet.csv')),
  cl_st_drk = clean_real(format_st_drk,
                         cl_st_p$target_nt,
                         cl_st_p$min_nt,
                         cl_st_p$top_cut,
                         cl_st_p$bottom_cut),
  cl_st_rep_drk = my_render(input = knitr_in('tmpl/clean_template.Rmd'),
                            output_file = file_out('reports/cl_st.html'),
                            params = list(p_dataset_name='Stroop',
                                          p_target_nt= cl_st_p$target_nt,
                                          p_dirty_df = format_st_drk,
                                          p_clean_df = cl_st_drk),
                            quiet=TRUE),
  cl_st_f_drk = readr::write_csv(cl_st_drk, file_out('inst/clean_data/cl_st.csv')),

  # Clean Flexicon
  format_fl_drk = format_flexicon(file_in('inst/raw_data/french_lexicon_project_rt_data.RData')),
  cl_fl_drk = clean_real(format_fl_drk,
                         cl_fl_p$target_nt,
                         cl_fl_p$min_nt,
                         cl_fl_p$top_cut,
                         cl_fl_p$bottom_cut),
  cl_fl_rep_drk = my_render(knitr_in(input = 'tmpl/clean_template.Rmd'),
                            output_file = file_out('reports/cl_fl.html'),
                            params = list(p_dataset_name='Flexicon',
                                          p_target_nt= cl_fl_p$target_nt,
                                          p_dirty_df = format_fl_drk,
                                          p_clean_df = cl_fl_drk),
                            quiet = TRUE),
  cl_fl_f_drk = readr::write_csv(cl_fl_drk, file_out('inst/clean_data/cl_fl.csv')),

  # Clean synth files
  cl_sy_300_20_300_drk = format_synth(file_in('inst/raw_data/synth_miller-m300-s20-t300-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_300_20_300.csv')),
  cl_sy_300_50_300_drk = format_synth(file_in('inst/raw_data/synth_miller-m300-s50-t300-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_300_50_300.csv')),

  cl_sy_350_20_250_drk = format_synth(file_in('inst/raw_data/synth_miller-m350-s20-t250-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_350_20_250.csv')),
  cl_sy_350_50_250_drk = format_synth(file_in('inst/raw_data/synth_miller-m350-s50-t250-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_350_50_250.csv')),

  cl_sy_400_20_200_drk = format_synth(file_in('inst/raw_data/synth_miller-m400-s20-t200-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_400_20_200.csv')),
  cl_sy_400_50_200_drk = format_synth(file_in('inst/raw_data/synth_miller-m400-s50-t200-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_400_50_200.csv')),

  cl_sy_450_20_150_drk = format_synth(file_in('inst/raw_data/synth_miller-m450-s20-t150-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_450_20_150.csv')),
  cl_sy_450_50_150_drk = format_synth(file_in('inst/raw_data/synth_miller-m450-s50-t150-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_450_50_150.csv')),

  cl_sy_500_20_100_drk = format_synth(file_in('inst/raw_data/synth_miller-m500-s20-t100-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_500_20_100.csv')),
  cl_sy_500_50_100_drk = format_synth(file_in('inst/raw_data/synth_miller-m500-s50-t100-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                      file_out('inst/clean_data/cl_sy_500_50_100.csv')),

  cl_sy_550_20_50_drk = format_synth(file_in('inst/raw_data/synth_miller-m550-s20-t50-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                     file_out('inst/clean_data/cl_sy_550_20_50.csv')),
  cl_sy_550_50_50_drk = format_synth(file_in('inst/raw_data/synth_miller-m550-s50-t50-np10000-nt100-ep0-cp0.5-esm0-est0.csv'),
                                     file_out('inst/clean_data/cl_sy_550_50_50.csv')),
)

cfg = drake_config(plan)
vis_drake_graph(cfg)
make(plan)
