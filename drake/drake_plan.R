#!/homedtic/lmoris/miniconda3/envs/rtsimenv/bin/Rscript
#SBATCH -J rtsimpack
#SBATCH -p high
#SBATCH --time=48:00:00
#SBATCH -N 1-1
#SBATCH --mem-per-cpu=8G
#SBATCH -c 12
#SBATCH --workdir=/homedtic/lmoris/rtsimpack
#SBATCH -o /homedtic/lmoris/log/slurm.%N.%J.%u.%a.out # STDOUT
#SBATCH -e /homedtic/lmoris/log/slurm.%N.%J.%u.%a.err # STDERR

suppressMessages(suppressWarnings(library(drake)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(pbapply)))
suppressMessages(suppressWarnings(library(logging)))
pboptions(type='none')

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
# sim_: simulation results
# fp: false_positives

loglevel = 10
basicConfig(level=loglevel)

loginfo('Setting up the directory and environment', logger = 'drake_plan')

# Paths
## Directories
base_dir = '/home/zsigmas/CienciaSobreCiencia/rtsimpack' # This should match the directory where you have extracted the package

is_cluster = !(Sys.info()[['login']]=='zsigmas' & Sys.info()[['nodename']]=='zsigmas-pc')
loginfo(paste0('is_cluster=', is_cluster), logger = 'drake_plan.R')

if(is_cluster){
  base_dir = '/homedtic/lmoris/rtsimpack'
  Sys.setenv(PATH=paste0(Sys.getenv('PATH'),':','/homedtic/lmoris/miniconda3/envs/rtsimenv/bin'))
  n_jobs=12
}else{
  base_dir = '/home/zsigmas/CienciaSobreCiencia/rtsimpack'
  n_jobs = 4
}

loginfo(paste0('base_dir=', base_dir), logger = 'drake_plan.R')
loginfo(paste0('n_jobs=', n_jobs), logger = 'drake_plan.R')

setwd(base_dir) # Make sure we are running the project in the correct place

devtools::load_all()

expose_imports(rtsimpack)

# Plan parameters

## Cleaning
cl_st_p = list(target_nt = 64,
               min_nt = 32,
               top_cut= .99,
               bot_cut= .01)

cl_fl_p = list(target_nt = 2000,
               min_nt = 1000,
               top_cut= .99,
               bot_cut= .01)

sim_p = list(ni = 1000,
             np = 30,
             nipi = 10,
             chunks = 1000) # The total number of iterations per dataset is ni*chunks

# Auxiliary functions

## Render function

my_render = function(input, output_file, ...){
  # We need this function because render do funny things with the directories, we cannot specify it in input only
  rmarkdown::render(input = input,
                    output_file = basename(output_file),
                    output_dir = dirname(output_file),
                    ...)
}

## Ggsave function
my_ggsave  = function(filename, ...){
  ggplot2::ggsave(filename = basename(filename),
                  path = dirname(filename),
                  ...)
}

# Dummy run_simulation so we can divide the work in smaller pieces and parallelize

run_simulation_dummy = function(chunk_num, dataset, ...){
  #Chunk num is discarded is only used as a helper for the cross function below
  run_simulation(file = dataset[[1]], d_name = dataset[[2]], ...)
}

run_simulation_drk = purrr::partial(run_simulation_dummy, ni=sim_p[['ni']], np=sim_p[['np']], nipi=sim_p[['nipi']])

loginfo('Declaring plan', logger = 'drake_plan')

plan = drake_plan(

  ################################################### Cleaning stage ###########################################################

  #Clean Stroop
  format_st_drk = format_stroop(file_in('inst/raw_data/StroopCleanSet.csv')),
  cl_st_drk = clean_real(format_st_drk,
                         cl_st_p$target_nt,
                         cl_st_p$min_nt,
                         cl_st_p$top_cut,
                         cl_st_p$bot_cut),
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
                           cl_fl_p$bot_cut),
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

  ################################################### Running simulations ##############################################


  sim_drk = target(
    run_simulation_drk(chunk=g_chunk, dataset=g_dataset),
    transform = cross(g_dataset = list(list(cl_st_drk, 'stroop'),
                                       list(cl_fl_drk, 'flexicon'),
                                       list(cl_sy_300_20_300_drk, 's_300_20_300'),
                                       list(cl_sy_350_20_250_drk, 's_350_20_250'),
                                       list(cl_sy_400_20_200_drk, 's_00_20_200'),
                                       list(cl_sy_450_20_150_drk, 's_450_20_150'),
                                       list(cl_sy_500_20_100_drk, 's_500_20_100'),
                                       list(cl_sy_550_20_50_drk, 's_550_20_50'),
                                       list(cl_sy_300_50_300_drk, 's_300_50_300'),
                                       list(cl_sy_350_50_250_drk, 's_350_50_250'),
                                       list(cl_sy_400_50_200_drk, 's_400_50_200'),
                                       list(cl_sy_450_50_150_drk, 's_450_50_150'),
                                       list(cl_sy_500_50_100_drk, 's_500_50_100'),
                                       list(cl_sy_550_50_50_drk, 's_550_50_50')
    ),
    chunk = !!(1:sim_p[['chunks']]))
  ),

  jnd_res_drk = target(dplyr::bind_rows(sim_drk),
                       transform = combine(sim_drk),
                       format = 'fst'), # This is a huge target

  ################################################### Analysing data by datasets ##############################################

  ## Calculating fp_rates by single Preprocessing Pipeline

  tide_res_drk = target(tide_rename(jnd_res_drk),
                        format = 'fst'), # This is a huge target

  fp_single_drk = summarise_fp_single(tide_res_drk, alpha=.05),
  fp_single_f_drk = readr::write_csv(fp_single_drk, file_out('inst/results/single_fp.csv')),

  ## Calculating fp_rates by grouped Preprocessing Pipelines
  fp_group_drk = summarise_fp_group(tide_res_drk),
  fp_group_f_drk = readr::write_csv(fp_group_drk, file_out('inst/results/group_fp.csv')),

  ## Final report table and plot

  bar_plot_drk = gen_bar_plot(fp_single_drk, fp_group_drk),

  fp_rep_drk = my_render(knitr_in(input = 'tmpl/analysis_template.Rmd'),
                         output_file = file_out('reports/fp.html'),
                         params = list(fp_df = fp_single_drk),
                         quiet = TRUE),

  bar_plot_f_drk = my_ggsave(file_out('./inst/figures/bar_plot.tiff'),
                             plot=bar_plot_drk,
                             device='tiff',
                             width=90,
                             units='mm',
                             dpi=1000,
                             scale=2,
                             compression = "lzw")
)

loginfo('Setting up the future plan', logger = 'drake_plan')
future::plan(future::multiprocess)

loginfo('Building config', logger = 'drake_plan')
cfg = drake_config(plan)

# loginfo('Outdated targets', logger = 'drake_plan')
#     print.data.frame(outdated(cfg))
#
# loginfo('Building dependency graph', logger = 'drake_plan')
# vis_drake_graph(cfg, file = 'dependency_graph.html', selfcontained = TRUE)

loginfo('Building targets', logger = 'drake_plan')
make(plan, parallelism = 'future',
     jobs = n_jobs,
     prework = c("devtools::load_all()",
                 paste0("logging::basicConfig(level=", loglevel,")")),
     memory_strategy = "autoclean",
     garbage_collection = TRUE,
     keep_going = TRUE)
