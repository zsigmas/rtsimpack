library(drake)
library(rtsimpack)

expose_imports(rtsimpack)

home_dir = '/home/zsigmas/Dropbox/CienciaSobreCiencia/rtsimpack'
input_raw_stroop_file = file.path(home_dir, 'inst/raw_data/StroopCleanSet.csv')
input_cl_tmpl_file = file.path(home_dir, 'drake/clean_template.Rmd')
stroop_cl_report_file = file.path(home_dir, 'drake/t.html')

file.exists(input_raw_stroop, input_raw_stroop)

plan = drake_plan(
  stroop_format = format_stroop(file_in(input_raw_stroop_file)),
  stroop_clean = clean_real(stroop_format, 64, 32, .99, .1),
  stroop_cl_report = rmarkdown::render(input=knitr_in(input_cl_tmpl_file),
                                 output_file=file_out(stroop_cl_report_file),
                                 params = list(p_dataset_name='Stroop',
                                               p_target_nt= 62,
                                               p_dirty_df = stroop_format,
                                               p_clean_df = stroop_clean)
                                 )
)
cfg = drake_config(plan)
vis_drake_graph(cfg)
make(plan, target='stroop_cl_report')
