drake_plan:
	Rscript ./drake/drake_plan.R

slurm:
sbatch ./drake/drake_plan.R

website:
	R -e 'pkgdown::build_site()'
	firefox ./docs/index.html
