
drake_plan:
	Rscript ./drake/drake_plan.R

slurm:
	sbatch ./drake/drake_plan.R

website:
	R -e 'pkgdown::build_site()'
	firefox ./docs/index.html

outated:
	Rscript ./drake/drake_plan.R --outdated --no_make

graph:
	Rscript ./drake/drake_plan.R --graph --no_make
