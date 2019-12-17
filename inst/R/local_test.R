#' Benchamarking sorting in data.table
#'
#'
#
# unsorted = data.table::as.data.table(expand.grid(a = base::sample(1:1000), b = base::sample(1:1000)))
# sorted = unsorted[order(a, b)]
#
# rbenchmark::benchmark("Unsorted" = {unsorted[order(a,b)]},
#                       "Sorted" = sorted[order(a,b)],
#                       replications=100,
#                       columns=c('elapsed', 'relative'))
#


#' #' TESTING SIMULATION LOOP
#'
ni = 1
np = 3
ntc = 10
d = get_dummy_df(ni,np,ntc) %>% dplyr::select(-iter)
d = rtsimpack::compute_one_iteration(d, ni, np, ntc, get_ids(d))

f = "/home/zsigmas/Dropbox/CienciaSobreCiencia/rtsimpack/inst/test_data/test_comma.csv"

r = run_simulation(f, ni=1000, np=3, nipi=1000)

rbenchmark::benchmark("1" = run_simulation(f, ni=1000, np=2, nipi=1),
                      "10" = run_simulation(f, ni=1000, np=2, nipi=10),
                      "100" = run_simulation(f, ni=1000, np=2, nipi=100),
                      "1000" = run_simulation(f, ni=1000, np=2, nipi=1000),
                      replications=3,
                      columns=c('elapsed', 'relative'))

# It seems like the benefit decays rapidly with the nipi, being the nipi=1 very slow
# elapsed relative
# 1 132.055    2.236
# 2  67.859    1.149
# 3  59.928    1.015
# 4  59.071    1.000
