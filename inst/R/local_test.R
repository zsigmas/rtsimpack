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


################### Some code to launch benchmarking and simple tests

ni = 1
np = 3
ntc = 10
d = get_dummy_df(ni,np,ntc) %>% dplyr::select(-iter)
d = rtsimpack::compute_one_iteration(d, ni, np, ntc, get_ids(d))

f = "/home/zsigmas/Dropbox/CienciaSobreCiencia/rtsimpack/inst/test_data/test_comma.csv"
r = run_simulation(f, ni=100, np=10, nipi=10)

################### Testing the impact of nipi I
f = "/home/zsigmas/Dropbox/CienciaSobreCiencia/rtsimpack/inst/test_data/test_comma.csv"
r = run_simulation(f, ni=100, np=20, nipi=10)

rbenchmark::benchmark("1" = run_simulation(f, ni=1000, np=2, nipi=1),
                      "10" = run_simulation(f, ni=1000, np=2, nipi=10),
                      "100" = run_simulation(f, ni=1000, np=2, nipi=100),
                      "1000" = run_simulation(f, ni=1000, np=2, nipi=1000),
                      replications=3,
                      columns = c('test', 'replications', 'elapsed', 'relative'))

# test replications elapsed relative
# 1    1            3  71.647    9.394
# 2   10            3  13.600    1.783
# 3  100            3   7.627    1.000
# 4 1000            3  10.641    1.395

################### Testing the impact of nipi II
f = "/home/zsigmas/Dropbox/CienciaSobreCiencia/rtsimpack/inst/test_data/test_comma.csv"
r = run_simulation(f, ni=100, np=20, nipi=10)

rbenchmark::benchmark("1" = run_simulation(f, ni=100, np=30, nipi=1),
                      "5" = run_simulation(f, ni=100, np=30, nipi=5),
                      "10" = run_simulation(f, ni=100, np=30, nipi=10),
                      "50" = run_simulation(f, ni=100, np=30, nipi=50),
                      "100" = run_simulation(f, ni=100, np=30, nipi=100),
                      replications=3,
                      columns = c('test', 'replications', 'elapsed', 'relative'))

################## Benchmarking aggregate dplyr vs data.table
d = rtsimpack::dummy_agg

rbenchmark::benchmark("dplyr" = rtsimpack::compute_agg_dp(d),
                      "data.table" = rtsimpack::compute_agg_dt(d),
                      replications=10,
                      columns = c('test', 'replications', 'elapsed', 'relative'))

# test replications elapsed relative
# 2 data.table           10   1.937    1.000
# 1      dplyr           10  13.483    6.961

################# Benchmarking participant counting

d = rtsimpack::dummy_test
d[, .(count = .N), by = c('iter')][[1,'count']]/2
nrow(d)/length(unique(d[['iter']]))/2

rbenchmark::benchmark("div" = nrow(d)/length(unique(d[['iter']]))/2,
                      "count" = d[, .(count = .N), by = c('iter')][[1,'count']]/2,
                      "unique" = length(unique(d[['id']])),
                      replications=10000,
                      columns = c('test', 'replications', 'elapsed', 'relative'))

# test replications elapsed relative
# 2  count        10000   4.455   31.373
# 1    div        10000   0.169    1.190
# 3 unique        10000   0.142    1.000

################## Benchmarking test dplyr vs data.table
d = rtsimpack::dummy_test

rbenchmark::benchmark("dplyr" = rtsimpack::compute_test_dp(d),
                      "data.table" = rtsimpack::compute_test_dt(d),
                      replications=100,
                      columns = c('test', 'replications', 'elapsed', 'relative'))

# test replications elapsed relative
# 2 data.table          100   1.698    1.000
# 1      dplyr          100  24.018   14.145
