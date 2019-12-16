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
# d = get_dummy_df(2,3,10)
#'
