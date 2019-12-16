#' Load test data
#'
#'

get_test_data = function(){
  data = readr::read_delim(file = system.file('test_data/test_data.csv',
                                            mustWork = TRUE,
                                            package = 'rtsimpack'),
                         delim = ';',
                         col_types = 'illi',
                         )
}
