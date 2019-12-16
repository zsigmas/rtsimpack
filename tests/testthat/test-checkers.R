context("Testing checkers")

test_that('Number of trials checker',
          {
            same_it = expand.grid(id=1:3, iter=1:2, t=1:10)
            same_no_it = expand.grid(id=1:3, t=1:10)
            diff_it = dplyr::bind_rows(expand.grid(id=1:3, iter=1:2, t=1:10),
                                      expand.grid(id=1:2, iter=1:2, t=1:10))
            diff_no_it = dplyr::bind_rows(expand.grid(id=1:3, t=1:10),
                                      expand.grid(id=1:2, t=1:10))
            expect_equal(check_ntrial(same_it), 10)
            expect_equal(check_ntrial(same_no_it), 10)
            expect_equal(check_ntrial(diff_it), NA)
            expect_equal(check_ntrial(diff_no_it), NA)
          })
