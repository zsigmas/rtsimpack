test_that("clean_real returns the same number of trials for all, and equal no nt_target",
          {
            d = gen_dummy_sample() %>% dplyr::sample_n(20)
            cd = clean_real(d, 128, 32, .99, .1)
            nt = cd %>%
              dplyr::group_by(id) %>%
              dplyr::summarise(nt=dplyr::n()) %>%
              dplyr::pull(nt)

            expect_true(all(nt==128))
})
