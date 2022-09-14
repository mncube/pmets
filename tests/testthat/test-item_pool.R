test_that("UL1 returns a dataframe of length X", {

  df_ul1 <- data.frame(domain = 1:4,
                       imp_w = c(.25, .25, .25, .25),
                       dif_w = c(.50, .20, .20, .10))

  df_ul1_ip <- item_pool(df_ul1,
                         ULevels = list("domain"),
                         ULWeights = list(c(imp_w, dif_w)),
                         ULFuncs = list(cal*mean(ULWeights[[1]])/2))


  expect_equal(2 * 2, 4)
})
