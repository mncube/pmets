test_that("UL1 returns a dataframe of length X", {

  df_ul1 <- data.frame(domain = 1:4,
                       imp_w = c(.25, .25, .25, .25),
                       dif_w = c(.25, .25, .25, .25))

  df_ul1_ip <- item_pool(df = df_ul1,
                         L1N = "domain",
                         L1W = list("imp_w", "dif_w"),
                         L1F = function(x)mean(x),
                         cal_orig = 20,
                         max_its = 100)


  expect_equal(2 * 2, 4)
})
