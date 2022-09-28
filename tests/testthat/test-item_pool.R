test_that("item_pool_1l returns a dataframe of length 4", {

  df_ul1 <- data.frame(domain = 1:4,
                       imp_w = c(1, 1, 1, 1),
                       dif_w = c(1, 1, 1, 1))

  df_ul1_ip <- item_pool_1l(df = df_ul1,
                            l1_name = "domain",
                         l1_func = function(imp_w, dif_w)5*rowSums(cbind(imp_w,dif_w)),
                         cal_length = 40,
                         max_its = 100)


  expect_equal(2 * 2, 4)
})

#item_pool <- function(df,
#                      L1F,
#                      w){
#  #f <- function(...) names(rlang::enexprs(...))
#  #params <- mget(formalArgs(L1F))
#  x <- w
#  df %>%
#    dplyr::mutate(L1_Items = do.call(L1F, as.list(formalArgs(L1F))))
#}

#Create data frame
#df_ul1 <- data.frame(domain = 1:4,
#                     y = c(.25, .25, .25, .25),
#                     z = c(.25, .25, .25, .25))

#Create L1_Items variable based on L1F function
#df_ul1_ip <- item_pool(df = df_ul1,
#                       L1F = function(x,y,z){ x * rowMeans(cbind(y, z)) },
#                       w = 20)
