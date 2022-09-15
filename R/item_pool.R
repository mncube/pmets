item_pool <- function(df,
                      L1N,
                      L1W,
                      L1F = function(x)x,
                      L2N = NULL,
                      L2F = NULL,
                      L3N = NULL,
                      L3F = NULL,
                      cal_orig = 20,
                      min_tl = 4,
                      forms = 2,
                      mults = 3.3,
                      max_its = 15,
                      seed = 100){
  #Initialize repeat loop
  df <- df %>% dplyr::mutate(cal = cal_orig)
  its <- 1
  it_rounds <- 1

  if (is.null(L2N) & is.null(L3N)){
    repeat{

      df2 <- df %>%
        dplyr::mutate(cal = cal) %>%
        #mutate(Items_UL1 = round(cal*.25*(W_FOU + W_IOPS + W_IONC + W_DEO), 0)) %>%
        dplyr::mutate(L1_Items = L1F(x)) %>%
        #group_by(Domain) %>%
        #mutate(Domain_Length = n())%>%
        #mutate(Items_D = sum(Items_R)) %>%
      #  ungroup() %>%
        #If a domain has less than min_items_per_domain items, then add enough elements
        #to the domain to get min_items_per_domain items in the domain.  Within domain,
        #an equal number of items should be added to each responsibility.
        #mutate(Items_R = ifelse(Items_D < min_items_per_domain,
        #                        round(Items_R + (min_items_per_domain - Items_D)/Domain_Length,
        #                              0), Items_R)) %>%
        dplyr::mutate(L1_Items = ifelse(L1_Items < min_tl,
                                round(L1_Items + (min_tl - L1_Items), 0),
                                L1_Items))

      #Check that the calibration number worked
      if (sum(df$L1_Items) %in% c(cal_orig - 1, cal_orig, cal_orig + 1)) {break}

      if (its == max_its){stop("algorithm didn't converge")}

      #Update cal and its
      df <- df %>%
        dplyr::mutate(cal = cal - 1)
      its <- its + 1
    }

    #Randomly add item if total length cal - 1
    if (sum(df$L1_Items) == cal_orig - 1){
      tot_rows <- nrow(df)
      df <- df %>%
        mutate(fix_length_random_order = sample(1:tot_rows, tot_rows, replace = FALSE)) %>%
        mutate(L1_Items = ifelse(fix_length_random_order == 1, L1_Items + 1, L1_Items)) %>%
        mutate(L1_Items = max(L1_Items)) %>%
        ungroup()
    }

    #Randomly subtract item if total length is cal + 1
    if (sum(df$L1_Items) == cal_orig + 1){
      tot_rows <- nrow(df)
      df <- df%>%
        mutate(fix_length_random_order = sample(1:tot_rows, tot_rows, replace = FALSE)) %>%
        mutate(L1_Items = ifelse(fix_length_random_order == 1, L1_Items - 1, L1_Items)) %>%
        mutate(L1_Items = min(L1_Items))
    }

  }

  return(df)

}
