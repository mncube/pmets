item_pool <- function(df,
                      ULevels,
                      ULWeights,
                      ULFuncs,
                      cal_orig = 20,
                      min_tl = 4,
                      forms = 2,
                      mults = 3.3,
                      max_its = 15,
                      seed = 100){
  #Initialize repeat loop
  cal <- cal_orig
  its <- 1
  it_rounds <- 1

  if (length(ULevels) == 1){
    x <- ULFuncs[[1]]
    repeat{
      items_by_ul1 <- df %>% ##Stopped Here Need to Fix the Rest##
        dplyr::mutate(cal = cal) %>%
        #mutate(Items_UL1 = round(cal*.25*(W_FOU + W_IOPS + W_IONC + W_DEO), 0)) %>%
        dplyr::mutate(Items_UL1 = function(x)x) %>%
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
        dplyr::mutate(Items_UL1 = ifelse(Items_UL1 < min_tl,
                                round(Items_UL1 + (min_tl - Items_UL1), 0),
                                Items_UL1))

      #Check that the calibration number worked
      if (sum(items_by_ul1$Items_UL1) %in% c(cal_orig - 1, cal_orig, cal_orig + 1)) {break}

      if (its == max_its){stop("algorithm didn't converge")}

      #Update cal and its
      cal <- cal - 1
      its <- its + 1
    }

    #Randomly add item if total length cal - 1
    if (sum(items_by_ul1$Items_UL1) == cal_orig - 1){
      tot_rows <- nrow(items_by_ul1)
      items_by_ul1 <- items_by_ul1 %>%
        mutate(fix_length_random_order = sample(1:tot_rows, tot_rows, replace = FALSE)) %>%
        mutate(Items_UL1 = ifelse(fix_length_random_order == 1, Items_UL1 + 1, Items_UL1)) %>%
        mutate(Items_UL1 = max(Items_UL1)) %>%
        ungroup()
    }

    #Randomly subtract item if total length is cal + 1
    if (sum(items_by_ul1$Items_UL1) == cal_orig + 1){
      tot_rows <- nrow(items_by_ul1)
      items_by_ul1 <- items_by_ul1%>%
        mutate(fix_length_random_order = sample(1:tot_rows, tot_rows, replace = FALSE)) %>%
        mutate(Items_UL1 = ifelse(fix_length_random_order == 1, Items_UL1 - 1, Items_UL1)) %>%
        mutate(Items_UL1 = min(Items_UL1))
    }

  }

  return(items_by_ul1)

}
