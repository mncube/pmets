item_pool_1l <- function(df,
                      l1_name,
                      l1_func = NULL,
                      cal_length = 20,
                      min_l1 = 4,
                      forms = 2,
                      mults = 3.3,
                      max_its = 15,
                      seed = 100){
  #Set Seed
  set.seed(seed)

  #Initialize repeat loop
  cal <- cal_length
  its <- 1
  it_round <- 1

  #Check that a level 1 variable name has been provided
  if (missing(l1_name)){
    stop("Must provide a level 1 variable name", call. = TRUE)
  }

    repeat{

      df2 <- df %>%
        #mutate(Items_UL1 = round(cal*.25*(W_FOU + W_IOPS + W_IONC + W_DEO), 0)) %>%
        dplyr::mutate(l1_items = do.call(l1_func, lapply(formalArgs(l1_func), as.name))) %>%
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
        dplyr::mutate(l1_items = ifelse(l1_items < min_l1,
                                round(l1_items + (min_l1 - l1_items), 0),
                                l1_items))

      #Check that the calibration number worked
      if (sum(df$l1_items) %in% c(cal_length - 1, cal_length, cal_length + 1)) {break}

      if (its == max_its){stop("algorithm didn't converge")}

      #Update cal and its
      cal <- cal - 1
      its <- its + 1
    }

    #Randomly add item if total length cal - 1
    if (sum(df$l1_items) == cal_length - 1){
      tot_rows <- nrow(df)
      df <- df %>%
        mutate(fix_length_random_order = sample(1:tot_rows, tot_rows, replace = FALSE)) %>%
        mutate(l1_items = ifelse(fix_length_random_order == 1, l1_items + 1, l1_items)) %>%
        mutate(l1_items = max(l1_items)) %>%
        ungroup()
    }

    #Randomly subtract item if total length is cal + 1
    if (sum(df$l1_items) == cal_length + 1){
      tot_rows <- nrow(df)
      df <- df%>%
        mutate(fix_length_random_order = sample(1:tot_rows, tot_rows, replace = FALSE)) %>%
        mutate(l1_items = ifelse(fix_length_random_order == 1, l1_items - 1, l1_items)) %>%
        mutate(l1_items = min(l1_items))
    }

  #Get number of level1 items in item bank
  df <- df %>%
    mutate(l1_items_bank = round(l1_items*forms*mults))



  return(df)

}
