

item_overall_actions <- function(df){
  df <- as.data.table(df)
  output <- df[, .(item_ovr_view_count = sum(isView),
                   item_ovr_click_count = sum(isClick ),
                   item_ovr_add_to_cart_count = sum(isATC),
                   item_ovr_buy_count = sum(isBuy),
                   
                   item_ovr_view_ratio = sum(isView)/length(event_type),
                   item_ovr_click_ratio = sum(isClick )/length(event_type),
                   item_ovr_add_to_cart_ratio = sum(isATC)/length(event_type),
                   item_ovr_buy_ratio = sum(isBuy)/length(event_type)), 
               by = product_id]
  output <- as.data.frame(output)
  return(output)
}

item_overall_date_actions <- function(df){
  df$created_date <- as.integer(substr(df$created_at, 9, 10))
  df$created_date_qualitative <- ifelse(df$created_date <= 5, "1", 
                                        ifelse(df$created_date <= 10 & df$created_date > 5, "2",
                                               ifelse(df$created_date <= 15 & df$created_date > 10, "3", 
                                                      ifelse(df$created_date <= 20 & df$created_date > 15, "4", 
                                                             ifelse(df$created_date <= 25 & df$created_date > 20, "5", 
                                                                    ifelse(df$created_date > 25, "6", NA))))))
  df <- as.data.table(df)
  df.summarised <- df[, .(item_ovr_date_view_count = sum(isView),
                          item_ovr_date_click_count = sum(isClick),
                          item_ovr_date_add_to_cart_count = sum(isATC),
                          item_ovr_date_buy_count = sum(isBuy),
                          
                          item_ovr_date_view_ratio = sum(isView)/length(event_type),
                          item_ovr_date_click_ratio = sum(isClick)/length(event_type),
                          item_ovr_date_add_to_cart_ratio = sum(isATC)/length(event_type),
                          item_ovr_date_buy_ratio = sum(isBuy)/length(event_type)), 
                      by = .(product_id, created_date_qualitative)]
  df.summarised <- as.data.frame(df.summarised)
  output <- recast(df.summarised, product_id ~ variable  + created_date_qualitative, 
                   measure.var = 3:10)
  output[is.na(output)] <- 0
  
  return(output)
}

item_overall_hour_actions <- function(df){
  df$created_hour <- as.integer(substr(df$created_at, 12, 13))
  df$created_hour_qualitative <- ifelse(df$created_hour <= 7 & df$created_hour >= 23, "night", 
                                        ifelse(df$created_hour <= 12 & df$created_hour >= 8, "morning",
                                               ifelse(df$created_hour <= 18 & df$created_hour >= 13, "afternoon", 
                                                      ifelse(df$created_hour <= 22 & df$created_hour >= 19, "evening", NA))))
  df <- as.data.table(df)
  df.summarised <- df[, .(item_ovr_hour_view_count = sum(isView),
                          item_ovr_hour_click_count = sum(isClick),
                          item_ovr_hour_add_to_cart_count = sum(isATC),
                          item_ovr_hour_buy_count = sum(isBuy),
                          
                          item_ovr_hour_view_ratio = sum(isView)/length(event_type),
                          item_ovr_hour_click_ratio = sum(isClick)/length(event_type),
                          item_ovr_hour_add_to_cart_ratio = sum(isATC)/length(event_type),
                          item_ovr_hour_buy_ratio = sum(isBuy)/length(event_type)), 
                      by = .(product_id, created_hour)]
  df.summarised <- as.data.frame(df.summarised)
  output <- recast(df.summarised, product_id ~ variable  + created_hour, 
                   measure.var = 3:10)
  output[is.na(output)] <- 0
  return(output)
}

item_overall_wday_actions <- function(df){
  df$created_wday <- weekdays(as.Date(substr(df$created_at, 1, 10)))

  df <- as.data.table(df)
  df.summarised <- df[, .(item_ovr_wday_view_count = sum(isView),
                          item_ovr_wday_click_count = sum(isClick),
                          item_ovr_wday_add_to_cart_count = sum(isATC),
                          item_ovr_wday_buy_count = sum(isBuy),
                          
                          item_ovr_wday_view_ratio = sum(isView)/length(event_type),
                          item_ovr_wday_click_ratio = sum(isClick)/length(event_type),
                          item_ovr_wday_add_to_cart_ratio = sum(isATC)/length(event_type),
                          item_ovr_wday_buy_ratio = sum(isBuy)/length(event_type)), 
                      by = .(product_id, created_wday)]
  df.summarised <- as.data.frame(df.summarised)
  output <- recast(df.summarised, 
                    product_id ~ variable + created_wday, 
                    measure.var = 3:10)
  output[is.na(output)] <- 0
  return(output)
}

item_actions_diversity <- function(df){
  df <- as.data.table(df)
  df.summarised <- df[, .(item_view_diversity_count = length(unique(product_id[isView])),
                          item_click_diversity_count = length(unique(product_id[isClick])),
                          item_add_to_cart_diversity_count = length(unique(product_id[isATC])),
                          item_buy_diversity_count = length(unique(product_id[isBuy])),
                          item_action_count = length(unique(product_id))),
                      by = .(product_id)]
  
  df.summarised$item_view_diversity_ratio <- df.summarised$item_view_diversity_count / df.summarised$item_action_count
  df.summarised$item_click_diversity_ratio <- df.summarised$item_click_diversity_count / df.summarised$item_action_count
  df.summarised$item_add_to_cart_diversity_ratio <- df.summarised$item_add_to_cart_diversity_count / df.summarised$item_action_count
  df.summarised$item_buy_diversity_ratio <- df.summarised$item_buy_diversity_count / df.summarised$item_action_count
  
  df.summarised <- as.data.frame(df.summarised)
  df.summarised$item_action_count <- NULL

  output <- df.summarised
  return(output)
}

item_weekly_aggregation <- function(df){
  df$created_wday <- weekdays(as.Date(substr(df$created_at, 1, 10)))
  df.summarised <- df %>% filter(isBuy) %>% group_by(product_id, created_wday, order_id) %>%
    dplyr::summarise(
      number_of_purchase = sum(quantity)
    )
  
  df.summarised.2 <- df.summarised %>% group_by(product_id, created_wday) %>%
    dplyr::summarise(
      item_weekly_aggregation_max = max(number_of_purchase),
      item_weekly_aggregation_mean = mean(number_of_purchase),
      item_weekly_aggregation_sd = sd(number_of_purchase),
      item_weekly_aggregation_median = median(number_of_purchase)
    )
  
  output <- recast(df.summarised.2, 
                   product_id ~ variable + created_wday, 
                   measure.var = 3:6)
  output[is.na(output)] <- 0
  return(output)
}

item_ctm_aggregation <- function(df){
  df$created_date <- substr(df$created_at, 1, 10)
  df.summarised <- df %>% filter(isBuy) %>% group_by(product_id, user_id, order_id) %>%
    dplyr::summarise(
      number_of_purchase = sum(quantity),
      number_of_day = length(unique(created_date))
    )
  
  df.summarised.2 <- df.summarised %>% group_by(product_id) %>%
    dplyr::summarise(
      item_ctm_day_aggregation_max = max(number_of_day),
      item_ctm_day_aggregation_mean = mean(number_of_day),
      item_ctm_day_aggregation_sd = sd(number_of_day),
      item_ctm_day_aggregation_median = median(number_of_day),
      
      item_ctm_item_aggregation_max = max(number_of_purchase),
      item_ctm_item_aggregation_mean = mean(number_of_purchase),
      item_ctm_item_aggregation_sd = sd(number_of_purchase),
      item_ctm_item_aggregation_median = median(number_of_purchase)
    )
  output <- df.summarised.2
  output[is.na(output)] <- 0
  
  return(output)
}

item_recent_actions_week <- function(df, time_T){
  time_T <- as.Date(time_T)
  df$created_date <- as.Date(substr(df$created_at, 1, 10))
  df <- subset(df, created_date <= time_T)
  
  df.transformed <- df %>% group_by(client_id, product_id) %>%
    mutate(purchase_date = max(created_date[isBuy]))
  df.transformed <- subset(df.transformed, created_at >= purchase_date - 7)
  
  df.transformed.2 <- df.transformed %>% group_by(product_id, client_id) %>%
    dplyr::summarise(click_before_purchase = length(event_type[isClick]),
                     view_before_purchase = length(event_type[isView])
    )
  
  df.transformed.3 <- df.transformed.2 %>% group_by(product_id) %>%
    dplyr::summarise(mean_click_before_purchase = mean(click_before_purchase),
                     max_click_before_purchase = max(click_before_purchase),
                     sd_click_before_purchase = sd(click_before_purchase),
                     median_click_before_purchase = median(click_before_purchase),
                     mean_view_before_purchase = mean(view_before_purchase),
                     max_view_before_purchase = max(view_before_purchase),
                     sd_view_before_purchase = sd(view_before_purchase),
                     median_view_before_purchase = median(view_before_purchase))
  
  ouput <- df.transformed.3
  return(output)
}

item_recent_actions_month <- function(df, time_T){
  time_T <- as.Date(time_T)
  df$created_date <- as.Date(substr(df$created_at, 1, 10))
  df <- subset(df, created_date <= time_T)
  
  df.transformed <- df %>% group_by(client_id, product_id) %>%
    mutate(purchase_date = max(created_date[isBuy]))
  df.transformed <- subset(df.transformed, created_at >= purchase_date - 30)
  
  df.transformed.2 <- df.transformed %>% group_by(product_id, client_id) %>%
    dplyr::summarise(click_before_purchase = length(event_type[isClick]),
                     view_before_purchase = length(event_type[isView])
    )
  
  df.transformed.3 <- df.transformed.2 %>% group_by(product_id) %>%
    dplyr::summarise(mean_click_before_purchase = mean(click_before_purchase),
                     max_click_before_purchase = max(click_before_purchase),
                     sd_click_before_purchase = sd(click_before_purchase),
                     median_click_before_purchase = median(click_before_purchase),
                     mean_view_before_purchase = mean(view_before_purchase),
                     max_view_before_purchase = max(view_before_purchase),
                     sd_view_before_purchase = sd(view_before_purchase),
                     median_view_before_purchase = median(view_before_purchase))
  
  ouput <- df.transformed.3
  return(output)
}

construct_item_data <- function(df){
  # Action count 
  item.df.1 <- item_overall_actions(df)
  item.df.2 <- item_overall_date_actions(df)
  item.df.3 <- item_overall_wday_actions(df)
  item.df.4 <- item_actions_diversity(df)
  item.df.5 <- item_weekly_aggregation(df)
  item.df.6 <- item_ctm_aggregation(df)
  item.df.7 <- item_overall_hour_actions(df)
  
  # Merge data
  item.df <- merge(item.df.1, item.df.2, by = "product_id", all = T)
  item.df <- merge(item.df, item.df.3, by = "product_id", all = T)
  item.df <- merge(item.df, item.df.4, by = "product_id", all = T)
  item.df <- merge(item.df, item.df.5, by = "product_id", all = T)
  item.df <- merge(item.df, item.df.6, by = "product_id", all = T)
  item.df <- merge(item.df, item.df.7, by = "product_id", all = T)
  
  return(item.df)
}


