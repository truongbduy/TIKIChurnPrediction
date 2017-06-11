

item_overall_actions <- function(df){
  ouput <- df %>% group_by(product_id) %>%
    dplyr::summarise(
      item_ovr_view_count = sum(event_type == "VIEW"),
      item_ovr_click_count = sum(event_type == "SEARCH_CLICK" | event_type == "RECO_CLICK" ),
      item_ovr_add_to_cart_count = sum(event_type == "ADD_TO_CART"),
      item_ovr_buy_count = sum(event_type == "BUY"),
      
      item_ovr_view_ratio = sum(event_type == "VIEW")/length(event_type),
      item_ovr_click_ratio = sum(event_type == "SEARCH_CLICK" | event_type == "RECO_CLICK" )/length(event_type),
      item_ovr_add_to_cart_ratio = sum(event_type == "ADD_TO_CART")/length(event_type),
      item_ovr_buy_ratio = sum(event_type == "BUY")/length(event_type)
      )
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
                                        
  df.summarised <- df %>% group_by(product_id, created_date_qualitative) %>%
    dplyr::summarise(
      item_ovr_date_view_count = sum(event_type == "VIEW"),
      item_ovr_date_click_count = sum(event_type == "SEARCH_CLICK" | event_type == "RECO_CLICK" ),
      item_ovr_date_add_to_cart_count = sum(event_type == "ADD_TO_CART"),
      item_ovr_date_buy_count = sum(event_type == "BUY"),
      
      item_ovr_date_view_ratio = sum(event_type == "VIEW")/length(event_type),
      item_ovr_date_click_ratio = sum(event_type == "SEARCH_CLICK" | event_type == "RECO_CLICK" )/length(event_type),
      item_ovr_date_add_to_cart_ratio = sum(event_type == "ADD_TO_CART")/length(event_type),
      item_ovr_date_buy_ratio = sum(event_type == "BUY")/length(event_type)
    )
  
  output <- recast(df.summarised, product_id ~ variable  + created_date_qualitative, 
                measure.var = 3:10)

  return(output)
}

item_overall_wday_actions <- function(df){
  df$created_wday <- weekdays(as.Date(substr(df$created_at, 1, 10)))

  df.summarised <- df %>% group_by(product_id, created_wday) %>%
    dplyr::summarise(
      item_ovr_wday_view_count = sum(event_type == "VIEW"),
      item_ovr_wday_click_count = sum(event_type == "SEARCH_CLICK" | event_type == "RECO_CLICK" ),
      item_ovr_wday_add_to_cart_count = sum(event_type == "ADD_TO_CART"),
      item_ovr_wday_buy_count = sum(event_type == "BUY"),
      
      item_ovr_wday_view_ratio = sum(event_type == "VIEW")/length(event_type),
      item_ovr_wday_click_ratio = sum(event_type == "SEARCH_CLICK" | event_type == "RECO_CLICK" )/length(event_type),
      item_ovr_wday_add_to_cart_ratio = sum(event_type == "ADD_TO_CART")/length(event_type),
      item_ovr_wday_buy_ratio = sum(event_type == "BUY")/length(event_type)
    )
  output <- reshape(df.summarised, product_id ~ variable  + created_wday, 
                    measure.var = 3:10)
  return(output)
}

item_actions_diversity <- function(df){
 
  df.summarised <- df %>% group_by(product_id) %>%
    dplyr::summarise(
      item_view_diversity_count = length(unique(client_id[event_type == "VIEW"])),
      item_click_diversity_count = length(unique(client_id[event_type == "SEARCH_CLICK" | event_type == "RECO_CLICK"])),
      item_add_to_cart_diversity_count = length(unique(client_id[event_type == "ADD_TO_CART"])),
      item_buy_diversity_count = length(unique(client_id[event_type == "BUY"])),
      
      item_view_diversity_ratio = item_view_diversity_count / length(unique(client_id)) / length(unique(client_id)),
      item_click_diversity_ratio = item_view_diversity_count / length(unique(client_id)) / length(unique(client_id)),
      item_add_to_cart_diversity_ratio = item_view_diversity_count / length(unique(client_id)) / length(unique(client_id)),
      item_buy_diversity_ratio = item_view_diversity_count / length(unique(client_id)) / length(unique(client_id))
    )
  output <- df.summarised
  return(output)
}


construct_item_data <- function(df){
  # Action count 
  
}