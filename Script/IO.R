

#read_raw_log_data <- function(filePath = LOG_DATA_PATH){
#  if(filePath != LOG_DATA_PATH)
#    filePath <- paste0(DATA_FOLDER, filePath)
#  
#  json_file_line <- readLines(filePath)
#  df <- data.frame()
#  for(i in json_file_line){
#    parsedJSONLine <- fromJSON(i)
#    listJSONLine <- as.data.frame(t(unlist(parsedJSONLine)), stringsAsFactors = F)
#    df <- rbind.fill(df, listJSONLine)
#  }
#  return(df)
#}

read_log_data <- function(){
  #df.input <- read_raw_log_data(filePath)
  #df.output <- subset(df.input, product_id != 0, 
  #                    select = c("user_id", "event_type", "created_at", "client_id", "product_id", "order_id", "quantity"))
  #df.output$quantity <- as.numeric(df.output$quantity)
  #return(df.output)
  CONSOLIDATE_DATA_PATH <- paste0(DATA_FOLDER, "consolidate_log_data.csv")
  if(file.exists(CONSOLIDATE_DATA_PATH)){
    output <- read.csv(CONSOLIDATE_DATA_PATH)
  }else{
    log_files_list <- list.files(LOG_DATA_FOLDER)
    output <- data.frame()
    for(i in log_files_list){
      LOG_DATA_PATH <- paste0(LOG_DATA_FOLDER, i)
      log_data_date <- read.csv(LOG_DATA_PATH)
      output <- rbind(output, log_data_date)
    }
    output <- subset(output, product_id != 0, 
                     select = c("user_id", "event_type", "created_at", "client_id", "product_id", "order_id", "quantity"))
    write.csv(output, CONSOLIDATE_DATA_PATH, row.names = F)  
  }
  output$quantity <- as.numeric(output$quantity)
  return(output)
}

read_product <- function(){
  df <- read.csv(PRODUCT_DATA_PATH)
  df$brand_id <- as.numeric(df$brand_name)
  return(df)
}

merge_w_product <- function(df){
  df.output <- merge(df, read_product(), by = "product_id")
  df.output$isClick <- with(df.output, ifelse(event_type == "SEARCH_CLICK" | event_type == "RECO_CLICK", TRUE, FALSE))
  df.output$isView <- with(df.output, ifelse(event_type == "VIEW", TRUE, FALSE))
  df.output$isATC <- with(df.output, ifelse(event_type == "ADD_TO_CART", TRUE, FALSE))
  df.output$isBuy <- with(df.output, ifelse(event_type == "BUY", TRUE, FALSE))
  return(df.output)
}


