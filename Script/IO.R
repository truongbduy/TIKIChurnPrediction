

read_raw_data <- function(){
  json_file_line <- readLines(DATA_PATH)
  df <- data.frame()
  for(i in json_file_line){
    parsedJSONLine <- fromJSON(i)
    listJSONLine <- as.data.frame(t(unlist(parsedJSONLine)), stringsAsFactors = F)
    df <- rbind.fill(df, listJSONLine)
  }
  return(df)
}

read_data <- function(){
  df.input <- read_raw_data()
  df.output <- subset(df.input, product_id != 0, 
                      select = c("user_id", "event_type", "created_at", "client_id", "product_id", "order_id"))
  return(df.output)
}