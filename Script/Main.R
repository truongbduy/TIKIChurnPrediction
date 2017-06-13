# PROJECT_FOLDER = "D:/Github/TIKIChurnPrediction/"
PROJECT_FOLDER = "/home/duytruong/GitHub/TIKIChurnPrediction/"
Settings <- paste0(PROJECT_FOLDER, "Settings.R")
source(Settings)

initial_data <- read_log_data()
#initial_data <- rbind(initial_data, read_log_data("FlumeData.1491930001087"))
initial_data <- merge_w_product(initial_data)

# Customer dataset
customer_data <- construct_ctm_data(initial_data)

# Item dataset
item_data <- construct_item_data(initial_data)

# Brand dataset
brand_data <- construct_brand_data(initial_data)

# Cate dataset
# I think we should use attributes instead 
cate_data <- construct_cate_data(initial_data)

