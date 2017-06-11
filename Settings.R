#DATA_FOLDER = "/home/duytruong/TMP/Experimental/Data/"

DATA_FOLDER = paste0(PROJECT_FOLDER, "Data/")
SCRIPT_FOLDER = paste0(PROJECT_FOLDER, "Script/")


# Import library ----------------------------------------------------------

library_list <- c('rjson', 'dplyr', 'plyr', 'lubridate', 'reshape2')

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

lapply(library_list, FUN = pkgTest)
lapply(library_list, require, character.only = TRUE)



# Import script file ------------------------------------------------------

IO <- paste0(SCRIPT_FOLDER, "IO.R")
Item <- paste0(SCRIPT_FOLDER, "Construct_Item_Data.R")

file_list <- c(IO, Item)

lapply(file_list, source)

# Declare IO Path ---------------------------------------------------------

DATA_PATH = paste0(DATA_FOLDER, "FlumeData.1489999520438")

