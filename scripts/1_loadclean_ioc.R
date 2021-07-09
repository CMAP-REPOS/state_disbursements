# Read in IOC revenues data 
# Stern
# July 2021

# startup ----------------------------------

# packages
require(tidyverse)
require(here)
require(DBI)

# location of input data
setwd(here("data_raw", "ioc_afr"))

# import category descriptions
cats <- read.csv(here("resources", "ioc_cats.csv")) %>% 
  select(Category, CatName = Catname, CatName2 = Subcategory)

# identify databases -----------------------

files <- list.files(pattern = ".mdb$")


# import and process -----------------------

# Function to fetch and process revenue table data from IOC databases.
# Presumes you have already `setwd()` and the `filename` is in the working dir.
process_ioc <- function(filename, cats_table){
  
  # Establish a connection
  # (help from https://stackoverflow.com/a/52096456; https://db.rstudio.com/r-packages/dbi/)
  con <- dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                                                   getwd(), "//",filename,";"))
  
  # import necessary tables
  revenues <- dbReadTable(con, "Revenues")
  unitdata <- dbReadTable(con, "UnitData")
  
  # Close connection
  dbDisconnect(con)
  
  # create dataset
  df <- select(unitdata, Code, C1, County, UnitName, Description) %>% 
    inner_join(revenues, by = "Code") %>% 
    left_join(cats_table, by = "Category") %>% 
    as_tibble() %>% 
    select(UnitName, Code, County, C1, Description, CatCode = Category, CatName, CatName2, FY, everything())
  
  #assert all FY are the same and equal the year in the filename
  stopifnot(length(unique(df$FY)) == 1)
  stopifnot(as.numeric(df$FY[[1]]) == parse_number(filename))
  
  ## CURRENTLY USING ALL REVENUES. LINDSAY SUGGESTS ONLY... 
  ##   GN, SP, CP, and DS which are general funds, special funds, capital funds, and debt service funds.  
  # presumes that all columns from 10 onwards are revenue columns.
  cols_to_sum <- colnames(df)[10:length(colnames(df))] 
  
  # process database
  df <- df %>% 
    # filter for total rows only, to avoid double counting
    filter(str_sub(CatCode, 4) =="t") %>% 
    # sum revenues
    mutate(total = rowSums(across(all_of(cols_to_sum)), na.rm = TRUE))
  
  message(paste0(filename, ": ", paste(cols_to_sum, collapse = " ")))
  
  return(df)
}

# do the deed (Takes a while)
dfs_out <- map(files, process_ioc, cats) %>% 
  set_names(files)

# combine and export  ---------------------------

output <- bind_rows(dfs_out)


# export as excel workbook and RDS
setwd(here("data_processed"))
write_csv(output, "ioc_revenues.csv")
saveRDS(output, file = "ioc_revenues.rds")


# worth considering other fund categories??
filter(output, EP != 0 | TS != 0 | FD != 0 | DP != 0 | OT != 0) %>% 
  View()


