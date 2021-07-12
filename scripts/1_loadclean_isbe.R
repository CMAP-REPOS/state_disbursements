# Read in ISBE data 
# Stern
# July 2021


# startup ----------------------------------

# packages
require(tidyverse)
require(here)
require(readxl)

# source helper functions
source(here("scripts", "0_helpers.R"))

# location of input data
setwd(here("data_raw", "isbe_ilearn"))


# process excel files -----------------------
files <- list.files(pattern = ".xls$|.xlsx$")

# identify years
files_tbl <- str_remove(files, "ILEARN-FY") %>% 
  parse_number() %>% 
  {if_else(. < 90, 2000 + ., 1900 + .)} %>% 
  tibble(year = ., fname = files) %>% 
  arrange(year) %>% 
  # header lines to skip vary by year a bit  
  # *** IMPORTANT NOTE ***
  # Future year excel files should be checked to make sure the format has not evolved
  mutate(skip = case_when(
    year <= 2001 ~ 5,
    year <= 2004 ~ 4,
    year == 2005 ~ 6,
    year <= 2016 ~ 5,
    TRUE ~ 0 # looks like 2017 and forward don't contain headers
  ))

# import files
dfs <- map2(files_tbl$fname, files_tbl$skip, ~(read_excel(path = .x, skip = .y))) %>% 
  set_names(files_tbl$year)


# Function to process excel files
#
# *** IMPORTANT NOTE ***
# Future year excel files should be checked to make sure the format has not evolved
clean_excel <- function(df, fy){
  
  message(paste0(fy, "..."))
  #browser()
  
  df <- df %>% 
    # drop percentage fields
    select(-starts_with("%"), -contains(" Pct of Total")) %>%
    # make all years lowercase
    set_names(., tolower) %>% 
    # make prior year columns standard
    set_names(., str_replace(names(.), as.character(fy - 2), "prior yr")) %>% 
    # normalize district type column name
    set_names(., str_replace(names(.), "district type", "type")) %>% 
    # normalize total revenues column
    set_names(., str_replace(names(.), "^total receipts/ revenues$|^total revenues$|^total receipts$", "total revenue"))

  # check and remove total row
  df <- total_check_extract(df, "district name", "total revenue", fy, "Total")
  # 2002 is blank Statewide Total|State Totals|Totals|
  
  # tables after 2002 don't have Fiscal years
  if(!("FY" %in% names(df))){
    df <- mutate(df, fy = fy, .before = "type")
  }
  
  # 1997 table seems to have wrong FY listed
  if(fy == 1997 & length(unique(df$fy)) == 1 &  as.numeric(df$fy[[1]]) == 1999){
    df$fy <- 1997
  }
  
  #assert all FY are the same and equal the year in the filename
  stopifnot(length(unique(df$fy)) == 1)
  stopifnot(as.numeric(df$fy[[1]]) == fy)
  
  return(df)
}

## CURRENTLY STUCK ON HANDLING OF TOTAL ROWS AS IT IS INCONSISTENT

# map function across all available excel workbooks
dfs_out <- map2(dfs, files_tbl$year, clean_excel)


# combine and export  ---------------------------

# collapse lists, combine into one
output <- bind_rows(dfs_out)

# confirm all rows are present
stopifnot(sum(map_int(dfs_out, nrow)) == nrow(output))


# export as excel workbook and RDS
setwd(here("data_processed"))
write_csv(output, "isbe_ilearn.csv")
saveRDS(output, file = "isbe_ilearn.rds")

