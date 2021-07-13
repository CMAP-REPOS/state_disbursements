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


# clean excel files -----------------------

# total row in 2002 table is just blank.
dfs[["2002"]][[nrow(dfs[["2002"]]), "District Name"]] <- "Total"

# Function to process excel files
#
# *** IMPORTANT NOTE ***
# Future year excel files should be checked to make sure the format has not evolved
clean_excel <- function(df, fy){
  
  # column names are a mess. Edits occur in two parts:
  
  # first, substring-level replacements 
  names <- names(df) %>% 
    # make everything lowercase
    tolower() %>% 
    # remove references to prior years
    str_replace(as.character(fy - 2), "prior yr") %>% 
    # remove instances of current year in name
    str_remove(paste0("^", fy, " ")) %>% 
    # remove various stray punctuation
    str_remove("'") %>% 
    # make plurals singular
    str_replace("revenues", "revenue") %>% 
    str_replace("expenditures", "expenditure") %>% 
    str_replace("disbursements", "disbursement")
  
  # then, convert to df to perform string-level replacements
  names <- as.data.frame(names) %>% 
    set_names("nm") %>% 
    mutate(nm = case_when(
      str_detect(nm, "^total receipt|^total revenue") ~ "total revenue",        # total revenue
      str_detect(nm, "^id$|^rcdt") ~ "district number",                         # dist number
      str_detect(nm, "^type$") ~ "district type",                               # dist type
      str_detect(nm, "^district$") ~ "district name",                           # dist name
      str_detect(nm, "^cnty$|^county no$") ~ "county",                          # county code
      str_detect(nm, "average daily attendance$|ada$") ~ "avg daily attend",    # attendance
      str_detect(nm, "^total expenditure") ~ "total expenditure",               # total expenditure
      TRUE ~ nm
    ))
  
  # finally, overwrite names with adjusted ones
  df <- set_names(df, names$nm)

  # check and remove total row
  df <- total_check_extract(df, "district name", "total revenue", fy, "Total|total")
  # 2002 is blank Statewide Total|State Totals|Totals|
  
  # tables after 2002 don't have Fiscal years
  if(!("FY" %in% names(df))){
    df <- mutate(df, fy = fy, .before = "district type")
  }
  
  # 1997 table seems to have wrong FY listed
  if(fy == 1997 & length(unique(df$fy)) == 1 &  as.numeric(df$fy[[1]]) == 1999){
    df$fy <- 1997
  }
  
  #assert all FY are the same and equal the year in the filename
  stopifnot(length(unique(df$fy)) == 1)
  stopifnot(as.numeric(df$fy[[1]]) == fy)
  
  # adjust column data types and capitalization
  df <- mutate(df,
               county = as.character(county) %>% 
                 str_trunc(3, side = "left", ellipsis = "") %>% 
                 str_pad(3, side = "left", pad = "0"),
               fy = as.numeric(fy),
               `district name` = tolower(`district name`),
               `district number` = str_pad(`district number`, 13, side = "right", pad = "0"),
               `district type` = as.character(`district type`)
               ) %>% 
    # drop percentage fields and region field
    select(-starts_with("%"), -contains(" Pct of Total"), -contains("region")) %>% 
    # FOR NOW, DROP TAX RATE AND RANK FIELDS. SOME ARE IMPORTING AS DATES.
    select(-ends_with("tax rate"), -ends_with("rank"))
  
  return(df)
}

# map function across all available excel workbooks
dfs_out <- map2(dfs, files_tbl$year, clean_excel)


# combine and export  ---------------------------

## STUCK HERE. VARIOUS DATAFRAMES HAVE DIFFERENT COL NAMES, NOT WORTH PROCESSING FURTHER UNTIL WE NEED THE DATA
# collapse lists, combine into one
output <- bind_rows(dfs_out)
names(output)[str_which(names(output), "total")]

# at the moment, just using this to create a master list of school districts
districts <- dfs_out %>% 
  map(select, one_of("county", "district number", "fy", "district name", "average daily attendance")) %>% 
  bind_rows() %>% 
  arrange(`district number`, fy) %>% 
  group_by(`district number`) %>% 
  summarize(county = first(county),
            fy_min = min(fy),
            fy_max = max(fy),
            name = last(`district name`),
            names = paste(unique(`district name`), collapse = ","),
            pop = last(`average daily attendance`))

setwd(here("resources"))
write_csv(districts, "schooldistricts.csv")


# ls <- map(dfs_out, names)
# mx <- max(map_int(ls, length))
# ls <- lapply(ls, function(lst) c(lst, rep(NA, mx - length(lst))))
# as_tibble(ls) %>% View()
# 
# 
# # confirm all rows are present
# stopifnot(sum(map_int(dfs_out, nrow)) == nrow(output))
# 
# 
# # export as excel workbook and RDS
# setwd(here("data_processed"))
# write_csv(output, "isbe_ilearn.csv")
# saveRDS(output, file = "isbe_ilearn.rds")
# 
