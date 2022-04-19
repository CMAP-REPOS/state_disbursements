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
    str_replace("disbursements", "disbursement") %>% 
    # deal with any whitespace
    str_trim()
  
  # then, convert to df to perform field-level replacements
  names <- as.data.frame(names) %>% 
    set_names("nm") %>% 
    mutate(nm = case_when(
      str_detect(nm, "^cnty$|^county no$") ~ "county",                          # county code
      str_detect(nm, "^id$|^rcdt") ~ "district number",                         # dist number
      str_detect(nm, "^fiscal year$") ~ "fy",                                   # fiscal year
      str_detect(nm, "^type$") ~ "district type",                               # dist type
      str_detect(nm, "^district$") ~ "district name",                           # dist name
      str_detect(nm, "^total receipt|^total revenue") ~ "total revenue",        # total revenue
      str_detect(nm, "^(instruction|general administration|support services|other expenses) %$") ~ nm, # don't adjust the 4 % fields (new in FY20)
      str_detect(nm, "^instruction per") ~ nm,                                  # don't adjust any "instruction per pupil" cols
      str_detect(nm, "^instruction") ~ "instruction",                           # expend: instruction
      str_detect(nm, "^general admin") ~ "general admin",                       # expend: general administration
      str_detect(nm, "^support service") ~ "support services",                  # expend: support services
      str_detect(nm, "^other exp") ~ "other expenses",                          # expend: other
      str_detect(nm, "^total exp") ~ "total expenditure",                       # expend: total
      str_detect(nm, "difference|^amount of revenue over") ~ "net (rev-exp)",   # revenues less expenditures
      str_detect(nm, "k - 12|k-12|k12") ~ "net operating expense (k-12)",           # net k-12 operating expenses
      str_detect(nm, "average daily attend[ae]nce$|ada$") ~ "avg daily attend",    # attendance
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
               `district name` = str_to_title(`district name`),
               `district number` = str_pad(`district number`, 13, side = "right", pad = "0"),
               `district type` = as.character(`district type`)
               ) %>% 
    # drop percentage fields and region field
    select(-contains("%"), -contains(" Pct of Total"), -contains("region")) %>% 
    # FOR NOW, DROP TAX RATE AND RANK FIELDS. SOME ARE IMPORTING AS DATES. FIX IF/WHEN IT'S A PROBLEM
    select(-ends_with("tax rate"), -ends_with("rank"), -ends_with("ranking"))
  
  return(df)
}

# map function across all available excel workbooks
dfs_out <- map2(dfs, files_tbl$year, clean_excel)


# combine and export  ---------------------------


# collapse lists, combine into one
output <- bind_rows(dfs_out)

## SOME COL NAMES REMAIN TO BE MATCHED BUT CHOOSING TO IGNORE THIS UNTIL IT'S A PROBLEM.
ls <- map(dfs_out, names)
mx <- max(map_int(ls, length))
ls <- lapply(ls, function(lst) c(lst, rep(NA, mx - length(lst))))
as_tibble(ls) %>% View()

# confirm all rows are present
stopifnot(sum(map_int(dfs_out, nrow)) == nrow(output))


# export as excel workbook and RDS
setwd(here("data_processed"))
write_csv(output, "isbe_ilearn.csv")
saveRDS(output, file = "isbe_ilearn.rds")

