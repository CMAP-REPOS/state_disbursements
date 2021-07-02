# Read in IDOR income and use data 
# Original Author: Timi Koyejo
# Start Date: 11/20/2019
# Modified by: Matt Stern, July 2021

# Note: IDOR files before 2012 are PDFs, imported here via tabulizer and rJava,
#   which require Java be installed. For more info see:
#   https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/


# load packages ---------------------------

library("tidyverse")
library("readxl")
library("here")

library("rJava")
library("tabulizer")
library("writexl")


# Identify files to import ----------------

setwd(here("raw_data", "idor_income_use"))

files_pdf <- list.files(pattern = ".pdf$")
files_excel <- list.files(pattern = ".xls$|.xlsx$")

dfs_out <- list()

# import excel files -----------------------
dfs_excel <- map(files_excel, read_excel, skip = 5) %>% 
  set_names(files_excel)


# FY12 and FY13 use the same two-row-per-entry format. This function cleans.
clean_fy12_13 <- function(df, fy_label){
  
  clean <- df %>% 
    select(local_gov = 1, tax_type = 2, 
         vendor_num = 3, fy_total = ncol(.)) %>%   # rename columns
    mutate_all(list(~na_if(., ""))) %>%            # convert any "" to NA
    fill(fy_total, .direction = "up") %>%          # grab total value from row below
    filter(!is.na(local_gov)) %>%                  # remove dummy 2nd rows
    mutate_at(vars(tax_type), as.factor) %>%       # convert tax_type into factor
    mutate_at(vars(fy_total), as.numeric) %>%      # convert total to numeric
    mutate(fy_year = fy_label)                     # add column with fy year
  
  return(clean)
}

# use function to clean fiscal years 2012 and 2013
dfs_out$fy12 <- clean_fy12_13(dfs_excel$income_use_fy12.xls, 2012)
dfs_out$fy13 <- clean_fy12_13(dfs_excel$income_use_fy13.xls, 2013)


# FY14 and forward use single-row-per-entry format. This function cleans. 
# 
# ***IMPORTANT NOTE*** 
# Future year excel files should be checked to make sure the format has not evolved

clean_fy14_forward <- function(df, fy_label) {
  
  clean <- df %>%
    select(local_gov = 1, tax_type = 2, 
           vendor_num = 3, fy_total = ncol(.)) %>% # select and rename necessary cols
    mutate_at(vars(tax_type), as.factor) %>%       # convert tax_type into factor
    mutate_at(vars(fy_total), as.numeric) %>%      # confirm total is numeric
    mutate(fy_year = fy_label)                     # add column with fy year
  
  return(clean)
}

##  use function to to clean fiscal years 2014 and forward
# create temporary argument vectors 
# (these should automatically expand themselves to include future years if available) 
filenames <- setdiff(files_excel, c("income_use_fy12.xls", "income_use_fy13.xls"))
years_num <- str_extract(filenames, "[[:digit:]]+") %>% 
  str_c("20", .) %>% 
  as.numeric()
years_char <- str_extract(filenames, "[[:digit:]]+") %>% 
  str_c("fy", .)

# map over argument vectors to import files and add to 'dfs_out' list
dfs_out <- map2(dfs_excel[filenames], years_num, clean_fy14_forward) %>% 
  set_names(years_char) %>% 
  append(dfs_out, .)

# remove temporary argument vectors
rm(filenames, years_num, years_char)


# load IDOR income and use tax data  ---------------------------
# Warning it takes a LONG time to extract the pdfs

income_use_fy06 <- extract_tables(file = here("data", "raw", "idor", "income_use", "income_use_fy06.pdf"))
income_use_fy07 <- extract_tables(file = here("data", "raw", "idor", "income_use", "income_use_fy07.pdf"))
income_use_fy08 <- extract_tables(file = here("data", "raw", "idor", "income_use", "income_use_fy08.pdf"))
income_use_fy09 <- extract_tables(file = here("data", "raw", "idor", "income_use", "income_use_fy09.pdf"))
income_use_fy10 <- extract_tables(file = here("data", "raw", "idor", "income_use", "income_use_fy10.pdf"))
income_use_fy11 <- extract_tables(file = here("data", "raw", "idor", "income_use", "income_use_fy11.pdf"))


# Clean raw FY06 - FY11 files to make scrapped pdf files usable ---------------------------

# create function for looping income list cleaning

clean_income_list <- function(income_list, year_fy) {
 
   require(tidyverse)
  
  data <- tibble()
  
  for (i in 1:length(income_list)) {
    
    cat(i,"... ", sep="") # print out number for each element in list cleaned
    
    raw <- income_list %>%
      pluck(i) %>% # select first element
      {suppressMessages(as_tibble(., .name_repair = "unique"))} %>% # suppress message "New names:* `` -> ...1" that
      # arises when colnames generated for matrix columns w/o names
      select(local_gov = 1, tax_type = 2, vendor_num = 3, fy_total = ncol(.)) %>% # rename columns
      slice(-1:-2) %>% # remove first two rows that are poorly formatted headers
      mutate_all(list(~na_if(., ""))) %>% # convert all blanks "" to NA
      fill(fy_total, .direction = "up") %>% # insert the fy_total value from the row below up to deal with bad formatting of pdf
      filter(!is.na(local_gov)) %>%  # remove the rows with NA for local_gov
      filter(local_gov != "Local Government") %>% # filter out extraneous rows with repeating header
      mutate_at(vars(tax_type), as.factor) %>% # convert tax_type into factor
      mutate_at(vars(fy_total), list(~gsub(",","", .))) %>% # remove commas from column to allow conversion of fy_total to numeric
      mutate_at(vars(fy_total), as.numeric) # convert fy_total to numeric
    
    data <- rbind(data, raw)
  }
  data <- mutate(data, fy_year = year_fy) # create a column with year from function arguement
  
  # Check if sum of local gov fy totals matches total in pdf
  
  grand_total_data <- data[[nrow(data), "fy_total"]] # extract the fy total  disbursements
  row_total_data <- sum(head(data$fy_total, -1)) # sum up total column, excluting the last element which is total sum
  check_sums <- grand_total_data == row_total_data # check two numbers the same
  
  if (check_sums) message("Extracted FY Total equal to summed FY total") # Print message coniditonal if numbers are equal or not
  else message("Error: Extracted FY Total does not equal summed FY total")
  
  return(data)
}

# run fuction to clean income use FY06
income_use_fy06_clean <-  clean_income_list(income_use_fy06, 2006)

# run fuction to clean income use FY07
income_use_fy07_clean <-  clean_income_list(income_use_fy07, 2007)

# run fuction to clean income use FY08
income_use_fy08_clean <-  clean_income_list(income_use_fy08, 2008)

# run fuction to clean income use FY09
income_use_fy09_clean <-  clean_income_list(income_use_fy09, 2009)

# run fuction to clean income use FY10
income_use_fy10_clean <-  clean_income_list(income_use_fy10, 2010)

# run fuction to clean income use FY11
income_use_fy11_clean <-  clean_income_list(income_use_fy11, 2011)


# Clean raw FY12 - FY19 files to make scrapped pdf files usable ---------------------------



# create function to clean excel files
clean_income_use_excel <- function(income_excel, year_fy) {
  
  require(tidyverse)
  
  raw <- tibble()
  
  raw <- income_excel %>%
    as_tibble() %>%
    rename_all(tolower) %>%  # rename all columns to lowercase
    select(-july:-june) %>%
    rename(local_gov = `local government`, tax_type = tax,
           vendor_num = `vendor #`, fy_total = `fy total`)
  
  raw <- mutate(raw, fy_year = year_fy) # create a column with year from function arguement
  
  return(raw)
}


# clean FY12 manually due to irregular excel format
income_use_fy12_clean <- income_use_fy12 %>%
  select(local_gov = 1, tax_type = 2, vendor_num = 3, fy_total = ncol(.)) %>% # rename columns
  mutate_all(list(~na_if(., ""))) %>% # convert all blanks "" to NA
  fill(fy_total, .direction = "up") %>% # insert the total value from the row below up to deal with bad formatting of pdf
  filter(!is.na(local_gov)) %>%  # remove the rows with NA for local_gov
  mutate_at(vars(tax_type), as.factor) %>% # convert tax_type into factor
  mutate_at(vars(fy_total), as.numeric) %>% # convert total to numeric
  mutate(fy_year = 2012) # add column with fy year

# clean FY13 manually due to irregular excel format
income_use_fy13_clean <- income_use_fy13 %>%
  select(local_gov = 1, tax_type = 2, vendor_num = 3, fy_total = ncol(.)) %>% # rename columns
  mutate_all(list(~na_if(., ""))) %>% # convert all blanks "" to NA
  fill(fy_total, .direction = "up") %>% # insert the total value from the row below up to deal with bad formatting of pdf
  filter(!is.na(local_gov)) %>%  # remove the rows with NA for local_gov
  mutate_at(vars(tax_type), as.factor) %>% # convert tax_type into factor
  mutate_at(vars(fy_total), as.numeric) %>% # convert total to numeric
  mutate(fy_year = 2013) # add column with fy year


# run function to clean FY14
income_use_fy14_clean <- clean_income_use_excel(income_use_fy14, 2014)

# run function to clean FY15
income_use_fy15_clean <- clean_income_use_excel(income_use_fy15, 2015)

# run function to clean FY16
income_use_fy16_clean <- clean_income_use_excel(income_use_fy16, 2016)

# run function to clean FY17
income_use_fy17_clean <- clean_income_use_excel(income_use_fy17, 2017)

# run function to clean FY18
income_use_fy18_clean <- clean_income_use_excel(income_use_fy18, 2018)

# run function to clean FY19
income_use_fy19_clean <- clean_income_use_excel(income_use_fy19, 2019)

# Create list of cleaned FY datasets  ---------------------------
list_inc_clean <- setNames(lapply(ls(pattern="[0-9]_clean"), function(x) get(x)), ls(pattern = "[0-9]_clean"))

# bind tibbles from all FYs into one
df_inc_clean <- bind_rows(list_inc_clean) %>%
  mutate_at(vars(tax_type), as.factor) %>% 
  slice(1:(n()-4)) %>%  # remove last 4 rows with total and extraneous column
  filter(local_gov != "TOTAL")
# export as excel workbook and RDS
write_xlsx(df_inc_clean, path = here("data", "processed", "income_use_fy06_19_clean.xlsx"))
saveRDS(df_inc_clean, file = here("data", "processed", "income_use_fy06_19_clean.rds"))

