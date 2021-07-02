# Read in IDOR income and use data 
# Original Author: Timi Koyejo
# Start Date: 11/20/2019
# Modified by: Matt Stern, July 2021

# Note: IDOR files before 2012 are PDFs, imported here via tabulizer and rJava,
#   which require Java be installed. For more info see:
#   https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/


# load packages ---------------------------

library("tidyverse")
library("here")
library("readxl")
library("writexl")

# needed for pre-2012 imports only
library("rJava")
library("tabulizer")



# Identify files to import ----------------

setwd(here("raw_data", "idor_income_use"))

files_pdf <- list.files(pattern = ".pdf$")
files_excel <- list.files(pattern = ".xls$|.xlsx$")

dfs_out <- list()

# process excel files -----------------------

# import files
dfs_excel <- map(files_excel, read_excel, skip = 5) %>% 
  set_names(files_excel)

# Function to process excel files
# (Treats 2012 and 2013 specially)
#
# *** IMPORTANT NOTE ***
# Future year excel files should be checked to make sure the format has not evolved
clean_excel <- function(df, fy){
  
  # select and rename necessary cols
  df <- df %>% 
    select(local_gov = 1, tax_type = 2, 
           vendor_num = 3, fy_total = ncol(.))
  
  # FY12 and FY13 use the same two-row-per-entry format that must be reformatted
  if(fy %in% c(2012, 2013)){
    df <- df %>% 
      mutate_all(list(~na_if(., ""))) %>%       # convert any "" to NA
      mutate(                                   # create dummy tax_type for total row
        tax_type = ifelse(str_detect(local_gov, "TOTAL"), "TOTAL", tax_type)) %>% 
      fill(local_gov, tax_type, vendor_num) %>% # fill down descriptive column values
      filter(!is.na(fy_total))                  # remove dummy 2nd rows
  }
  
  # confirm totals and remove total row
  total_pos <- str_which(df$local_gov, "TOTAL")
  if (length(total_pos) > 2) {
    message(paste0("Table ", fy, ": Multiple total rows? Check!"))
  } else if (length(total_pos) == 0) {
    message(paste0("Table ", fy, ": No total row found. Check!"))
  } else {
    # first part of message: where is total row located?
    msg1 <- paste("Total row (@", total_pos, "of", nrow(df), "rows")
    
    # extract total row and remove it (and any footnotes) from df
    total_row <- df[total_pos,]
    df <- slice(df, 1:total_pos-1)
    
    # compare totals
    df_sum <- sum(df$fy_total)
    totalrow_sum <- total_row$fy_total
    matches <- all.equal(df_sum, totalrow_sum)
    msg2 <- if_else(matches,
                   "sum OK",
                   paste("total row and sum mismatch. Check!",
                         paste("   df sum:   ", df_sum),
                         paste("   total row:", totalrow_sum),
                         sep = "\n"
                         )
    )
    
    # return message
    message(paste("FY", fy, "|", msg1, "|", msg2))
  }
  
  df <- df %>% 
    mutate_at(vars(tax_type), as.factor) %>%   # convert tax_type into factor
    mutate(fy_year = fy)                       # add column with fy year
  

  return(df)
}

# create temporary vectors
years_num <- str_extract(files_excel, "[[:digit:]]+") %>% 
  str_c("20", .) %>% 
  as.numeric()
years_char <- str_extract(files_excel, "[[:digit:]]+") %>% 
  str_c("fy", .)

# map function across all available excel workbooks
dfs_excel_out <- map2(dfs_excel, years_num, clean_excel) %>% 
  set_names(years_char)


# remove temporary argument vectors
rm(years_num, years_char)




# import PDF files -----------------------
# ...it takes a LONG time to extract the pdfs

dfs_pdf <- map(files_pdf, extract_tables) %>% 
  set_names(files_pdf)

clean_fy11_earlier <- function(list, fy_label){
  
}


extract_tables("income_use_fy06.pdf")

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

