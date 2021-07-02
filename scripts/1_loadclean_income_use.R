# Read in IDOR income and use data 
# Author: Matt Stern, July 2021
# Building on work by Timi Koyejo, Nov 2019


# startup ----------------------------------

# packages
library(tidyverse)
library(here)
library(readxl)
library(writexl)
library(pdftools)

# source helper functions (`total_check_extract()` needed here)
source(here("scripts", "0_helpers.R"))

# location of input data
setwd(here("data_raw", "idor_income_use"))


# process excel files -----------------------
files_excel <- list.files(pattern = ".xls$|.xlsx$")

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
  
  # check and remove totals with helper fn
  df <- total_check_extract(df, "local_gov", "fy_total", paste0("FY", fy))
  
  df <- df %>% 
    mutate_at(vars(tax_type), as.factor) %>%   # convert tax_type into factor
    mutate(fy_year = fy)                       # add column with fy year
  

  return(df)
}

# create temporary vector
years_num <- str_extract(files_excel, "[[:digit:]]+") %>% 
  str_c("20", .) %>% 
  as.numeric()

# map function across all available excel workbooks
dfs_excel_out <- map2(dfs_excel, years_num, clean_excel)


# remove temporary argument vectors
rm(years_num)


# import PDF files -----------------------
files_pdf <- list.files(pattern = ".pdf$")

# import all (not actually dfs, yet - a list of lists)
dfs_pdf <- map(files_pdf, ~(str_split(pdf_text(.), "\n")))

# function for interpreting pdfs
clean_pdf <- function(raw, fy){
  
  # work through each page to remove headers and footers
  processed <- list()
  for (i in seq.int(length(raw))) {
    # identify the row that contains the column headers, and remove rows up through it.
    header_row <- str_which(raw[[i]], "Local Government\\s*Tax Vendor")
    processed[[i]] <- raw[[i]][-(seq.int(header_row))] 
    
    # identify the first blank row, and remove it and all rows after.
    footer_row <- str_which(processed[[i]], "^$")[1]
    processed[[i]] <- processed[[i]][-seq(footer_row, length(processed[[i]]))]
  }
  
  table <- processed %>% 
    unlist() %>%     # collapse to single vector of rows
    as_tibble() %>%  # convert this character vector to a table with 1 col ("value")
    mutate(value = trimws(value)) %>% # drop leading and trailing white space
    # split into three columns based on fixed value of date columns
    extract(value,   
            into = c("lg_tax_vendor", "date", "values"),
            regex = "(.*(?=JUL - DEC)|.*(?=JAN - JUN))(JUL - DEC|JAN - JUN)(.*)") %>% 
    # separate local government
    separate(lg_tax_vendor,
             into = c("local_gov", "tax_vendor"),
             sep = "\\s{2,}",
             extra = "merge",
             fill = "right") %>% 
    # separate tax_type and vendor_num
    mutate(tax_vendor = trimws(tax_vendor)) %>% 
    separate(tax_vendor,
             into = c("tax_type", "vendor_num"),
             sep = "\\s",
             fill = "right") %>% 
    # separate number fields
    mutate(values = trimws(values)) %>% 
    separate(values,
             into = c("mo1", "mo2", "mo3", "mo4", "mo5", "mo6", "fy_total"),
             sep = "\\s{2,}",
             fill = "right") %>% 
    # convert currencies to numbers
    mutate_at(c("mo1", "mo2", "mo3", "mo4", "mo5", "mo6", "fy_total"), parse_number) %>% 
    # create dummy tax_type for total row
    mutate(tax_type = ifelse(str_detect(local_gov, "TOTAL"), "TOTAL", tax_type)) %>% 
    # convert any "" to NA
    mutate_all(list(~na_if(., ""))) %>%
    # fill down descriptive column values
    fill(local_gov, tax_type, vendor_num) %>% 
    # remove dummy 2nd rows
    filter(!is.na(fy_total)) %>% 
    # simplify and add date
    select(local_gov, tax_type, vendor_num, fy_total) %>% 
    mutate(fy_year = fy) 
  
  # remove totals row, checking to make sure it sums correctly
  table <- total_check_extract(table, "local_gov", "fy_total", paste0("FY", fy))
  
  return(table)
}

# create temporary vector
years_num <- str_extract(files_pdf, "[[:digit:]]+") %>% 
  str_c("20", .) %>% 
  as.numeric()

# map function across all available excel workbooks
dfs_pdf_out <- map2(dfs_pdf, years_num, clean_pdf)

# remove temporary argument vector
rm(years_num)


# combine and export  ---------------------------

# collapse lists, and combine into one
output <- bind_rows(
  bind_rows(dfs_pdf_out),
  bind_rows(dfs_excel_out)
)

# confirm all rows are present
sum(map_int(dfs_excel_out, nrow)) + sum(map_int(dfs_pdf_out, nrow)) == nrow(output)


# export as excel workbook and RDS
setwd(here("data_processed"))
write_xlsx(output, path = "idor_income_use.xls")
saveRDS(output, file = "idor_income_use.rds")


# check against Timi's work ---------------------
#
# - New script seems to handle situations where one local gov has multiple vendor nums
#   better. See, for example, FY2012 Wilmington and Windsor
check <-readRDS("S:\\Projects_FY20\\Policy Development\\Tax policy analysis\\State disbursements\\Data Analysis\\data\\processed\\income_use_fy06_19_clean.RDS")
left_join(output, check, by = c("local_gov", "tax_type", "vendor_num", "fy_year")) %>% 
  rowwise() %>% 
  mutate(equal = ifelse(all.equal(fy_total.x, fy_total.y), "YES", "-")) %>% 
  View()

