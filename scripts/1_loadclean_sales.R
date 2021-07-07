# Read in IDOR sales tax disbursement data 
# Stern/Koyejo
# July 2021

# NOTE: This is *very* similar to 1_loadclean_income_use.R. Differences are 
#   all small specification changes and tweaks.

# startup ----------------------------------

# packages
require(tidyverse)
require(here)
require(readxl)
require(pdftools)

# source helper functions
source(here("scripts", "0_helpers.R"))

# location of input data
setwd(here("data_raw", "idor_sales"))


# process excel files -----------------------
files_excel <- list.files(pattern = ".xls$|.xlsx$")

# import files
dfs_excel <- map(files_excel, read_excel, skip = 5) %>% 
  set_names(files_excel)

# Manually insert TOTAL into last row of local gov column for FY16
dfs_excel$idor_sales_fy16.xls[nrow(dfs_excel$idor_sales_fy16.xls), 1] <- "TOTAL"

# Function to process excel files
# (Treats 2012 and 2013 specially)
#
# *** IMPORTANT NOTE ***
# Future year excel files should be checked to make sure the format has not evolved
clean_excel <- function(df, fy){
  
  # select, rename, and convert necessary cols
  df <- df %>% 
    select(local_gov = 1, tax_type = 2, 
           vendor_num = 3, fy_total = ncol(.)) %>% 
    mutate(vendor_num = as.character(vendor_num))
  
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
clean_pdf <- function(list, fy){
  
  # work through each page to remove headers and footers
  processed <- rm_header_footer(list, "Local Government\\s*Tax\\s*Vendor", "^$")
  
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
             sep = "\\s{1,}",
             fill = "right") %>% 
    # separate number fields
    mutate(values = trimws(values)) %>% 
    separate(values,
             into = c("mo1", "mo2", "mo3", "mo4", "mo5", "mo6", "fy_total"),
             sep = "\\s{1,}",
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
  bind_rows(dfs_excel_out)) %>% 
  relocate(fy_year, .before = fy_total) %>% 
  arrange(local_gov, tax_type, vendor_num, fy_year)


# confirm all rows are present
stopifnot(sum(map_int(dfs_excel_out, nrow)) + sum(map_int(dfs_pdf_out, nrow)) == nrow(output))


# export as excel workbook and RDS
setwd(here("data_processed"))
write_csv(output, "idor_sales.csv")
saveRDS(output, file = "idor_sales.rds")


# # check against Timi's work ---------------------
# #
# # New script seems to handle situations where one local gov has multiple vendor nums
# #   better. See, for example, FY2012 Wilmington and Windsor
# check <- read_excel("S:\\Projects_FY20\\Policy Development\\Tax policy analysis\\State disbursements\\Data Analysis\\data\\processed\\sales_disbursement_fy06_19_clean.xlsx")
# left_join(output, check, by = c("local_gov", "tax_type", "vendor_num", "fy_year")) %>% 
#   rowwise() %>% 
#   mutate(equal = ifelse(all.equal(fy_total.x, fy_total.y), "YES", "-")) %>% 
#   View()
# 
# # Only major difference here is 2013. New script is accurate to total row in IDOR xls file
# full_join(
#   output %>% 
#     group_by(fy_year) %>% 
#     summarize(fy_total = sum(fy_total)),
#   check %>% 
#     group_by(fy_year) %>% 
#     summarize(fy_total = sum(fy_total)),
#   by = "fy_year",
#   suffix = c(".m", ".t")
# ) %>% 
#   mutate(dif = fy_total.m - fy_total.t)
# 

