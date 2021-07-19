# Read in IDOR income and use data 
# Stern/Koyejo
# July 2021


# startup ----------------------------------

# packages
require(tidyverse)
require(here)
require(readxl)
require(pdftools)

# source helper functions
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
  
  # Some munis are missing unique identifiers in certain or all excel files.
  # - Wilmington: there are two Wilmingtons
  # - Windsor: New Windsor is recorded as Windsor (New Windsor) in these files
  # - Elmwood: Elmwood Park is recorded as just Elmwood in certain files.
  # These have been matched based on vendor numbers to entries in the PDFs, 
  # which do have descriptive identifiers. They are corrected here:
  df <- df %>% 
    mutate(local_gov = case_when(
      local_gov == "WINDSOR" & vendor_num == "380009420" ~ "WINDSOR (NEW WINDSOR)",
      local_gov == "WINDSOR" & vendor_num == "380009430" ~ "WINDSOR (SHELBY COUNTY)",
      local_gov == "WILMINGTON" & vendor_num == "380009390" ~ "WILMINGTON (GREENE COUNTY)",
      local_gov == "WILMINGTON" & vendor_num == "380009392" ~ "WILMINGTON (WILL COUNTY)",
      local_gov == "ELMWOOD" & vendor_num == "380002770" ~ "ELMWOOD PARK",
      TRUE ~ local_gov
    ))

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
  processed <- rm_header_footer(list, "Local Government\\s*Tax Vendor", "^$")
  
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


# combine and perform name cleaning  ---------------------------

# collapse lists, and combine into one
output <- bind_rows(
  bind_rows(dfs_pdf_out),
  bind_rows(dfs_excel_out)) %>% 
  relocate(fy_year, .before = fy_total) %>% 
  arrange(local_gov, tax_type, vendor_num, fy_year)


output <- mutate(output,
  # establish local government type: all counties are identified as such.
  local_gov_type = ifelse(str_detect(local_gov, "COUNTY GOVERNMENT$"),
                          "county",
                          "muni"),
  # remove the explicit text in local gov name to county
  local_gov = str_remove(local_gov, " COUNTY GOVERNMENT$"))

# clean up names
output$local_gov <- clean_names(output$local_gov)



# Perform some checks  ---------------------------

# confirm all rows are present
sum(map_int(dfs_excel_out, nrow)) + sum(map_int(dfs_pdf_out, nrow)) == nrow(output)

# Investigate joins using a wide version of the table, to explore name accuracy across years.
# The only rows in this table should be munis that don't collect a certain type of tax or
# were not incorporated at some point during the years of data analyzed here. There should be no 
# duplicated munis in this list, if all name corrections have been handled correctly.
output %>% 
  select(-vendor_num) %>% 
  pivot_wider(id_cols = c("local_gov", "local_gov_type"),
              names_from = c("tax_type", "fy_year"),
              values_from = "fy_total",
              names_sort = TRUE) %>% 
  filter_all(any_vars(is.na(.))) %>% 
  View("LGs missing records")

# Check against Timi's work
#
# Note that this analysis was built before the `clean_names()` function was built. 
# The join will likely only work if that function is not applied to the `local_gov`
# column.

# # New script seems to handle situations where one local gov has multiple vendor nums
# #   better. See, for example, FY2012 Wilmington and Windsor
# check <-readRDS("S:\\Projects_FY20\\Policy Development\\Tax policy analysis\\State disbursements\\Data Analysis\\data\\processed\\income_use_fy06_19_clean.RDS")
# left_join(output, check, by = c("local_gov", "tax_type", "vendor_num", "fy_year")) %>% 
#   rowwise() %>% 
#   mutate(equal = ifelse(all.equal(fy_total.x, fy_total.y), "YES", "-")) %>% 
#   View()
# 
# # Only major difference here is 2012 and 2013. 
# # New script is accurate to total rows in IDOR xls files
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


# Export finished files  ---------------------------

setwd(here("data_processed"))
write_csv(output, "idor_income_use.csv")
saveRDS(output, file = "idor_income_use.rds")





