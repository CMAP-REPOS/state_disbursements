# Read in MFT disbursement data 
# Stern/Koyejo
# July 2021


# startup ----------------------------------

# packages
require(tidyverse)
require(here)
require(readxl)
require(pdftools)
require(scales)

# source helper functions
source(here("scripts", "0_helpers.R"))

# location of input data
setwd(here("data_raw", "idot_mft"))


# import PDF files -----------------------
files <- list.files(pattern = ".pdf$")

# import all (not actually dfs, yet - a list of lists)
dfs_pdf <- map(files, ~(str_split(pdf_text(.), "\n")))

# remove last page from each import - this is just a page with totals
last_page_to_keep <- map_int(dfs_pdf, length) - 1
dfs_pdf <- map2(dfs_pdf, last_page_to_keep, head)

# function for interpreting pdfs
clean_pdf <- function(list, fy){
  
  # work through each page to remove headers and footers. 
  processed <- rm_header_footer(list, "Agency Name\\s*Local Agency Type|Agency Name\\s*LA Type|For Fiscal Year ", "^$")
  
  table <- processed %>% 
    unlist() %>%     # collapse to single vector of rows
    as_tibble() %>%  # convert this character vector to a table with 1 col ("value")
    mutate(value = trimws(value))  # drop leading and trailing white space
  
  # this next command varies based on 2020 format
  #
  # *** IMPORTANT NOTE ***
  # Future year PDFs  should be checked to make sure the format has not evolved
  if (fy >= 2020){
    table <- table %>%  
      separate(value,  # separate value column into multiple columns
               into = c("local_gov", "local_gov_type", NA, NA, "fy_total"), # DROP MFT AND TRF COLUMNS, KEEP TOTAL ONLY
               sep = " \\s{2,}", # separate anywhere where at least 2 spaces exist
               fill = "left")    # leave empty cells on the left (for empty "agency" and "local_gov_type") 
  } else {
    table <- table %>%  
      separate(value,  # separate value column into multiple columns
               into = c("local_gov", "local_gov_type", "fy_total"),
               sep = " \\s{2,}", # separate anywhere where at least 2 spaces exist
               fill = "left")    # leave empty cells on the left (for empty "agency" and "local_gov_type")
  }
  
  table <- table %>% 
    mutate(fy_total = parse_number(fy_total)) %>% 
    # drop totals rows
    filter(!is.na(local_gov_type)) %>% 
    # give type meaning
    mutate(local_gov_type = recode_factor(local_gov_type,
                                          `1` = "county",
                                          `2` = "township",
                                          `3` = "municipality")) %>% 
    # create county column
    mutate(county = case_when(local_gov_type == "county" ~ local_gov),
           .after = local_gov_type) %>% 
    fill(county) %>% 
    arrange(local_gov_type, local_gov, county) %>% 
    mutate(fy_year = fy) 
  
  # print totals for manual verification
  msg <- table %>% 
    group_by(local_gov_type) %>% 
    summarise(fy_total = sum(fy_total)) %>% 
    mutate(
      local_gov_type = as.character(local_gov_type),
      fy_total = dollar_format()(fy_total)) %>% 
    paste(collapse = ", ")
  
  message(paste0("FY", fy, ": ", msg))
  
  return(table)
}

# create temporary vector
years_num <- str_extract(files, "[[:digit:]]+") %>% 
  str_c("20", .) %>% 
  as.numeric()

# Map function across all available excel workbooks
# 
# this will print messages containing summed totals by type for each year
# for manual checking against the last page of each imported PDF.
dfs_pdf_out <- map2(dfs_pdf, years_num, clean_pdf)

# remove temporary argument vector
rm(years_num, last_page_to_keep)


# combine and export  ---------------------------

# collapse lists, combine into one
output <- bind_rows(dfs_pdf_out) %>% 
  relocate(fy_year, .before = fy_total) %>% 
  arrange(local_gov, local_gov_type, county, fy_year)

# clean up names
output$local_gov <- clean_names(output$local_gov)
output$county <- clean_names(output$county)

# Sum fy_total by unique local_gov, local_gov_type, county, fy_year to
# resolve issue where Rocky Run and Wilcox Twps were separate until mid-2017
output <- output %>%
  group_by(local_gov, local_gov_type, county, fy_year) %>%
  summarize(fy_total = sum(fy_total), .groups = "drop")

# confirm all rows are present
stopifnot(sum(map_int(dfs_pdf_out, nrow)) == nrow(output))

# Investigate joins using a wide version of the table, to explore name accuracy across years.
# The only rows in this table should be munis that don't collect a certain type of tax or
# were not incorporated at some point during the years of data analyzed here. There should be no 
# duplicated munis in this list, if all name corrections have been handled correctly.
output %>%
  pivot_wider(id_cols = c("local_gov", "local_gov_type", "county"),
              names_from = "fy_year",
              values_from = "fy_total",
              names_prefix = "fy",
              names_sort = TRUE) %>%
  filter_all(any_vars(is.na(.))) %>%
  View("LGs missing records")



# # check against Timi's work ---------------------
# 
# check <- read_excel("S:\\Projects_FY20\\Policy Development\\Tax policy analysis\\State disbursements\\Data Analysis\\data\\processed\\mft_data_05_19.xlsx") %>% 
#   mutate(local_gov_type = recode_factor(local_gov_type, COUNTY = "county", TOWNSHIP = "township", MUNICIPALITY = "municipality"),
#          agency_name = str_to_title(agency_name))
# 
# # this all looks pretty good
# full_join(output, check, by = c("local_gov" = "agency_name", "local_gov_type" = "local_gov_type", "county", "fy_year")) %>% 
#   rowwise() %>% 
#   mutate(equal = ifelse(all.equal(fy_total.x, fy_total.y), "YES", "-")) %>% 
#   View()
# 
# 
# # New table has around $500K more disbursement captured in each year than the old
# # analysis. Manual checks against PDF sums indicate new number is right.
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

# export as excel workbook and RDS
setwd(here("data_processed"))
write_csv(output, "idot_mft.csv")
saveRDS(output, file = "idot_mft.rds")
