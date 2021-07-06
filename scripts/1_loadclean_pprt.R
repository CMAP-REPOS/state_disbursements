# Read in IDOR PPRT disbursement data 
# Author: Matt Stern, July 2021
# Building on work by Timi Koyejo, Nov 2019

# NOTE: This is *very* similar to 1_loadclean_income_use.R.

# startup ----------------------------------

# packages
library(tidyverse)
library(here)
library(readxl)
library(writexl)

# source helper functions
source(here("scripts", "0_helpers.R"))

# location of input data
setwd(here("data_raw", "idor_pprt"))


# process excel files -----------------------
files <- list.files(pattern = ".xls$|.xlsx$")

# there's a bit more header in FY18 and FY19 spreadsheets
skiprows <- recode(files, 
                   "pprt_fy18.xls" = 6,
                   "pprt_fy19.xls" = 6,
                   .default = 5)


# import files
dfs <- map2(files, skiprows, ~(read_excel(path = .x, skip = .y))) %>% 
  set_names(files)

# Function to process excel files
#
# *** IMPORTANT NOTE ***
# Future year excel files should be checked to make sure the format has not evolved
clean_excel <- function(df, fy){
  
  # select and rename necessary cols
  df <- df %>% 
    select(local_gov = 1, district_num = 2, 
           vendor_num = 3, fy_total = ncol(.))
  
  # reformat two-row-per-entry format
  df <- df %>% 
    mutate_all(list(~na_if(., ""))) %>%       # convert any "" to NA
    fill(local_gov, district_num, vendor_num) %>% # fill down descriptive column values
    filter(!is.na(fy_total))                  # remove dummy 2nd rows
  
  # check and remove totals with helper fn
  df <- total_check_extract(df, "local_gov", "fy_total", paste0("FY", fy))
  
  df <- df %>% 
    mutate(fy_year = fy)                       # add column with fy year
  
  return(df)
}

# create temporary vector
years_num <- str_extract(files, "[[:digit:]]+") %>% 
  str_c("20", .) %>% 
  as.numeric()

# map function across all available excel workbooks
dfs_out <- map2(dfs, years_num, clean_excel)


# combine and export  ---------------------------

# collapse lists, combine into one, and calculate local gov types
# gov types from Timi's work, which included these references:
# - list of IL local governments https://illinoiscomptroller.gov/financial-data/local-government-division/types-of-local-governments-in-illinois/
# - intro to IL special districts http://www.ilga.gov/commission/lru/specialdistricts.pdf
output <- bind_rows(dfs_out) %>% 
  relocate(fy_year, .before = fy_total) %>% 
  arrange(local_gov, district_num, vendor_num, fy_year) %>% 
  mutate(district_local_gov_id = str_sub(district_num, 4, 6)) %>%  # extract 4th - 6th digit from district num
  mutate(local_gov_type = case_when(district_local_gov_id == "101" ~ "County",
                                    district_local_gov_id == "240" ~ "Municipality",
                                    district_local_gov_id == "302" ~ "Township",
                                    district_local_gov_id == "508" ~ "Park District",
                                    district_local_gov_id == "509" ~ if_else(district_num %in% c("0165090025", "0455090010", "0455090042", 
                                                                                                 "0575090017", "1015090123"),
                                                                             "Water Reclaim District",
                                                                             "Sanitary District"),
                                    district_local_gov_id == "510" ~ "Fire Protection District",
                                    district_local_gov_id == "511" ~ "Public Health District",
                                    district_local_gov_id == "512" ~ "Hospital District",
                                    district_local_gov_id == "513" ~ "TB Sanatorium District",
                                    district_local_gov_id == "514" ~ "Mosquito Abatement District",
                                    district_local_gov_id == "515" ~ "Airport Authority District",
                                    district_local_gov_id == "516" ~ "Public Library District",
                                    district_local_gov_id == "518" ~ "Water Authority District",
                                    district_local_gov_id == "519" ~ "Cemetery District",
                                    district_local_gov_id == "520" ~ "Forest Preserve District",
                                    district_local_gov_id == "521" ~ "Street Lighting District",
                                    district_local_gov_id == "523" ~ "Mass Transit District", 
                                    district_local_gov_id == "530" ~ if_else(district_num %in% c("0225300025", "0825300022"), # subclassify WESTMONT 1 SURFACE WATER DIST and MASCOUTAH SURFACE WTR DISTRICT as Surface Water
                                                                             "Surface Water District",
                                                                             "River Conservancy District"), # classifying the FULTON FLOOD DIST as River Conservancy for this purpose
                                    district_local_gov_id == "531" ~ "Soil and Water Conservation District", # SWCD
                                    district_local_gov_id == "532" ~ "Conservation District", # Combines a small number of related districts that have same code
                                    district_local_gov_id == "606" ~ "Community College District",
                                    district_local_gov_id %in% c("701", "702", "703", "704", "705") ~ "Elementary School District",
                                    district_local_gov_id %in% c("713", "716", "717", "718", "719") ~ "High School District", # 713 is Cosolidated High School, 716
                                    # some context on what the school codes mean, see "Type Code" https://www.isbe.net/Documents/key_codes.pdf
                                    district_local_gov_id %in% c("722", "724", "725", "726", "727") ~ "Unit School District",
                                    district_local_gov_id == "902" ~ "Road & Bridge District",
                                    TRUE ~ "Other" ))

# confirm all rows are present
sum(map_int(dfs_out, nrow)) == nrow(output)


# export as excel workbook and RDS
setwd(here("data_processed"))
write_xlsx(output, path = "idor_pprt.xlsx")
saveRDS(output, file = "idor_pprt.rds")



# check against Timi's work ---------------------

check <- read_excel("S:\\Projects_FY20\\Policy Development\\Tax policy analysis\\State disbursements\\Data Analysis\\data\\processed\\pprt_fy07_19_clean.xlsx")

# Only difference here is 2019, which is minor. New script is accurate to total row in IDOR xls file
full_join(
  output %>% 
    group_by(fy_year) %>% 
    summarize(fy_total = sum(fy_total)),
  check %>% 
    group_by(fy_year) %>% 
    summarize(fy_total = sum(fy_total)),
  by = "fy_year",
  suffix = c(".m", ".t")
) %>% 
  mutate(dif = fy_total.m - fy_total.t)

# this all looks pretty good
left_join(output, check, by = c("local_gov" = "district_name", "district_num", "vendor_num", "fy_year")) %>% 
  rowwise() %>% 
  mutate(equal = ifelse(all.equal(fy_total.x, fy_total.y), "YES", "-")) %>% 
  View()

