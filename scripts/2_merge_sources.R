# merge datasets

require(tidyverse)
require(here)
require(tidycensus)

setwd(here("data_processed"))

# a function to create a lowercase name without spaces or punctuation
create_namejoin <- function(names_vector){
  names_vector %>% 
    str_to_lower() %>% 
    str_remove_all("[^[[:alnum:]]]")
}


# Create core list of local governments ---------------------


# Municipalities
pop_place <- get_estimates(geography = "place",
                           variables = "POP",
                           year = 2019,
                           state = 17) %>% 
  # clean up names
  mutate(local_gov = str_remove(NAME, ", Illinois$"),
         muni_type = str_extract(local_gov, "village$|town$|city$"),
         local_gov = str_remove(local_gov, " village$| town$| city$"),
         local_gov_type = "muni") %>%
  # make some name corrections
  mutate(local_gov = case_when(
    local_gov == "Alvan" ~ "Alvin",
    local_gov == "Bureau Junction" ~ "Bureau",
    local_gov == "Windsor" & muni_type == "village" ~ "Windsor (New Windsor)",
    local_gov == "Windsor" & muni_type == "city" ~ "Windsor (Shelby County)",
    local_gov == "Wilmington" & muni_type == "village" ~ "Wilmington (Greene County)",
    local_gov == "Wilmington" & muni_type == "city" ~ "Wilmington (Will County)",
    TRUE ~ local_gov
  )) %>% 
  select(local_gov, local_gov_type, muni_type, id = GEOID, pop = value)


# Counties
pop_county <- get_estimates(geography = "county",
                            variables = "POP",
                            year = 2019,
                            state = 17) %>% 
  # clean up names
  mutate(local_gov = str_remove(NAME, " County, Illinois"),
         local_gov_type = "county") %>% 
  select(local_gov, local_gov_type, id = GEOID, pop = value)


# School districts
pop_schooldists <- readRDS("isbe_ilearn.rds") %>% 
  select(county, `district number`, fy, `district name`, `avg daily attend`) %>% 
  arrange(`district number`, fy) %>% 
  group_by(`district number`) %>% 
  summarize(county = first(county),
            fy_min = min(fy),
            fy_max = max(fy),
            name = last(`district name`),
            names = paste(unique(`district name`), collapse = ","),
            pop = last(`avg daily attend`)) %>% 
  mutate(local_gov_type = "school district") %>% 
  select(local_gov = name, local_gov_type, id = `district number`, pop)




core <- bind_rows(pop_place, pop_county) %>% 
  bind_rows(pop_schooldists) %>% 
  mutate(join_name = create_namejoin(local_gov)) %>% 
  arrange(local_gov)



# Income and use data
idor_income_use <- readRDS("idor_income_use.rds") 

# investigate joins using a wide version of the table, to explore name accuracy across years
idor_income_use %>% 
  select(-vendor_num) %>% 
  pivot_wider(id_cols = c("local_gov", "local_gov_type"),
              names_from = c("tax_type", "fy_year"),
              values_from = "fy_total",
              names_sort = TRUE) %>% 
  filter_all(any_vars(is.na(.))) %>% 
  View("LGs missing records")

# join a wide version of the table to the census names to explore match accuracy
# only missings are 3 munis that have disincorporated. these will be dropped.
idor_income_use %>% 
  select(-vendor_num) %>% 
  pivot_wider(id_cols = c("local_gov", "local_gov_type"),
              names_from = c("tax_type", "fy_year"),
              values_from = "fy_total",
              names_sort = TRUE) %>% 
  select(local_gov, local_gov_type, INC_2019) %>%
  mutate(in_idor_tbl = "yes",
         join_name = local_gov %>% 
           str_to_lower() %>% 
           str_remove_all("[^[[:alnum:]]]")) %>% 
  full_join(core, by = c("join_name", "local_gov_type")) %>%
  filter((is.na(in_idor_tbl) & local_gov_type != "school district")|is.na(id)) %>% 
  View()



# next step do final join on long data

# will also need to add clean names methods to other import tools.