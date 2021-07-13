# merge datasets

require(tidyverse)
require(here)
require(tidycensus)

setwd(here("data_processed"))


# Create core list of local governments
pop_place <- get_estimates(geography = "place",
                           variables = "POP",
                           year = 2019,
                           state = "IL")

pop_county <- get_estimates(geography = "county",
                            variables = "POP",
                            year = 2019,
                            state = "IL")

pop_schooldists <- readRDS("isbe_ilearn.rds") %>% 
  select(county, `district number`, fy, `district name`, `avg daily attend`) %>% 
  arrange(`district number`, fy) %>% 
  group_by(`district number`) %>% 
  summarize(county = first(county),
            fy_min = min(fy),
            fy_max = max(fy),
            name = last(`district name`),
            names = paste(unique(`district name`), collapse = ","),
            pop = last(`avg daily attend`))




core <- bind_rows(
  #municipalities
  pop_place %>%
    mutate(local_gov = str_remove(NAME, ", Illinois$"),
           local_gov_type = str_extract(local_gov, "village$|town$|city$"),
           local_gov = str_remove(local_gov, " village$| town$| city$")) %>% 
    select(local_gov, local_gov_type, id = GEOID, pop = value),
  #counties
  pop_county %>% 
    mutate(local_gov = str_remove(NAME, " County, Illinois"),
           local_gov_type = "county") %>% 
    select(local_gov, local_gov_type, id = GEOID, pop = value)
) %>% 
  bind_rows(
    # school districts
    pop_schooldists %>% 
      mutate(local_gov_type = "school district") %>% 
      select(local_gov = name, local_gov_type, id = `district number`, pop)
  ) %>% 
  arrange(local_gov)



# Income and use data
idor_income_use <- readRDS("idor_income_use.rds") %>% 
  mutate(local_gov_type = ifelse(str_detect(local_gov, "COUNTY GOVERNMENT$"),
                                 "county",
                                 "muni"),
         local_gov = str_remove(local_gov, "COUNTY GOVERNMENT$")
  )

