#### LOAD LIBRARIES

library(tidycensus)
library(tidyverse)

#### PREP

# View variable names (for reference)
load_variables(2000,dataset = "sf1", cache = T) %>% View()
load_variables(2019,dataset = "acs1", cache = T) %>% View()

# FIPS codes for seven county CMAP region
cmap_seven_counties <- c("31","43","89","93","97","111","197")

# Create list of age variables (used in 2000 and 2010 census)
age_2000 <- 
  c(rep("P01",49)) %>% 
  # Append the individual variable IDs, which run from 1 to 49
  paste0(seq(2001,2049,1))

# 2010 used the same convention. 2020 is expected to use the same convention
age_2010 <- age_2000
age_2020 <- age_2000

# Create list of ACS convention age variables
age_2019 <-
  c(rep("B01001_",49)) %>% 
  # Append the individual variable IDs
  paste0(sprintf("%03d",seq(1,49,1)))

# Create helper recoding for age buckets
age_recode <- c("1" = "Total",
                "2" = "Total",
                "3" = "Under 5",
                "4" = "5 to 9",
                "5" = "10 to 14",
                "6" = "15 to 17",
                "7" = "18 to 19",
                "8" = "20",
                "9" = "21",
                "10" = "22 to 24",
                "11" = "25 to 29",
                "12" = "30 to 34",
                "13" = "35 to 39",
                "14" = "40 to 44",
                "15" = "45 to 49",
                "16" = "50 to 54",
                "17" = "55 to 59",
                "18" = "60 to 61",
                "19" = "62 to 64",
                "20" = "65 to 66",
                "21" = "67 to 69",
                "22" = "70 to 74",
                "23" = "75 to 79",
                "24" = "80 to 84",
                "25" = "85 and over")

#### PULL POPULATION AGE DISTRIBUTION BY COUNTY

age_distro_2000 <-
  get_decennial(year = 2000,
                geography = "county",
                variables = age_2000,
                state = "17",
                county = cmap_seven_counties) %>% 
  # Add flag to enable summing of male and female totals by age
  mutate(id = as.numeric(substr(variable,6,7))) %>% 
  mutate(id = case_when(
    id >= 26 ~ id - 24,
    TRUE ~ id
  )) %>% 
  # Sum and rename
  group_by(GEOID,id) %>% 
  mutate(total = sum(value)) %>% 
  mutate(age = recode(id,!!!age_recode)) %>% 
  ungroup() %>% 
  select(GEOID,NAME,age,total) %>% 
  distinct()

age_distro_2010 <-
  get_decennial(year = 2010,
                geography = "county",
                variables = age_2010,
                state = "17",
                county = cmap_seven_counties) %>% 
  # Add flag to enable summing of male and female totals by age
  mutate(id = as.numeric(substr(variable,6,7))) %>% 
  mutate(id = case_when(
    id >= 26 ~ id - 24,
    TRUE ~ id
  )) %>% 
  # Sum and rename
  group_by(GEOID,id) %>% 
  mutate(total = sum(value)) %>% 
  mutate(age = recode(id,!!!age_recode)) %>% 
  ungroup() %>% 
  select(GEOID,NAME,age,total) %>% 
  distinct()

# Note - the sample sizes for age distribution are low in outer counties for the
# 1-year ACS. Have used 5-year for that reason.
age_distro_2019 <-
  get_acs(year = 2019,
          geography = "county",
          survey = "acs5",
          variables = age_2019,
          state = "17",
          county = cmap_seven_counties) %>% 
  # Add flag to enable summing of male and female totals by age
  mutate(id = as.numeric(substr(variable,9,11))) %>% 
  mutate(id = case_when(
    id >= 26 ~ id - 24,
    TRUE ~ id
  )) %>% 
  # Sum and rename
  group_by(GEOID,id) %>% 
  mutate(total = sum(estimate)) %>% 
  mutate(age = recode(id,!!!age_recode)) %>% 
  ungroup() %>% 
  select(GEOID,NAME,age,total) %>% 
  distinct()
  
# This should work as of 9/16
age_distro_2020 <-
  get_decennial(year = 2020,
                geography = "county",
                variables = age_2010,
                state = "17",
                county = cmap_seven_counties) %>% 
  # Add flag to enable summing of male and female totals by age
  mutate(id = as.numeric(substr(variable,6,7))) %>% 
  mutate(id = case_when(
    id >= 26 ~ id - 24,
    TRUE ~ id
  )) %>% 
  # Sum and rename
  group_by(GEOID,id) %>% 
  mutate(total = sum(value)) %>% 
  mutate(age = recode(id,!!!age_recode)) %>% 
  ungroup() %>% 
  select(GEOID,NAME,age,total) %>% 
  distinct()


#### PULL HOUSEHOLDS BY COUNTY

hh_2000 <-
  get_decennial(year = 2000,
                geography = "county",
                variables = "H006001",
                state = "17",
                county = cmap_seven_counties)

hh_2010 <-
  get_decennial(year = 2010,
                geography = "county",
                variables = "H006001",
                state = "17",
                county = cmap_seven_counties)

hh_2019 <-
  get_acs(year = 2019,
          geography = "county",
          variables = "B25006_001",
          survey = "acs1",
          state = "17",
          county = cmap_seven_counties)

# This should work as of 9/16
hh_2020 <-
  get_decennial(year = 2020,
                geography = "county",
                variables = "H006001",
                state = "17",
                county = cmap_seven_counties)

#### PULL HOUSING UNITS

hu_2000 <-
  get_decennial(year = 2000,
                geography = "county",
                variables = "H001001",
                state = "17",
                county = cmap_seven_counties)

hu_2010 <-
  get_decennial(year = 2010,
                geography = "county",
                variables = "H001001",
                state = "17",
                county = cmap_seven_counties)

hu_2019 <-
  get_acs(year = 2019,
          geography = "county",
          variables = "B25001_001",
          survey = "acs1",
          state = "17",
          county = cmap_seven_counties)

# This should work as of 9/16
hu_2020 <-
  get_decennial(year = 2020,
                geography = "county",
                variables = "H001001",
                state = "17",
                county = cmap_seven_counties)
