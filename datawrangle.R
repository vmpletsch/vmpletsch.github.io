library(tidyverse)
library(dplyr)
#combine all data into a master doc

#load each dataset
fertility <- read.csv("fertility.csv")
religion <- read.csv("religion.csv")
education <- read.csv("education.csv")
ppp <- read.csv("ppp.csv")

#pivot fertility dataset
fertility <- fertility %>%
  pivot_longer(
    cols = starts_with("X"),      # Select all columns that start with 'X'
    names_to = "year",            # The column names (X1800, etc.) go here
    names_prefix = "X",           # This removes the 'X' from the names
    values_to = "babies_per_woman" # The cell values (7.0, 6.93, etc.) go here
  ) %>%
  # Convert year to numeric so your Shiny slider can read it
  mutate(year = as.numeric(year))


#pivot education dataset
education <- education %>%
  pivot_longer(
    cols = starts_with("X"),      # Select all columns that start with 'X'
    names_to = "year",            # The column names (X1800, etc.) go here
    names_prefix = "X",           # This removes the 'X' from the names
    values_to = "mean_years" # The cell values (7.0, 6.93, etc.) go here
  ) %>%
  # Convert year to numeric so your Shiny slider can read it
  mutate(year = as.numeric(year))


#pivot ppp dataset
ppp <- ppp %>%
  pivot_longer(
    cols = starts_with("X"),      # Select all columns that start with 'X'
    names_to = "year",            # The column names (X1800, etc.) go here
    names_prefix = "X",           # This removes the 'X' from the names
    values_to = "income_ppp" # The cell values (7.0, 6.93, etc.) go here
  ) %>%
  # Convert year to numeric so your Shiny slider can read it
  mutate(year = as.numeric(year))

#pivot religion dataset
religion <- religion %>% 
  select(-income_groups, -is..country, -iso3166_1_alpha2, 
         -unicode_region_subtag, -iso3166_1_alpha2, -iso3166_1_alpha3, -iso3166_1_numeric,
         -iso3166_2, -landlocked, -un_state, -world_4region, -world_6region, -unicef_region,
         -un_sdg_ldc, -unhcr_region, -west_and_rest, -income_3groups)

religion2 <- religion %>% 
  select(-income_3groups, -un_sdg_region)

religion2 <- religion2 %>%
  rename(
    geo = country
    
  )
#combine all datasets into 1

master_data <- fertility %>%
  left_join(ppp, by = c("geo", "year")) %>%
  left_join(education, by = c("geo", "year")) %>%
  left_join(religion2, by = "geo")


#remove excess names

master_data <- master_data %>%
  select(-name.y, -name.x.x, -name.y.y)



#call new dataset on gender equality 
gender <- read.csv("gendereq_idea.csv")

gender <- gender %>%
  pivot_longer(
    cols = starts_with("X"),      # Select all columns that start with 'X'
    names_to = "year",            # The column names (X1800, etc.) go here
    names_prefix = "X",           # This removes the 'X' from the names
    values_to = "idea" # The cell values (7.0, 6.93, etc.) go here
  ) %>%
  # Convert year to numeric so your Shiny slider can read it
  mutate(year = as.numeric(year))

#combine with master_data 
master_data <- master_data %>%
  left_join(gender, by = c("geo", "year"))

master_data <- master_data %>%
  select(-name)

#save as new csv 

gapminder_master <- gapminder_master %>%
  select(-name)

write_csv(gapminder_master, "gapminder_master.csv")

gapminder_master <- read.csv("gapminder_master.csv")
