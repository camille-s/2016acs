# merge neighborhood profiles from block group, tract based files
library(tidyverse)
library(forcats)
library(XLConnect)

# rm(list = ls())

year <- 2016

prof_bg <- read_csv(paste(year, "neighborhood extended (BG).csv"))
prof_tract <- read_csv(paste(year, "neighborhood extended (tract).csv"))
# make indicators a factor, bc spread will convert them to factor and put levels in alphabetical order
# convert back into character to clean up labels
# want to squeeze in titles for indicators--use select to put blank title columns in place
blanks <- rep(NA, nrow(prof_bg))
titles <- data.frame(
  title_pop = blanks,
  title_race = blanks,
  title_hh_type = blanks,
  title_hh_family = blanks,
  title_occupancy = blanks,
  title_disconnect = blanks,
  title_edu = blanks,
  title_foreign = blanks,
  title_language = blanks,
  title_employment = blanks,
  title_commute = blanks,
  title_working_parent = blanks,
  title_occupation = blanks,
  title_income = blanks,
  title_poverty = blanks,
  title_vehicle = blanks,
  title_home_val = blanks,
  title_own_cost = blanks,
  title_rent_cost = blanks,
  title_all_cost = blanks
)
prof_df <- inner_join(prof_bg, prof_tract, by = "name") %>%
  bind_cols(titles) %>%
  select(name, title_pop, num_total_pop.x:per_age65plus, title_race, num_total_pop.1:per_other_race,
         title_hh_type, num_households:per_family_hh, title_hh_family, num_family_hh, 
         num_family_hh2:per_female_hh_kids,
         title_occupancy, num_total_units:per_renter_hh, title_disconnect, 
         num_ages16_19:per_disconnected, title_edu, num_age25plus:per_bach_plus,
         title_foreign, num_total_pop.y:per_not_citizen, title_language,
         num_age5plus:per_spanish_low_english, title_employment, num_age16plus:per_unemployment_rate,
         title_commute, num_workers:per_other_transit, title_working_parent, 
         num_children_in_fams:per_children_all_parents_work, title_occupation, 
         num_employed.1:per_production, title_income, num_households.1:per_fam_inc_200plus,
         title_poverty, num_pov_determined:per_2xfpl, num_under6_pov_status:per_over65_poverty,
         num_families.1:per_family_poverty, title_vehicle,
         num_occupied_units:per_1more_occupants, title_home_val, num_owned_units:per_val_300plus,
         title_own_cost, num_owned_units.1:per_owned_cost50, title_rent_cost,
         num_rented_units:per_rented_cost50, title_all_cost, num_units:per_units_cost50) %>%
  gather(key = indicator, value = value, -name) %>%
  mutate(type = str_extract(indicator, "^([a-z]+)"),
         indicator = ifelse(str_detect(indicator, "title"), 
                            indicator, 
                            str_sub(indicator, 5)) %>% factor(., levels = unique(.))) %>%
  mutate(heading = paste(name, type)) %>%
  select(-type, -name) %>%
  spread(key = heading, value = value) %>%
  select(-ends_with("title")) %>%
  select(1:7, 10:21, 24:25, 22:23, 28:29, 32:45, 30:31, 26:27, 8:9)


# write to Excel template
excel <- loadWorkbook("excel_template.xlsx")
setStyleAction(excel, XLC$STYLE_ACTION.NONE)
writeNamedRegion(excel, prof_df %>% select(-indicator), name = "profile_data", header = F)
saveWorkbook(excel)

