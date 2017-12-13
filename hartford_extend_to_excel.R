library(tidyverse)
library(XLConnect)

year <- 2016

prof_df <- read_csv(sprintf("%s Hartford neighborhood extended.csv", year))

# add blank columns for titles in excel sheet
blanks <- rep(NA, nrow(prof_df))
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
prof_xl <- prof_df %>%
	bind_cols(titles) %>%
	select(name, town, title_pop, num_total_pop:per_age65plus, title_race, num_total_pop.1:per_other_race,
				 title_hh_type, num_households:per_family_hh, title_hh_family, num_family_hh, 
				 num_family_hh2:per_female_hh_kids,
				 title_occupancy, num_total_units:per_renter_hh, title_disconnect, 
				 num_ages16_19:per_disconnected, title_edu, num_age25plus:per_bach_plus,
				 title_foreign, num_total_pop.2:per_not_citizen, title_language,
				 num_age5plus:per_spanish_low_english, title_employment, num_age16plus:per_unemployment_rate,
				 title_commute, num_workers:per_other_transit, title_working_parent, 
				 num_children_in_fams:per_children_all_parents_work, title_occupation, 
				 num_employed.1:per_production, title_income, num_households.1:per_fam_inc_200plus,
				 title_poverty, num_pov_determined:per_2xfpl, num_under6_pov_status:per_over65_poverty,
				 num_families.1:per_family_poverty, title_vehicle,
				 num_occupied_units:per_1more_occupants, title_home_val, num_owned_units:per_val_300plus,
				 title_own_cost, num_owned_units.1:per_owned_cost50, title_rent_cost,
				 num_rented_units:per_rented_cost50, title_all_cost, num_units:per_units_cost50) %>%
	gather(key = indicator, value = value, -name, -town) %>%
	mutate(town = factor(town, levels = c("Hartford", "West Hartford", "Greater Hartford", "Connecticut")),
				 name = factor(name),
				 type = str_extract(indicator, "^([a-z]+)"),
				 indicator = ifelse(str_detect(indicator, "title"),
				 									 indicator,
				 									 str_sub(indicator, 5)) %>% factor(., levels = unique(.)),
				 heading = paste0(name, ", ", town, " ", type) %>% factor(., levels = unique(.))) %>%
	select(-type, -name, -town) %>%
	spread(key = heading, value = value) %>%
	select(-ends_with("title")) %>%       # shuffle column positions
	select(indicator, 2:32 %>% map(~c(., . + 31)) %>% reduce(c))

excel <- loadWorkbook(paste0("Hartford ", year, " neighborhood profile.xlsx"))
setStyleAction(excel, XLC$STYLE_ACTION.NONE)
writeNamedRegion(excel, prof_xl %>% select(-indicator), name = "profile_data", header = T)
saveWorkbook(excel)
