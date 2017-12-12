# need to use this workaround for problems in API for 2016
# acs.fetch(endyear=2016, span=5, variable=acs.lookup(endyear=2015, span=5, table.number="B01001"), geography = geo.make(state = 09))

############## REDO FUNCTIONS ###############

all_geo_table_x <- function(town_lookup, table.number, year = 2016, regions, state = 9) {
	if(!"county" %in% names(town_lookup)) stop("town_lookup missing column county. Try again with function get_town_names")
	if(!"town" %in% names(town_lookup)) stop("town_lookup missing column town. Try again with function get_town_names")
	
	state_geo <- acs::geo.make(state = state)
	region <- purrr::map2(regions, names(regions), ~make_regional_geo(.x, .y, town_lookup = town_lookup)) %>% purrr::reduce(c)
	county <- acs::geo.make(state = state, county = "*")
	towns <- acs::geo.make(state = state, county = "*", county.subdivision = "*")
	geos <- c(state_geo, region, county, towns)
	table <- acs::acs.fetch(geography = geos, endyear = year, variable = acs.lookup(endyear = 2015, span = 5, table.number = table.number), col.names = "pretty")
	return(table)
}

#############################################

neighborhood_table_x <- function(neighborhood_list, table.number, year = 2016, state = 9, county = 9, blocks = F) {
	if(!is.list(neighborhood_list)) stop("neighborhood_list must be a named list")
	if(is.null(names(neighborhood_list))) stop("neighborhood_list must be a named list")
	geos <- neighborhood_list %>%
		purrr::map2(names(neighborhood_list),
								~make_neighborhood(.x, name = .y, state = state, county = county, blocks = blocks)) %>%
		purrr::reduce(c)
	table <- acs::acs.fetch(geography = geos, endyear = year, variable = acs.lookup(endyear = 2015, span = 5, table.number = table.number), col.names = "pretty")
	acs::geography(table)$NAME <- names(neighborhood_list)
	return(table)
}

#############################################

regional_table_x <- function(town_list, name = "Aggregate", town_lookup, state = 9, table.number, year = 2016) {
	geo <- acs::geo.make(state = state,
											 county = dplyr::filter(town_lookup, town %in% town_list)$county,
											 county.subdivision = dplyr::filter(town_lookup, town %in% town_list)$town,
											 combine = T,
											 combine.term = name)
	table <- acs::acs.fetch(geography = geo, endyear = year, variable = acs.lookup(endyear = 2015, span = 5, table.number = table.number), col.names = "pretty")
	return(table)
}

##########################################