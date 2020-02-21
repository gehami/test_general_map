######## Libraries #########

library(tidycensus)
library(tidyverse)
library(purrr)


######## Constants #########

START_ACS_YEAR = 2014
END_ACS_YEAR = 2018
CONTAIN_THRESHOLD = 0.5


######## General functions #########
#like the %in% function, but retains the order of the vector that your checking to see if the other vector is in it.
#EX: if x <- c(1,4,6) and y <- c(6,1,4), then in_match_order(x, y) = c(3,1,2) since 6 appears at x_ind 3, 1 appears at x_ind 1, and 4 appears at x_ind 2
in_match_order = function(vec_in, vec){
  ret_inds = NULL
  for(n in vec){
    ret_inds = c(ret_inds,
                 try(which(n == vec_in)[1])
    )
  }
  return(ret_inds[!is.na(ret_inds)])
}


###### pulling the vars that are needed ##########

# test_profile = load_variables(ACS_YEAR, 'acs5/profile')
# test_subject = load_variables(ACS_YEAR, 'acs5/subject')
# test = load_variables(ACS_YEAR, 'acs5')
# setwd('shiny_web_app/')

vars_needed = read.csv('variable_mapping.csv', stringsAsFactors = FALSE, header = TRUE)
acs_vars_needed = vars_needed[vars_needed$Dataset == 'ACS',]

acs_codes = unique(acs_vars_needed$var_name)

###### seting up the census api #########

census_key = readLines('C:\\Users\\gehami\\Documents\\API Keys\\census_api_key.txt')
census_api_key(key = census_key)


##### downloading the data ##########

#shout out to Kyle walker for this code snippet on grabbing every state's census tracts
#https://walkerke.github.io/2017/05/tidycensus-every-tract/
us <- unique(fips_codes$state)[1:51]

for(ACS_YEAR in START_ACS_YEAR:END_ACS_YEAR){
  

  all_acs_dat <- map_df(us, function(x) {
    get_acs(geography = "tract", variables = acs_codes[1], year = ACS_YEAR, survey = 'acs5',
            state = x, cache_table = TRUE)
  })

  all_acs_dat$moe = NULL
  colnames(all_acs_dat)[colnames(all_acs_dat) == 'estimate'] = all_acs_dat$variable[1]
  all_acs_dat$variable = NULL
  for(n in 2:length(acs_codes)){
    new_var <- map_df(us, function(x) {
      get_acs(geography = "tract", variables = acs_codes[n], year = ACS_YEAR, survey = 'acs5',
              state = x, cache_table = TRUE)
    })
    all_acs_dat = cbind(all_acs_dat, estimate = new_var$estimate)
    colnames(all_acs_dat)[colnames(all_acs_dat) == 'estimate'] = new_var$variable[1]
    
  }
  
  ##### saving the data #########
  
  print(paste0('Saving ', ACS_YEAR))
  
  saveRDS(all_acs_dat, paste0('data_tables/all_acs_dat_',ACS_YEAR, '.rds'))
  
  
}


####### Identifying tracts for each city in the US #########

library(rgeos)
library(tigris)
library(sp)
library(hash)
library(magrittr)

#given a single row in an spdf, returns spdf row with only the largest polygon (by area)
get_largest_shape = function(spdf, row_id = 1){
  require(sp)
  spolys = spdf@polygons[[1]]@Polygons
  max_area = 0
  for(n in seq_along(spolys)){
    if(spolys[[n]]@area > max_area){
      max_area = spolys[[n]]@area
      max_area_n = n
    }
  }
  single_poly = Polygons(list(spolys[[max_area_n]]), row_id)
  single_spoly = SpatialPolygons(list(single_poly))
  proj4string(single_spoly) = spdf@proj4string
  data_for_spdf = data.frame(spdf@data[1,], stringsAsFactors = FALSE, row.names = row_id)
  ret_spdf = SpatialPolygonsDataFrame(single_spoly, data = data_for_spdf, match.ID = TRUE)
  return(ret_spdf)
}
#given two shapes (spdf), determines what %age of points one is within the other
points_in_shape = function(shape, shape_within, ret_perc = TRUE, n = 1){
  points = SpatialPoints(shape_within[n,]@polygons[[1]]@Polygons[[1]]@coords, proj4string = shape_within@proj4string)
  if(ret_perc) return(length(which(!is.na(over(points, shape)[,1])))/nrow(points@coords))
  return(length(which(!is.na(over(points, shape)[,1]))))
}
#given all of the census tracts, returns the census tracts that are within the city limits and additional tracts 
tracts_in_shape = function(tracts, shape, contain_threshold = .5){
  require(rgeos)
  if(contain_threshold <= 0){
    return(tracts[which(gIntersects(shape, tracts, byid = TRUE)),])
  }
  contain_tracts = tracts[which(gContains(shape, tracts, byid = TRUE)),]
  if(is.numeric(contain_threshold) & contain_threshold < 1){
    border_tracts = tracts[which(gOverlaps(shape, tracts, byid = TRUE)),]
    keep_tracts = NULL
    for(n in seq_len(nrow(border_tracts))){
      if(points_in_shape(shape, border_tracts, ret_perc = TRUE, n) > contain_threshold){
        keep_tracts = c(keep_tracts, n)
      }
    }
    contain_tracts = rbind(contain_tracts, border_tracts[keep_tracts,])
  }
  return(contain_tracts)
}
#given the city name, the county fips for the city, and the state abbreviation, returns all of the tracts in that city
get_tracts_in_city = function(city_name, county_fips, state_abb, contain_threshold = 0){
  require(rgeos)
  require(tigris)
  county_tracts = tryCatch(tracts(state_abb, county_fips, cb = TRUE),
                           error = function(e) tracts(state_abb, county_fips))
  city_shape = tryCatch({places(state_abb, cb = TRUE)}, 
                        error = function(e) places(state_abb))
  
  city_shape = city_shape[city_shape$NAME == city_name,] %>% get_largest_shape()
  
  # shape = city_shape
  # tracts = county_tracts
  ret_tracts = tracts_in_shape(county_tracts, city_shape, contain_threshold = contain_threshold)
  return(ret_tracts$GEOID)
}

#pulling together mapping of all cities and tracts
city_to_county_map = read.csv('uscities-counties.csv', stringsAsFactors = FALSE)


#creating a hash for each city to have its own list of tracts, and a large list of all the tracts to include
include_tracts = NULL
fail_cities = NULL
#dictionary for city and tracts
all_tract_city_dictionary = hash::hash()

missing_cities = NULL

for(n in 1:length(state.abb)){
  state_abb = state.abb[n]
  
  
  state = tigris::places(state = state_abb)
  
  state$NAME = gsub(' \\([[:print:]]+\\)$', '',state$NAME) #cleaning state name
  if(length(which(state$NAME %in% city_to_county_map$city)) != length(state$NAME)) warning(paste0('some cities missing in ', state_abb)) #confirming all cdc cities are in city-to-county-map
  #tracking missing cities
  missing_cities = c(missing_cities, state[which(!(state$NAME %in% city_to_county_map$city)),])
  
  for(i in which(state$NAME %in% city_to_county_map$city)){
    city = state[i,]
    city_state_name = paste0(city$NAME, ' ', state_abb)
    city_county = city_to_county_map[which(paste0(city_to_county_map$city, city_to_county_map$state_id) %in% paste0(city$NAME, state_abb)),
                                     which(colnames(city_to_county_map) %in% c("city", "county_name", 'county_fips', 
                                                                               'state_name', 'state_id'))]
    
    city_name = city$NAME
    county_fips = substr(city_county$county_fips, nchar(city_county$county_fips) - 2, nchar(city_county$county_fips))[1]
    
    tryCatch({tracts_in_city = get_tracts_in_city(city_name, county_fips, state_abb, contain_threshold = CONTAIN_THRESHOLD)
    include_tracts = c(include_tracts, tracts_in_city)
    tract_city_dictionary[[city_state_name]] = tracts_in_city},
    error = function(e){
      fail_cities = c(fail_cities, city_state_name)
    })
    #testing to make sure it works. It seems to.
    # state_tracts = tracts(state_abb, county_fips)
    # state_tracts_for_city = state_tracts[state_tracts$GEOID %in% tracts_in_city,]
    # plot(state_tracts_for_city, border = 'red')
    # plot(city, add = TRUE)
    
    
  }
  
}







###### identifying only the tracts in the 500 cdc cities - tract_city_dictionary ##########


cdc_dat = readRDS('data_tables/cdc_2018.rds')
city_list = data.frame(city = cdc_dat$placename, state = cdc_dat$stateabbr, stringsAsFactors = FALSE)[!duplicated(cdc_dat$placename),]

#for each city, I need to download that city's shapefile, and then find all the tracts that are within or intersect with that city.
city_to_county_map = read.csv('uscities-counties.csv', stringsAsFactors = FALSE)


length(which(city_list$city %in% city_to_county_map$city)) == length(city_list$city) #confirming all cdc cities are in city-to-county-map
city_counties = city_to_county_map[which(paste0(city_to_county_map$city, city_to_county_map$state_id) %in% paste0(city_list$city, city_list$state)),
                                   which(colnames(city_to_county_map) %in% c("city", "county_name", 'county_fips', 
                                                                                        'state_name', 'state_id'))]

city_counties = city_counties[order(city_counties$city, city_counties$state_id),]
city_counties$county_fips = substr(city_counties$county_fips, nchar(city_counties$county_fips) - 2, nchar(city_counties$county_fips))
city_list = city_list[order(city_list$city, city_list$state),]
#now that the two lists are identical and ordered, we can append them
city_county_list = data.frame(city_list, city_counties, stringsAsFactors = FALSE)

#adding the alternative names to the city_county_list that are picked up by tigris
city_county_list$city.1 = NULL
city_county_list$alt_name = ""
fix_name_cities = city_to_county_map[city_to_county_map$tigris_city_name != '',c('tigris_city_name', 'city', 'state_name', 'state_id')]
for(n in 1 : nrow(fix_name_cities)){
  fill_row = which(city_county_list$city == fix_name_cities$city[n] & city_county_list$state_id == fix_name_cities$state_id[n])
  city_county_list$alt_name[fill_row] = fix_name_cities$tigris_city_name[n]
}
#checking to make sure all names that needed to be fixed were fixed --> it does. 
print(length(which(city_county_list$alt_name != '')) == nrow(fix_name_cities))


library(rgeos)
library(tigris)
library(sp)
library(hash)

#given a single row in an spdf, returns spdf row with only the largest polygon (by area)
get_largest_shape = function(spdf, row_id = 1){
  require(sp)
  spolys = spdf@polygons[[1]]@Polygons
  max_area = 0
  for(n in seq_along(spolys)){
    if(spolys[[n]]@area > max_area){
      max_area = spolys[[n]]@area
      max_area_n = n
    }
  }
  single_poly = Polygons(list(spolys[[max_area_n]]), row_id)
  single_spoly = SpatialPolygons(list(single_poly))
  proj4string(single_spoly) = spdf@proj4string
  data_for_spdf = data.frame(spdf@data[1,], stringsAsFactors = FALSE, row.names = row_id)
  ret_spdf = SpatialPolygonsDataFrame(single_spoly, data = data_for_spdf, match.ID = TRUE)
  return(ret_spdf)
}
#given two shapes (spdf), determines what %age of points one is within the other
points_in_shape = function(shape, shape_within, ret_perc = TRUE, n = 1){
  points = SpatialPoints(shape_within[n,]@polygons[[1]]@Polygons[[1]]@coords, proj4string = shape_within@proj4string)
  if(ret_perc) return(length(which(!is.na(over(points, shape)[,1])))/nrow(points@coords))
  return(length(which(!is.na(over(points, shape)[,1]))))
}
#given all of the census tracts, returns the census tracts that are within the city limits and additional tracts 
tracts_in_shape = function(tracts, shape, contain_threshold = .5){
  require(rgeos)
  if(contain_threshold <= 0){
    return(tracts[which(gIntersects(shape, tracts, byid = TRUE)),])
  }
  contain_tracts = tracts[which(gContains(shape, tracts, byid = TRUE)),]
  if(is.numeric(contain_threshold) & contain_threshold < 1){
    border_tracts = tracts[which(gOverlaps(shape, tracts, byid = TRUE)),]
    keep_tracts = NULL
    for(n in seq_len(nrow(border_tracts))){
      if(points_in_shape(shape, border_tracts, ret_perc = TRUE, n) > contain_threshold){
        keep_tracts = c(keep_tracts, n)
      }
    }
    contain_tracts = rbind(contain_tracts, border_tracts[keep_tracts,])
  }
  return(contain_tracts)
}
#given the city name, the county fips for the city, and the state abbreviation, returns all of the tracts in that city
get_tracts_in_city = function(city_name, county_fips, state_abb, contain_threshold = 0){
  require(rgeos)
  require(tigris)
  county_tracts = tryCatch(tracts(state_abb, county_fips, cb = TRUE),
                           error = function(e) tracts(state_abb, county_fips))
  city_shape = tryCatch({places(state_abb, cb = TRUE)}, 
                        error = function(e) places(state_abb))
  
  city_shape = city_shape[city_shape$NAME == city_name,] %>% get_largest_shape()

  # shape = city_shape
  # tracts = county_tracts
  ret_tracts = tracts_in_shape(county_tracts, city_shape, contain_threshold = contain_threshold)
  return(ret_tracts$GEOID)
}


#creating a hash for each city to have its own list of tracts, and a large list of all the tracts to include
include_tracts = NULL
fail_cities = NULL
tract_city_dictionary = hash::hash()

for(n in seq_along(city_county_list[,1])){
  city_name = city_county_list$city[n]
  county_fips = city_county_list$county_fips[n]
  state_abb = city_county_list$state_id[n]
  city_state_name = paste0(city_name, ' ', state_abb)
  
  #if need be, fills in the alt name for tigris to identify it.
  if(city_county_list$alt_name[n] != ''){
    city_name = city_county_list$alt_name[n]
  }
  
  tryCatch({tracts_in_city = get_tracts_in_city(city_name, county_fips, state_abb, contain_threshold = CONTAIN_THRESHOLD)
  include_tracts = c(include_tracts, tracts_in_city)
  tract_city_dictionary[[city_state_name]] = tracts_in_city},
  error = function(e){
    fail_cities = c(fail_cities, city_state_name)
  })
}


#seeing which cities were not included in the has
missing_cities = city_county_list[which(!(paste0(city_county_list$city, ' ', city_county_list$state_id) %in% keys(tract_city_dictionary))),]
missing_cities = missing_cities[order(missing_cities$state),]
#we will need to figure out how to add those cities into the hash. all told there are 21 missing cities, so not too bad. FIXED!

# library(clipr)
#copy to clipboard
state = places(state = 'UT')
clipr::write_clip(state$NAME)

#NOTES ON FIXING CITIES: 
#HONOLULU SEEMS TO HAVE AN EAST HONOLULU AND AN URBAN HONOLULU. defaulted to urban honolulu


saveRDS(tract_city_dictionary, 'data_tables/tract_city_dictionary.rds')

###### filtering the acs data down to only the tracts in the 500 cdc cities - acs_hash ##########

acs_hash = hash::hash()
years = seq(2014, 2018)
vars_needed = read.csv('variable_mapping.csv', stringsAsFactors = FALSE, header = TRUE)
acs_codebook = vars_needed[vars_needed$Dataset == 'ACS',]

tract_city_dictionary = readRDS('data_tables/tract_city_dictionary.rds')



for(year in years){
  if(!(year %in% keys(acs_hash))){
    acs_dat = readRDS(paste0('data_tables/all_acs_dat_', as.character(year), '.rds'))
    
    total_household = acs_dat[,acs_codebook$var_name[acs_codebook$ï..Variable.name == 'Total households']]
    total_pop = acs_dat[,acs_codebook$var_name[acs_codebook$ï..Variable.name == 'Total pop']]
    #conducting operations and creating new_acs_dat
    new_acs_dat = data.frame(GEOID = acs_dat$GEOID)
    for(n in which(!duplicated(acs_codebook$Operation) | acs_codebook$Operation == '')){
      if(acs_codebook$Operation[n] == ''){
        new_acs_dat = data.frame(new_acs_dat,acs_dat[,acs_codebook$var_name[n]], stringsAsFactors = FALSE)
        colnames(new_acs_dat)[ncol(new_acs_dat)] = acs_codebook$Name[n]
      }else{
        var_operation = acs_codebook$Operation[n]
        if(length(which(acs_codebook$Operation == var_operation))<2){
          new_var = acs_dat[,acs_codebook$var_name[acs_codebook$Operation == var_operation]]
        }else{
          new_var = rowSums(acs_dat[,acs_codebook$var_name[acs_codebook$Operation == var_operation]]) #if there are multiple vars under the same operation, adds them together first
        }
        if(grepl('minus', var_operation, ignore.case = TRUE)){ #if the var needs to be a number subtracted by the var (usually to reverse the direction of a variable), does it
          minus_number = gsub('([[:print:]]*)(minus)(_*)([0-9]+)([[:print:]]*)', '\\4', var_operation) %>% as.numeric()
          new_var = minus_number - new_var
        }
        if(grepl('over_total_household', var_operation, ignore.case = TRUE)){ #if a var needs to be divided by the total household population
          new_var = new_var/total_household
        }
        if(grepl('over_total_pop', var_operation, ignore.case = TRUE)){ #if a var needs to be divided by the total population
          new_var = new_var/total_pop
        }
        if(grepl('times', var_operation, ignore.case = TRUE)){ #if the var needs to be multiplied by a number
          times_number = gsub('([[:print:]]*)(times)(_*)([0-9]+)([[:print:]]*)', '\\4', var_operation) %>% as.numeric()
          new_var = new_var * times_number
        }
        new_acs_dat = data.frame(new_acs_dat, new_var, stringsAsFactors = FALSE)
        colnames(new_acs_dat)[ncol(new_acs_dat)] = acs_codebook$Name[n]
      }
    }
    
    acs_hash[[as.character(year)]] = new_acs_dat
  }
}

all_tracts = values(tract_city_dictionary) %>% unlist() %>% unique()

for(acs_key in keys(acs_hash)){
  new_acs = acs_hash[[acs_key]]
  trimmed_acs = new_acs[new_acs$GEOID %in% all_tracts,]
  acs_hash[[acs_key]] = trimmed_acs
}



saveRDS(acs_hash, file = 'data_tables/acs_dat_hash.rds')

########## Identifying the zipcodes of each tract, adding to acs_hash - acs_hash ###############

tract_city_dictionary = readRDS('data_tables/tract_city_dictionary.rds')

all_tracts = hash::values(tract_city_dictionary) %>% unlist() %>% unique()

tract_zip_mapping = read.csv(url('https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt?#'), stringsAsFactors = FALSE)


#converting the zip code into characters and making all zips 5 digits - works.
tract_zip_mapping$ZCTA5 = as.character(tract_zip_mapping$ZCTA5)
tract_zip_mapping$ZCTA5[nchar(tract_zip_mapping$ZCTA5) == 3] = paste0('00', tract_zip_mapping$ZCTA5[nchar(tract_zip_mapping$ZCTA5) == 3])
tract_zip_mapping$ZCTA5[nchar(tract_zip_mapping$ZCTA5) == 4] = paste0('0', tract_zip_mapping$ZCTA5[nchar(tract_zip_mapping$ZCTA5) == 4])
tract_zip_mapping$GEOID = as.character(tract_zip_mapping$GEOID)


#this gets the majority of tracts, but not all of them
all_tracts_to_zip_mapping = data.frame(GEOID = all_tracts, zipcodes = '', stringsAsFactors = FALSE)
#checking to make sure every tract is mapped to some zipcode - they are.
# length(all_tracts %in% tract_zip_mapping$GEOID) == length(all_tracts)
for(n in seq_along(all_tracts)){
  zips = tract_zip_mapping[tract_zip_mapping$GEOID %in% all_tracts[n], c('ZCTA5', 'TRPOPPCT')]
  zips = zips[order(as.numeric(zips$TRPOPPCT), decreasing = TRUE),]
  # if(nrow(zips) < 1){print(paste0('failure at ', n, ' GEOID ', all_tracts[n]))}
  if(n %% 1000 == 0){print(n/length(all_tracts))}
  all_tracts_to_zip_mapping$zipcodes[n] = paste(zips$ZCTA5, collapse = ', ')
}


#To get the rest of the tracts' zip codes we'll need to manually match the spatial files

#opening the zipcode to tract mapping, can be found here: https://www.census.gov/geographies/reference-files/2010/geo/relationship-files.html#par_textimage_674173622
zip_shape = tigris::zctas()

trimmed_tracts = readRDS('data_tables/trimmed_tract_data.rds')
trimmed_tracts_ordered = trimmed_tracts[in_match_order(trimmed_tracts$GEOID, all_tracts),]

# zip_shape@proj4string 
# trimmed_tracts@proj4string #these are the same

#given the zipcode shapefile and a trimmed tract row, returns all zips that intersect with that tract in order of how much of the tract is in the zip
zips_of_tract = function(tract_row, zip_shape){
  selected_zips = zip_shape[which(gIntersects(tract_row, zip_shape, byid = TRUE)),]
  zips_to_tract = data.frame(zips = selected_zips$ZCTA5CE10, contain_perc = 0, stringsAsFactors = FALSE)
  if(nrow(zips_to_tract) == 1){return(data.frame(zips = selected_zips$ZCTA5CE10, contain_perc = 1, stringsAsFactors = FALSE))}
  for(n in 1:nrow(zips_to_tract)){
    zips_to_tract[n,2] = points_in_shape(selected_zips[n,], tract_row)
  }
  return(zips_to_tract[order(zips_to_tract$contain_perc, decreasing = TRUE),])
}
#given two shapes (spdf), determines what %age of points one is within the other
points_in_shape = function(shape, shape_within, ret_perc = TRUE, n = 1){
  points = SpatialPoints(shape_within[n,]@polygons[[1]]@Polygons[[1]]@coords, proj4string = shape_within@proj4string)
  if(ret_perc) return(length(which(!is.na(over(points, shape)[,1])))/nrow(points@coords))
  return(length(which(!is.na(over(points, shape)[,1]))))
}



identical(as.character(all_tracts), as.character(trimmed_tracts_ordered@data$GEOID))

#works
missing_tracts = which(all_tracts_to_zip_mapping$zipcodes == '')
progress_tracker = 1
for(n in missing_tracts){
  all_tracts_to_zip_mapping$zipcodes[n] = paste0(zips_of_tract(trimmed_tracts_ordered[n,], zip_shape)$zips, collapse = ', ')
  progress_tracker = progress_tracker + 1
  if(progress_tracker %% 10 == 0) print(progress_tracker/length(missing_tracts))
}

#testing the above. This works.
# n = missing_tracts[100]
# plot(zip_shape[zip_shape$ZCTA5CE10 == zips_of_tract(trimmed_tracts_ordered[n,], zip_shape)$zips[1],])
# plot(trimmed_tracts_ordered[n,], add = TRUE, col = 'red')


# all_tracts_to_zip_mapping[sample(1:nrow(all_tracts_to_zip_mapping), 10),]

acs_hash = readRDS('data_tables/acs_dat_hash.rds')

#checking to make sure the GEOIDs are in each file
length(which(all_tracts_to_zip_mapping$GEOID %in% acs_hash$`2018`$GEOID)) == length(acs_hash$`2018`$GEOID)


for(year in keys(acs_hash)){
  # acs_hash[[year]]$GEOID = as.character(acs_hash[[year]]$GEOID)
  acs_hash[[year]]$zipcodes = NULL
  acs_hash[[year]] = merge(acs_hash[[year]], all_tracts_to_zip_mapping, by = 'GEOID')
}

saveRDS(acs_hash, file = 'data_tables/acs_dat_hash.rds')

########## saving all do the cities that I need as their own spatial files of tracts ##########

require(tigris)
all_us_tracts = tracts(state = state.abb[1])
for(state_abb in c(state.abb[-1], 'DC')){
  all_us_tracts = rbind(all_us_tracts, tracts(state = state_abb))
}


#checking to make sure all of the tracts in all_tracts are in here. It is close but no cigar. missing DC tracts. Woops. Fixed. 
length(which(as.character(all_tracts) %in% all_us_tracts$GEOID)) == length(all_tracts)

trimmed_us_tracts = all_us_tracts[all_us_tracts$GEOID %in% all_tracts,]


saveRDS(trimmed_us_tracts, file = 'data_tables/trimmed_tract_data.rds')






