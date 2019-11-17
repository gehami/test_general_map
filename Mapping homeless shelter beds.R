######### Libraries #########

library(devtools)


# devtools::install_github("dkahle/ggmap")
library(ggmap)


library(leaflet)
library(tidyverse)
library(magrittr)
library(dplyr)
library(sp)
library(htmltools)
library(hash)

##### directory #######
setwd("shiny_web_app")

######## Constants #######

PROJ_4_STRING = CRS("+proj=longlat +datum=WGS84")

#data for homelessness - 
#binge drinking, mental health, low education, households under poverty, unemployment
homeless_risk_vars = c('Low education (no high school diploma)',
                       'Unemployment rate', 
                       'Households under poverty line',
                       'Rate of binge drinking', 
                       'Mental health diagnoses')
homeless_risk_weights = c(1,
                          1,
                          1,
                          1,
                          1)

#percent of a variable that is allowed to be NA for me to keep it in the predictors dataset
NA_TOL = .1


############ Globally used functions #############
#given a vector of numeric values, or something that can be coerced to numeric, returns a vector of the percentile each observation is within the vector.
#Example: if vec == c(1,2,3), then get_percentile(vec) == c(0.3333333, 0.6666667, 1.0000000)
get_percentile = function(vec, compare_vec = NULL){
  if(is.null(compare_vec)){
    return(ecdf(vec)(vec))
  }else{
    new_vec = rep(0, length(vec))
    for(n in seq_along(vec)){
      new_vec[n] = ecdf(c(vec[n], compare_vec))(c(vec[n], compare_vec))[1]
    }
    return(new_vec)
  }
}
#given a vector of numeric values, and the number of bins you want to place values into, returns a vector of 'vec' length where each observation is the quantile of that observation.
#Example: if vec == c(1,2,3), then get_quantile(vec, quantile_bins = 2) = as.factor(c(0, 50, 50)). 
#To have the top observation be marked as the 100%ile, set ret_100_ile == TRUE. 
#To return a factor variable, ret_factor == TRUE, otherwise it will return a numeric vector. 
get_quantile = function(vec, quantile_bins, ret_factor = TRUE, ret_100_ile = FALSE, compare_vec = NULL){
  quantile_bins = round(min(max(quantile_bins, 2), 100)) #ensuring the quantile bins is an integer between 2 and 100
  quant_val = floor(get_percentile(vec, compare_vec)*100 / (100/quantile_bins)) * (100/quantile_bins)
  if(!ret_100_ile){ quant_val[quant_val == 100] = unique(quant_val)[order(-unique(quant_val))][2]}
  if(ret_factor){return(factor(quant_val))}
  return(quant_val)
}

#given a vector, min-max-scales it between 0 and 1
min_max_vec = function(vec, na.rm = TRUE, ...){
  if(max(vec, na.rm = na.rm, ...) == min(vec, na.rm = na.rm, ...)){
    return(rep(0, length(vec)))
  }
  return((vec - min(vec, na.rm = na.rm, ...))/(max(vec, na.rm = na.rm, ...)-min(vec, na.rm = na.rm, ...)))
}


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


######### geocoding the addresses ##########
api <- readLines("C:\\Users\\gehami\\Documents\\API Keys\\apikey.txt")
register_google(key = api)

#given a vector of addresses, returns the geocoded data
geocode_addys = function(addy_list){
  geocoded <- data.frame(name = addy_list, lon = 0, lat = 0, geoaddress = "", stringsAsFactors = FALSE)
  
  # Loop through the addresses to get the latitude and longitude of each address and add it to the
  # origAddress data frame in new columns lat and lon
  for(i in 1:nrow(geocoded)){
    # Print("Working...")
    worked = FALSE
    n = 1
    while(!worked){
      try({result <- geocode(addy_list[i], output = "latlona", source = "google")
      worked = TRUE
      })
      n = n+1
      if(n > 100) break
    }
    
    geocoded$lon[i] <- as.numeric(result[1])
    geocoded$lat[i] <- as.numeric(result[2])
    geocoded$geoaddress[i] <- as.character(result[3])
  }
  return(geocoded)
}


###### geocoding shelters #######
shelter_dat = read.csv("long-beach_housing_inventory_count.csv", stringsAsFactors = FALSE)
shelters =shelter_dat$Organization.Name[shelter_dat$Organization.Name != '']
shelters = paste0(as.character(shelters), ", Long Beach, CA")
shelters[shelter_dat$Filler_address != ""] = shelter_dat$Filler_address[shelter_dat$Filler_address != ""]

# geocoded_shelters = geocode_addys(shelters)
# saveRDS(geocoded_shelters, file = 'Geocoded addresses/shelter_geocodes.rds')

geocoded_shelters = readRDS('Geocoded addresses/shelter_geocodes.rds')

####### geocoding the foodbanks and making spatial database - foodbanks_spdf ##########
food_banks = read.csv('Food banks in long beach.csv', stringsAsFactors = FALSE)

# geocoded_foodbanks = geocode_addys(food_banks$address)
# saveRDS(geocoded_foodbanks, file = 'Geocoded addresses/foodbanks_geocodes.rds')

geocoded_foodbanks = readRDS('Geocoded addresses/foodbanks_geocodes.rds')


ma_foodbanks = data.frame(org = gsub('â€™|â€\u0090', '',food_banks$ï..name), phone = gsub('â€\u0090','-',food_banks$phone), 
                     geocoded_foodbanks[,-1], stringsAsFactors = FALSE)

food_banks_spdf = SpatialPointsDataFrame(coords = cbind(lon = as.numeric(ma_foodbanks$lon), lat = as.numeric(ma_foodbanks$lat)),
                                         data = ma_foodbanks, proj4string = PROJ_4_STRING)

##### Putting together the final shelter spatial database - shelter_spdf #########

ma_shelter = cbind(geocoded_shelters[,-1], shelter_dat) %>% select(org = Organization.Name, project = Project.Name, proj_type = Proj..Type, target_pop = Target.Pop.,
                                                          house_type = Housing.Type,
                                                          beds_hh_kids = Beds.HH.w..Children, beds_hh_no_kids = Beds.HH.w.o.Children,
                                                          year_round_beds = Year.Round.Beds, pit_count = PIT.Count, tot_beds = Total.Beds, utilization_rate = Utilization.Rate,
                                                          lon, lat, geoaddress)

ma_shelter$house_type = gsub(pattern = '[ ]?â€“', replacement = ':', x = shelter_dat$Housing.Type)

#collapsing the data
rowaddys = unique(ma_shelter$geoaddress)

#given a set of rows with the same geoaddress, collapses them properly into a single row
collapse_rows = function(rowaddy, ma_shelter){
  rows = ma_shelter[ma_shelter$geoaddress == rowaddy,]
  if(length(unique(rows$project)) > 1){
    rows$project = "multiple projects"
  }
  rows$proj_type = paste(unique(rows$proj_type), collapse = ', ')
  if(is.na(rows$proj_type[1])) rows$proj_type = ""
  rows$house_type = paste(unique(gsub(":[[:print:]]*", '', rows$house_type)), collapse = ' & ')
  ret_row = data.frame(org = rows$org[1], project = rows$project[1], proj_type = rows$proj_type[1], target_pop = rows$target_pop[1], house_type = rows$house_type[1], 
                       beds_hh_kids = sum(rows$beds_hh_kids), beds_hh_no_kids = sum(rows$beds_hh_no_kids), year_round_beds = sum(rows$year_round_beds), 
                       pit_count = sum(rows$pit_count), tot_beds = sum(rows$tot_beds), utilization_rate = sum(rows$pit_count)/sum(rows$tot_beds), 
                       lon = rows$lon[1], lat = rows$lat[1], geoaddress = rows$geoaddress[1], stringsAsFactors = FALSE)
  return(ret_row)
}

shelter_map_dat = collapse_rows(rowaddys[1], ma_shelter)
for(n in 2:length(rowaddys)){
  shelter_map_dat = rbind(shelter_map_dat, collapse_rows(rowaddys[n], ma_shelter))
}
for(col in 1:ncol(shelter_map_dat)){
  shelter_map_dat[is.na(shelter_map_dat[,col]),col] = ""
}

#making the labels
attach(shelter_map_dat)
labels = paste(sep = "<br/>", paste0(org, '- ', project), paste0("Beds: ", tot_beds), paste0('Occupied: ', pit_count, " (", round(as.numeric(utilization_rate), digits = 2)*100, "% occpuancy)"),
               paste0("Housing: ", house_type), paste0("Address: ", gsub(", ca [0-9]*, usa", '', geoaddress))) 
detach(shelter_map_dat)

shelter_map_dat$label = labels

shelter_spdf = sp::SpatialPointsDataFrame(coords = cbind(lon = as.numeric(shelter_map_dat$lon), lat = as.numeric(shelter_map_dat$lat)),
                                          data = shelter_map_dat, proj4string = PROJ_4_STRING)



######## Adding demographic data from the acs - lb_acs #######



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




#Identify the census tracts in long beach
library(tigris)
library(rgeos)

CITY_NAME = 'Long Beach'

la_county = tigris::tracts('CA', 'Los Angeles', cb = TRUE)
lb_city = places('CA', cb = TRUE)
lb_city = lb_city[lb_city$NAME == CITY_NAME,] %>% get_largest_shape()

lb_tracts = tracts_in_shape(la_county, lb_city, contain_threshold = .8)

acs_2017 = readRDS('data_tables/all_acs_dat_2017.rds')

lb_acs = lb_tracts
lb_acs@data = merge(lb_acs@data, acs_2017, by = 'GEOID')


###### Adding in some cdc data - lb_all #############

cdc_2018 = readRDS('data_tables/cdc_2018.rds')

cdc_merge = cdc_2018[which(cdc_2018$tractfips %in% lb_acs$GEOID),]
cdc_merge_geoid = cdc_merge$tractfips
for(n in seq_len(ncol(cdc_merge))){if(!is.na(as.numeric(cdc_merge[1,n]))) cdc_merge[,n] = as.numeric(cdc_merge[,n])}
cdc_merge = cdc_merge %>% select_if(is.numeric)
cdc_merge$placefips = NULL
cdc_merge$tractfips = NULL
cdc_merge$GEOID = cdc_merge_geoid
#for some reason there is a duplicate geoid in the cdc_merge, removing that
cdc_merge = cdc_merge[!duplicated(cdc_merge$GEOID),]


tracts_not_in_cdc = lb_acs$GEOID[which(!((lb_acs$GEOID) %in% (cdc_merge$GEOID)))]
for(n in seq_along(tracts_not_in_cdc)){
  add_row = c(rep(NA, (ncol(cdc_merge) - 1)), tracts_not_in_cdc[n])
  cdc_merge = rbind(cdc_merge, add_row)
}

lb_all = lb_acs
lb_all@data = merge(lb_acs@data, cdc_merge, by = 'GEOID')


######## Cleaning data, opening codebook, and and changing vars to numeric #########


#reading in the data codebook
data_code_book = read.csv('variable_mapping.csv', stringsAsFactors = FALSE)

#making all of the columns that need to be numeric numeric
for(n in data_code_book$var_name){
  lb_all@data[,n] = as.numeric(lb_all@data[,n])
}

#reversing the education variable (from hs graduation rate to rate of not graduating high school)
lb_all@data$DP02_0066P = 100 - lb_all@data$DP02_0066P

########## calculating the score and creating label - lb_all ##########

risk_vars = homeless_risk_vars
risk_weights = homeless_risk_weights
spdf = lb_all
data_code_book = data_code_book

calculate_score = function(risk_vars, risk_weights, spdf, data_code_book){
  
  if(length(risk_vars) != length(risk_weights)){
    warning("risk vars and risk weights are not the same length")
    return(NULL)
  }
  if(!all(risk_vars %in% data_code_book$ï..Variable.name)){
    warning("some var names are not in the codebook")
    return(NULL)
  }
  
  
  
  data_code_book = data_code_book[order(data_code_book$ï..Variable.name),]
  risk_dataset = data.frame(risk_vars, risk_weights, stringsAsFactors = FALSE)[order(risk_vars),]
  risk_dataset$var_code = data_code_book$var_name[data_code_book$ï..Variable.name %in% risk_dataset$risk_vars]
  
  #get the vars from the spdf that are valuable here and order them in the same was as the risk_dataset
  score_vars = spdf@data[,which(colnames(spdf@data) %in% risk_dataset$var_code)]
  score_vars = score_vars[,risk_dataset$var_code]
  
  #standardizing the score_vars between 0 and 1
  for(n in seq_len(ncol(score_vars))){
    score_vars[,n] = min_max_vec(score_vars[,n], na.rm = TRUE)
  }
  
  #Marking any entries with NAs as a large negative number to ensure the resulting value is negative, so we can mark NA scores.
  for(n in seq_len(ncol(score_vars))){
    score_vars[is.na(score_vars[,n]),n] = -10000000
  }
  score_mat = data.matrix(score_vars)
  
  #multiplying those scores by the weights and summing them. works
  score = score_mat %*% risk_dataset$risk_weights
  
  score[score < 0] = NA
  
  return(data.frame(geoid = spdf$GEOID, score = min_max_vec(score, na.rm = TRUE)))
  
}

lb_all@data$score = calculate_score(homeless_risk_vars, homeless_risk_weights,
                                    lb_all, data_code_book)$score

#making the label from this
make_label_for_score = function(risk_vars, spdf, data_code_book, quantile_bins = 10){
  label_list = NULL
  for(row_ind in 1:nrow(spdf@data)){

    label_string = NULL
    for(n in seq_along(risk_vars)){
      label_string = c(label_string, paste0(risk_vars[n], ': ', 
                                            round(spdf@data[row_ind,which(colnames(spdf@data) == data_code_book$var_name[data_code_book$ï..Variable.name == risk_vars[n]])]), '% (', 
                                            get_quantile(spdf@data[,which(colnames(spdf@data) == data_code_book$var_name[data_code_book$ï..Variable.name == risk_vars[n]])], quantile_bins = quantile_bins)[row_ind], '%ile)'))
    }
    full_label = c(paste0("<b>Overall metric: ", get_quantile(spdf@data$score[row_ind], quantile_bins = quantile_bins, compare_vec = spdf@data$score), "%ile</b>"), label_string)
    label_list = c(label_list, paste(full_label, collapse = '</br>')) 
  }
  return(label_list)
}

lb_all@data$label = make_label_for_score(homeless_risk_vars, lb_all, data_code_book)





########## Pulling the past acs data (acs 2016) and fixing numbers - lb_past_acs #########

acs_2016 = readRDS('data_tables/all_acs_dat_2016.rds')
#reversing the education variable (from hs graduation rate to rate of not graduating high school)
acs_2016$DP02_0066P = 100 - acs_2016$DP02_0066P

lb_past_acs = lb_tracts
lb_past_acs@data = merge(lb_past_acs@data, acs_2016, by = 'GEOID')



######## pulling the past cdc data (cdc_2016) - lb_past_all ######

cdc_2016 = readRDS('data_tables/cdc_2016.rds')

cdc_merge = cdc_2016[which(cdc_2016$tractfips %in% lb_past_acs$GEOID),]
cdc_merge_geoid = cdc_merge$tractfips
#converting cdc columns to numeric
for(n in seq_len(ncol(cdc_merge))){
  if(!is.na(tryCatch(as.numeric(cdc_merge[1,n]), error = function(e) NA))){
    cdc_merge[,n] = as.numeric(cdc_merge[,n])
  } 
}
cdc_merge = cdc_merge %>% select_if(is.numeric)
cdc_merge$placefips = NULL
cdc_merge$tractfips = NULL
cdc_merge$GEOID = cdc_merge_geoid
#for some reason there is a duplicate geoid in the cdc_merge, removing that
cdc_merge = cdc_merge[!duplicated(cdc_merge$GEOID),]


tracts_not_in_cdc = lb_past_acs$GEOID[which(!((lb_past_acs$GEOID) %in% (cdc_merge$GEOID)))]
for(n in seq_along(tracts_not_in_cdc)){
  add_row = c(rep(NA, (ncol(cdc_merge) - 1)), tracts_not_in_cdc[n])
  cdc_merge = rbind(cdc_merge, add_row)
}

lb_past_all = lb_past_acs
lb_past_all@data = merge(lb_past_acs@data, cdc_merge, by = 'GEOID')

####### Calculating past score and creating label ##########

#making all of the columns that need to be numeric numeric
for(n in data_code_book$var_name){
  if(n %in% colnames(lb_past_all@data)){
    lb_past_all@data[,n] = as.numeric(lb_past_all@data[,n])
    }
}


risk_vars = homeless_risk_vars
risk_weights = homeless_risk_weights
spdf = lb_past_all
data_code_book = data_code_book

lb_past_all@data$score = calculate_score(homeless_risk_vars, homeless_risk_weights,
                                    lb_past_all, data_code_book)$score


lb_past_all@data$label = make_label_for_score(homeless_risk_vars, lb_past_all, data_code_book)


######### Narrowing down the predictor variables to the ones that actually exist - lb_past_all ###########


drop_na_vars = function(df, NA_TOL = .1){
  tot_rows = nrow(df)
  remove_cols = NULL
  for(n in seq_len(ncol(df))){
    if(length(which(is.na(df[,n])))/tot_rows > NA_TOL){
      remove_cols = c(remove_cols,n)
    }
  }
  ret_df = df[,-remove_cols]
}

lb_past_all@data = drop_na_vars(lb_past_all@data, NA_TOL)

######### calculating the neighbor matrix - loc_dist_matrix #########
# 1/x_ij where x is number of blocks between block i and j (starting at 1), 0 if more than MAX_BLOCK_DIST away
MAX_LOC_DIST = 1 #looking at neighbords directly next to tract
loc_it = 1

#initializing the matrix
loc_dist_matrix = matrix(0, nrow = nrow(lb_past_all@data), ncol = nrow(lb_past_all@data))
loc_matrix = rgeos::gTouches(lb_past_all, byid = TRUE)



#iterates through all blocks of 1 - MAX_BLOCK_DIST away, identifies which iteration it was picked up, and marks that number into matrix
#this will likely take hours (lol, takes 1 second).
for(loc_it_count in loc_it : ncol(loc_dist_matrix)){
  layer_locs = loc_it_count
  marked_locs = loc_it_count
  for(its in 1  : MAX_LOC_DIST){
    if(length(layer_locs) > 1){
      layer_locs_vec = which(rowSums(loc_matrix[,layer_locs])>0)

    }else{
      layer_locs_vec = which(loc_matrix[,layer_locs])
    }
    layer_locs_vec = layer_locs_vec[which(!(layer_locs_vec %in% marked_locs))]
    loc_dist_matrix[layer_locs_vec,loc_it_count] = its
    layer_locs = layer_locs_vec
    marked_locs = c(marked_locs, layer_locs)
  }
  if(loc_it_count %% 50 == 0) print(loc_it_count)
}
# saveRDS(block_dist_matrix, 'RDS files/block_dist_matrix.rds')
# saveRDS(block, 'RDS files/current_block_it.rds')
# saveRDS(block_matrix, 'RDS files/block_matrix_t_f.rds')
#checking to make sure the above works - it seems to work.
# plot(lb_past_all[loc_dist_matrix[,50] > 0 & loc_dist_matrix[,50] < 3,])

colnames(loc_dist_matrix) = lb_past_all@data$GEOID
rownames(loc_dist_matrix) = lb_past_all@data$GEOID

loc_dist_matrix = 1/loc_dist_matrix
loc_dist_matrix[loc_dist_matrix > 1] = 0

############# Adding Neighbor avg variables to complete ind vars for prediction model - big_ind_dat ############

#given a vector of length n and the n by n neighbor matrix, returns a vector of n length of the averaged value for each GEOID's neibs on that var
get_neib_average_vec = function(vec, loc_dist_matrix, na_neibs_count = 0){
  return((vec %*% loc_dist_matrix)/(rowSums(loc_dist_matrix) - na_neibs_count)) 
}#checked and works
#given a vec of n values and a n by n neighbor matrix, returns the number of neighbors with an NA value for each row in the vec
count_na_neibs = function(vec, loc_dist_matrix){
  vec[!is.na(vec)] = 0
  vec[is.na(vec)] = 1
  na_neibs_count = vec %*% loc_dist_matrix
  return(na_neibs_count)
}#works
#given a vector of cn length (where c is an integer) and the n by n neighbor matrix, returns a vector of cn length of the averaged value for each row's neibs on that var
get_full_neib_average_vec = function(vec, loc_dist_matrix, na.rm = TRUE){
  ret_vec = rep(0, length(vec))
  next_start_vec_ind = 1
  n = nrow(loc_dist_matrix)
  
  if(na.rm){
    na_neibs_count = count_na_neibs(vec, loc_dist_matrix)
    vec[is.na(vec)] = 0
  } 
  
  for(c in seq_len(length(vec)/n)){
    focus_inds = next_start_vec_ind:(next_start_vec_ind+n-1)
    ret_vec[focus_inds] = get_neib_average_vec(vec[focus_inds], loc_dist_matrix, na_neibs_count)
    next_start_vec_ind = next_start_vec_ind + n
  }
  return(ret_vec)
} #checked and works (loose checking, but yeah seems to work)
#given the x_vars, ids, and the loc_dist_matrix, returns the table of calculated weighted average negihbor score for each x_var
neib_avg_scores = function(x_vars, ids, loc_dist_matrix, na.rm = TRUE){
  neib_matrix = data.frame(array(0, dim = c(nrow(x_vars), (length(x_vars) + 1))), stringsAsFactors = FALSE)
  colnames(neib_matrix) = c('GEOID', colnames(x_vars))
  neib_matrix$GEOID = ids
  loc_dist_matrix = loc_dist_matrix[rownames(loc_dist_matrix) %in% neib_matrix$GEOID, colnames(loc_dist_matrix) %in% neib_matrix$GEOID]
  for(x_var in colnames(x_vars)){
    neib_matrix[,x_var] = get_full_neib_average_vec(x_vars[,x_var], loc_dist_matrix, na.rm)
  }
  colnames(neib_matrix)[2:ncol(neib_matrix)] = paste0('neib_avg_', colnames(x_vars))
  
  if(!identical(neib_matrix$GEOID, ids)) message('not identical ids, something is wrong')
  
  return(neib_matrix)
  
}


x_vars = lb_past_all@data[,(which(colnames(lb_past_all@data) == 'NAME.y')+1):ncol(lb_past_all@data)]
#making sure all of the columns are numeric
for(n in seq_len(ncol(x_vars))) x_vars[,n] = as.numeric(x_vars[,n])

loc_dist_matrix = loc_dist_matrix
ids = lb_past_all@data$GEOID
ind_vars = data.frame(GEOID = ids, x_vars, stringsAsFactors = FALSE) #all the ind vars and GEOID id tag

neib_matrix = neib_avg_scores(x_vars, ids, loc_dist_matrix, na.rm = TRUE)

big_ind_dat = merge(ind_vars, neib_matrix, by = 'GEOID')

########### building dependent vars for prediction model - dep_dat ########


dep_vars = data_code_book$var_name[in_match_order(data_code_book$ï..Variable.name, homeless_risk_vars)]

dep_dat = data.frame(GEOID = lb_all@data$GEOID, lb_all@data[,dep_vars], stringsAsFactors = FALSE)

########### ordering deps and inds and building the models - model_list ########

dep_dat = dep_dat[order(dep_dat$GEOID),]
big_ind_dat = big_ind_dat[order(big_ind_dat$GEOID),]

if(identical(dep_dat$GEOID, big_ind_dat$GEOID)){
  print("Good for analysis")
}else{warning("ids don't match, not good for analysis")}

dep_dat_fin = dep_dat[,-1] %>% sapply(min_max_vec) #scales all of the variables as well
ind_dat_fin = big_ind_dat[,-1] %>% sapply(min_max_vec) #scales all of the variables as well

dep_df = data.frame(dep_dat_fin, stringsAsFactors = FALSE)
ind_df = data.frame(ind_dat_fin, stringsAsFactors = FALSE)

ind_df = ind_df[,grep(paste(colnames(dep_dat), collapse = '|'), colnames(ind_df), value = FALSE)]

#building the models
model_list = list()
for(n in 1 : ncol(dep_df)){
  model = lm(dep_df[,n] ~ ., data = ind_df)
  model_list[[n]] = model
}
score_model_data = merge(data.frame(GEOID = lb_all@data$GEOID, score = lb_all@data$score, stringsAsFactors = FALSE), 
                         data.frame(ind_df, GEOID = big_ind_dat$GEOID, stringsAsFactors = FALSE), by = 'GEOID')
score_fin_dat = score_model_data[,-1] %>% sapply(min_max_vec) %>% data.frame(stringsAsFactors = FALSE)
score.lm = lm(score ~ ., data = score_fin_dat)

model_list[[n+1]] = score.lm

names(model_list) = c(data_code_book$ï..Variable.name[in_match_order(data_code_book$var_name, colnames(dep_dat))], 'Overall risk factor score')

#checking the absolute error rate. since the scores are between 0 and 1, this shows the %error of the scores
summary(abs(lb_all$score[!is.na(lb_all$score)] - predict(score.lm, newdata = ind_df[!is.na(lb_all$score),])))

####### Getting the predicted score and appending to lb_all ############

#get the neighbor variables for the 2018 data 
x_vars = lb_all@data[,(which(colnames(lb_all@data) == 'NAME.y')+1):(ncol(lb_all@data)-2)]
#making sure all of the columns are numeric
for(n in seq_len(ncol(x_vars))) x_vars[,n] = as.numeric(x_vars[,n])

loc_dist_matrix = loc_dist_matrix
ids = lb_all@data$GEOID
ind_vars = data.frame(GEOID = ids, x_vars, stringsAsFactors = FALSE) #all the ind vars and GEOID id tag

neib_matrix = neib_avg_scores(x_vars, ids, loc_dist_matrix, na.rm = TRUE)

pred_ind_dat = merge(ind_vars, neib_matrix, by = 'GEOID')

#min_max_scaling pred_ind_dat
pred_ind_dat[,2:ncol(pred_ind_dat)] = sapply(pred_ind_dat[,2:ncol(pred_ind_dat)], min_max_vec)


# predicted_values = array(NA, dim = c(nrow(lb_all@data), ncol = length(homeless_risk_vars) + 2))
# colnames(predicted_values) = c('GEOID', homeless_risk_vars, 'Overall risk factor score')
# 
# for(n in 1 : length(model_list)){
#   predicted_values[,(n+1)] = predict(model_list[[n]], newdata = pred_ind_dat)
# }

pred_ind_dat = pred_ind_dat[,grep(paste(colnames(dep_dat), collapse = '|'), colnames(pred_ind_dat), value = FALSE)]

pred_score =  predict.lm(score.lm, newdata = pred_ind_dat)
division_factor = sum(pred_score, na.rm = TRUE)/sum(lb_all@data$score, na.rm = TRUE)
pred_score_fixed = pred_score/division_factor
pred_score_fixed[pred_score_fixed < 0] = 0

pred_score_quantile = get_quantile(pred_score_fixed, quantile_bins = 10)

pred_score_label = paste0("Predicted homelessness risk factor score 2020: <br/><b>", pred_score_quantile, "%ile</b>")

lb_all@data$pred_score = pred_score_fixed
lb_all@data$pred_score_quantile = pred_score_quantile
lb_all@data$pred_label = pred_score_label




##### starter map #############
lon_med = mean(lb_all@bbox[1,])
lat_med = mean(lb_all@bbox[2,])

map <- leaflet(options = leafletOptions(minZoom = 10, zoomControl = FALSE)) %>% 
  # add ocean basemap
  # addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.Full) %>%
  # focus map in a certain area / zoom level
  setView(lng = lon_med, lat = lat_med, zoom = 12) 






##### Adding the shelters ######

#adds a legend of circles to show size
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title = NULL){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border-radius: 50%")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px; margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
}



QUANT_BINS = 5
RET_FACTOR = TRUE
RET_100_ILE = TRUE
COMPARE_VEC = c(seq(0,1, length.out = QUANT_BINS+1))
#see here for color palletes: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
COLOR_PAL = 'Spectral'

circle_marker_radius_values = log10(as.numeric(shelter_spdf@data$tot_beds)^2)*5
color_val = get_quantile(as.numeric(shelter_spdf@data$utilization_rate), quantile_bins = QUANT_BINS, 
                         ret_factor = RET_FACTOR, ret_100_ile = RET_100_ILE, compare_vec = COMPARE_VEC)

map_pal = colorFactor(
  palette = COLOR_PAL,
  domain = color_val
)

shelter_map <- map %>% addCircleMarkers(data = shelter_spdf, radius = circle_marker_radius_values, 
                                        label = lapply(shelter_spdf@data$label, HTML), stroke =TRUE, 
                                        color = ~map_pal(color_val),
                                        opacity = .8) %>% 
  addLegend(position = "topright", pal = map_pal, title = "Bed Utilization Rate",
            values = color_val) 
#%>% addLegendCustom(colors = 'grey', labels = round(seq(0, max(as.numeric(shelter_spdf@data$tot_beds), na.rm = TRUE), length.out = 5)),
#                 sizes = round(seq(0, max(circle_marker_radius_values*2, na.rm = TRUE), length.out = 5)), title = "Number of beds")


######## adding the food banks ######
food_and_shelter_map = shelter_map %>% addMarkers(data = food_banks_spdf, label = lapply(paste(sep ='<br/>',"Food Bank", food_banks_spdf$org, food_banks_spdf$phone), HTML))

#add an outline of long beach here
food_and_shelter_map_with_outline = food_and_shelter_map %>% addPolygons(data = lb_city, fillOpacity = 0,
                                                                         stroke = TRUE, color = 'black', opacity = 1)

######## Making the map from the score ########
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title = NULL, position = 'bottomright'){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border-radius: 50%")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px; margin-top: 0px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title,
                   position = position))
}


SHELTER_COLOR = 'green'
SHELTER_RADIUS = 100
FOOD_BANK_COLOR = 'blue'
FOOD_BANK_RADIUS = 100
LOCATION_STROKE = FALSE
LOCATION_OPACITY = .8
LOCATION_LEGEND_RADIUS = 10
LOCATION_LEGEND_OPACITY = 1
TRACT_PAL = 'RdYlGn'
TRACT_OPACITY = .7
tract_color_vals = get_quantile(lb_all@data$score, quantile_bins = 10)
past_tract_color_vals = get_quantile(lb_past_all@data$score, quantile_bins = 10)
future_tract_color_vals = get_quantile(lb_all@data$pred_score, quantile_bins = 10)

tract_pal = colorFactor(
  palette = TRACT_PAL, 
  domain = tract_color_vals,
  reverse = TRUE
)



u_tract_color_vals = unique(tract_color_vals[!is.na(tract_color_vals)])
legend_val = u_tract_color_vals[order(u_tract_color_vals)][c(1,length(u_tract_color_vals))]

map_all = map %>% addMarkers(group = 'Clear', lng = 10, lat = 10) %>% 
  addMapPane('risk_tiles', zIndex = 410) %>% addMapPane('shelters', zIndex = 420) %>%
  addMapPane('foodbanks', zIndex = 430) %>%
  addPolygons(data = lb_all, fillColor = ~tract_pal(tract_color_vals), popup = lb_all@data$label, stroke = T,
                              fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
                              highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                                  bringToFront = FALSE, dashArray = FALSE),
                              group = '2018',options = pathOptions(pane = "risk_tiles")) %>% 
  addPolygons(data = lb_past_all, fillColor = ~tract_pal(past_tract_color_vals), popup = lb_past_all@data$label, stroke = T,
              fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
              highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                  bringToFront = FALSE, dashArray = FALSE),
              group = '2016', options = pathOptions(pane = "risk_tiles")) %>% 
  addPolygons(data = lb_past_all, fillColor = ~tract_pal(future_tract_color_vals), popup = lb_all@data$pred_label, stroke = T,
              fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
              highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                  bringToFront = FALSE, dashArray = FALSE),
              group = '2020', options = pathOptions(pane = "risk_tiles")) %>% 
  addCircles(data = shelter_spdf, radius = SHELTER_RADIUS, 
                   popup = lapply(shelter_spdf@data$label, HTML), stroke = LOCATION_STROKE, 
                   label = shelter_spdf@data$org,
                   color = SHELTER_COLOR,
                   fillOpacity = LOCATION_OPACITY, 
             options = pathOptions(pane = "shelters")) %>%
  addCircles(data = food_banks_spdf, 
             popup = lapply(paste(sep ='<br/>',"Food Bank", food_banks_spdf$org, food_banks_spdf$phone), HTML),
             label = food_banks_spdf@data$org,
             stroke = LOCATION_STROKE,
             radius = FOOD_BANK_RADIUS,
             color = FOOD_BANK_COLOR,
             fillOpacity = LOCATION_OPACITY,
             options = pathOptions(pane = "foodbanks")) %>%
  addLegend(colors = tract_pal(legend_val[length(legend_val):1]), opacity = 0.7, position = 'bottomright',
            title = 'Risk factors level', labels = c('High (90%ile)', 'Low (10%ile)')) %>%
  addLegendCustom(colors = c(SHELTER_COLOR, FOOD_BANK_COLOR), labels = c('Shelters', 'Food banks'),
                  sizes = LOCATION_LEGEND_RADIUS, title = NULL, opacity = LOCATION_LEGEND_OPACITY,
                  position = 'bottomright') %>% 
  addLayersControl(baseGroups = c('Clear', '2016', '2018', '2020')) %>%
  showGroup('2018') %>% hideGroup('Clear')








