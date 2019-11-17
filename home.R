# home page


# county_list = toTitleCase(gsub("([^,]+)(,)([[:print:]]+)", "\\3 county, \\1", county.fips$polyname))

######## REading in codebook #########


#reading in codebook to translate the names of the acs vars to their name in the dataset
# if(!exists('codebook')){
  codebook = read.csv('variable_mapping.csv', stringsAsFactors = FALSE)
# }

data_code_book = codebook[!duplicated(paste0(codebook$risk_factor_name, codebook$metric_category)),]

####### Constants #########


VIOLENCE_CHOICES = data_code_book$risk_factor_name[grep('at-risk', data_code_book$metric_category, ignore.case = TRUE)]
HEALTH_CHOICES = data_code_book$risk_factor_name[grep('health', data_code_book$metric_category, ignore.case = TRUE)]
ECONOMIC_CHOICES = data_code_book$risk_factor_name[grep('economic', data_code_book$metric_category, ignore.case = TRUE)]
QOL_CHOICES = data_code_book$risk_factor_name[grep('qol', data_code_book$metric_category, ignore.case = TRUE)]



#Understanding the year range that should be available in the app
#since cdc data only goes back to 2016, we are cutting the year range off at 2016 minimum
YEAR_RANGE = c(2016,2018)


#loading one cdc data to know what cities we have cdc data on
cdc_2018 = readRDS('data_tables/cdc_2018.rds')
cities_cdc = paste0(cdc_2018$placename[!duplicated(cdc_2018$placename)], ' ', cdc_2018$stateabbr[!duplicated(cdc_2018$placename)])
states_cdc = unique(cdc_2018$stateabbr)

######## custom JS ######
redirect_jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = '?map';});"


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
  if(!ret_100_ile){ 
    if(length(quant_val) == 1 & quant_val == 100){quant_val = (1 - 1/quantile_bins)*100}else{
      quant_val[quant_val == 100] = unique(quant_val)[order(-unique(quant_val))][2]
      }
    }
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





############# map functions ########
#Given therisk vars, risk weights, spdf, and codebook, calculates the overall risk factor score
calculate_score = function(risk_vars, risk_weights, spdf, data_code_book){
  
  if(length(risk_vars) != length(risk_weights)){
    warning("risk vars and risk weights are not the same length")
    return(NULL)
  }
  if(!all(risk_vars %in% data_code_book$risk_factor_name)){
    warning("some var names are not in the codebook")
    return(NULL)
  }
  
  
  
  data_code_book = data_code_book[order(data_code_book$Name),]
  risk_dataset = data.frame(risk_vars, risk_weights, stringsAsFactors = FALSE)[order(risk_vars),]
  risk_dataset$var_code = data_code_book$Name[in_match_order(data_code_book$risk_factor_name, risk_dataset$risk_vars)]
  
  #handles the edge case where only one variable is involved in the scoring.
  if(length(risk_vars) < 2) {
    score_var = as.numeric(spdf@data[,risk_dataset$var_code])
    if(is.null(spdf$GEOID)) return(data.frame(score = min_max_vec(score_var, na.rm = TRUE), stringsAsFactors = FALSE))
    return(data.frame(GEOID = spdf$GEOID, score = min_max_vec(score_var, na.rm = TRUE), stringsAsFactors = FALSE))
  }
  #get the vars from the spdf that are valuable here and order them in the same was as the risk_dataset
  score_vars = tryCatch(spdf@data[,which(colnames(spdf@data) %in% risk_dataset$var_code)], 
                        error = function(e) spdf[,which(colnames(spdf) %in% risk_dataset$var_code)])
  score_vars = score_vars[,risk_dataset$var_code]
  
  #converting each column in score_vars to numeric
  for(n in seq_len(ncol(score_vars))){
    score_vars[,n] = as.numeric(score_vars[,n])
  }
  
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
  if(is.null(spdf$GEOID)) return(data.frame(score = min_max_vec(score, na.rm = TRUE), stringsAsFactors = FALSE))
  return(data.frame(GEOID = spdf$GEOID, score = min_max_vec(score, na.rm = TRUE), stringsAsFactors = FALSE))
  
}
#making the label for the map from the risk_vars, spdf, codebook, and quantile bins
make_label_for_score = function(risk_vars, spdf, data_code_book, quantile_bins = 10, front_name = FALSE){
  label_list = NULL
  #cleaning up the risk_var names
  risk_var_cats = unique(gsub('([[:alpha:]]+)(_[[:print:]]*)', '\\1', names(risk_vars)))
  risk_var_cats_name_conversion = data.frame(cats = risk_var_cats, display_names = paste0(tools::toTitleCase(risk_var_cats), ' factors'), stringsAsFactors = FALSE)
  risk_var_cats_name_conversion$display_names[risk_var_cats_name_conversion$cats == 'qol'] = "Quality of life factors"
  
  if(length(risk_var_cats) > 1){
    risk_cats_scores = data.frame(array(dim = c(nrow(spdf@data), length(risk_var_cats))))
    colnames(risk_cats_scores) = risk_var_cats
    for(risk_cat in risk_var_cats){
      interest_vars = risk_vars[grep(risk_cat, names(risk_vars))]
      interest_var_names = data_code_book$Name[in_match_order(data_code_book$risk_factor_name, interest_vars)] #works
      min_max_vars = data.frame(spdf@data[,interest_var_names])
      for(n in seq_len(ncol(min_max_vars))){
        min_max_vars[,n] = min_max_vec(min_max_vars[,n], na.rm = TRUE)
      }
      risk_cats_scores[,risk_cat] = rowSums(min_max_vars)
    }
    risk_cats_quantiles = risk_cats_scores
    for(n in seq_len(ncol(risk_cats_quantiles))){
      risk_cats_quantiles[,n] = get_quantile(risk_cats_scores[,n], quantile_bins = quantile_bins)
    }
  }
  
  for(row_ind in 1:nrow(spdf@data)){
    
    label_string = NULL
    if(length(risk_var_cats) < 2){
      for(n in seq_along(risk_vars)){
        
        if(front_name){
        label_string = c(label_string, paste0('<small class = "no_small_screen">',data_code_book$front_name[data_code_book$risk_factor_name == risk_vars[n]][1], ' :', 
                                              round(spdf@data[row_ind,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]]), '% ', 
                                              get_quantile(spdf@data[,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]], quantile_bins = quantile_bins)[row_ind], '%ile)</small>'))
        }else{
          label_string = c(label_string, paste0('<small class = "no_small_screen">', round(spdf@data[row_ind,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]]), '% ', 
                                                data_code_book$back_name[data_code_book$risk_factor_name == risk_vars[n]][1], ' (',
                                                get_quantile(spdf@data[,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]], quantile_bins = quantile_bins)[row_ind], '%ile)</small>'))
        }
        

      }
      full_label = c(paste0("<b>Overall ", risk_var_cats_name_conversion$display_names[1], " metric: ", get_quantile(spdf@data$score[row_ind], quantile_bins = quantile_bins, compare_vec = spdf@data$score),
                            "%ile</b>", '<br class = "no_big_screen">'), label_string)
      label_list = c(label_list, paste(full_label, collapse = '<br class = "no_small_screen">')) 
      
    }else{
      for(risk_cat in risk_var_cats){
        # risk_cat = risk_var_cats[1]
        cat_score = risk_cats_quantiles[row_ind,risk_cat]
        label_string = c(label_string, paste0('<i>', risk_var_cats_name_conversion$display_names[risk_var_cats_name_conversion$cats == risk_cat],
                                              ': ', as.character(cat_score), '%ile</i><br class = "no_big_screen">'))
        
        interest_vars = risk_vars[grep(risk_cat, names(risk_vars))]
        if(front_name){
          display_var_names = data_code_book$front_name[in_match_order(data_code_book$risk_factor_name, interest_vars)]
        }else{
          display_var_names = data_code_book$back_name[in_match_order(data_code_book$risk_factor_name, interest_vars)]
        }
        interest_var_names = data_code_book$Name[in_match_order(data_code_book$risk_factor_name, interest_vars)] #works
        for(sub_vars_ind in seq_len(length(interest_vars))){
          if(front_name){
            label_string = c(label_string, 
                             paste0('<small class = "no_small_screen">', display_var_names[sub_vars_ind], ': ', 
                                    round(spdf@data[row_ind,interest_var_names[sub_vars_ind]]), '% (', 
                                    get_quantile(spdf@data[,interest_var_names[sub_vars_ind]], quantile_bins = quantile_bins)[row_ind], '%ile)', '</small>')
            )
          }else{
            label_string = c(label_string, 
                             paste0('<small class = "no_small_screen">',  
                                    round(spdf@data[row_ind,interest_var_names[sub_vars_ind]]), '% ', display_var_names[sub_vars_ind], ' (',
                                    get_quantile(spdf@data[,interest_var_names[sub_vars_ind]], quantile_bins = quantile_bins)[row_ind], '%ile)', '</small>')
            )
          }
        }
      }
      full_label = c(paste0("<b>Overall risk metric: ", 
                            get_quantile(spdf@data$score[row_ind], quantile_bins = quantile_bins, compare_vec = spdf@data$score), "%ile</b>", '<br class = "no_big_screen">'), label_string)
      label_list = c(label_list, paste(full_label, collapse = '<br class = "no_small_screen">'))
    }
  }
  return(label_list)
}
#given an spdf, codebook, risk_vars, risk_weights, and quantiles, returns the updated spdf with the metric score and full label as new vars
make_full_spdf = function(spdf, data_code_book, risk_vars, risk_weights, quantile_bins){
  #making sure the variables of interest are numeric
  for(n in risk_vars){
    spdf@data[,colnames(spdf@data) == data_code_book$Name[data_code_book$risk_factor_name == n]] = 
      as.numeric(spdf@data[,colnames(spdf@data) == data_code_book$Name[data_code_book$risk_factor_name == n]])
  }
  
  spdf@data = merge(spdf@data, calculate_score(risk_vars, risk_weights, spdf, data_code_book), by = 'GEOID')
  
  spdf@data$label = make_label_for_score(risk_vars, spdf, data_code_book, quantile_bins)
  
  return(spdf)
}

#given the spdf, returns the loc_dist_matrix
get_loc_dist_matrix = function(spdf, MAX_LOC_DIST = 1){
  #initializing the matrix
  loc_dist_matrix = matrix(0, nrow = nrow(spdf@data), ncol = nrow(spdf@data))
  loc_matrix = rgeos::gTouches(spdf, byid = TRUE)
  
  
  #iterates through all blocks of 1 - MAX_BLOCK_DIST away, identifies which iteration it was picked up, and marks that number into matrix
  #this will likely take hours (lol, takes 1 second).
  for(loc_it_count in 1 : ncol(loc_dist_matrix)){
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
  
  colnames(loc_dist_matrix) = spdf@data$GEOID
  rownames(loc_dist_matrix) = spdf@data$GEOID
  
  loc_dist_matrix = 1/loc_dist_matrix
  loc_dist_matrix[loc_dist_matrix > 1] = 0
  
  return(loc_dist_matrix)
  
}
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
#given the spdf, risk_vars, and the codebook, returns the independent vars for predicting
get_ind_vars_for_model = function(spdf, risk_vars, data_code_book, MAX_LOC_DIST = 1){
  x_vars = spdf@data[data_code_book$Name[data_code_book$risk_factor_name %in% risk_vars]]
  #making sure all of the columns are numeric
  for(n in seq_len(ncol(x_vars))) x_vars[,n] = as.numeric(x_vars[,n])
  
  loc_dist_matrix = get_loc_dist_matrix(spdf, MAX_LOC_DIST)
  ids = spdf@data$GEOID
  ind_vars = data.frame(GEOID = ids, x_vars, stringsAsFactors = FALSE) #all the ind vars and GEOID id tag
  
  neib_matrix = neib_avg_scores(x_vars, ids, loc_dist_matrix, na.rm = TRUE)
  
  big_ind_dat = merge(ind_vars, neib_matrix, by = 'GEOID')
  
  return(big_ind_dat)
}
#given the full spdf hash, inputs list, risk_vars, and codebook, returns the 1) raw predicted scores, 2) pred score quantiles, and 3) labels for the pred map
get_predicted_scores_and_labels = function(city_all_spdf_hash, inputs, risk_vars, risk_weights, data_code_book, quantile_bins = 10, MAX_LOC_DIST = 1){
  
  ind_vars = get_ind_vars_for_model(city_all_spdf_hash[[as.character(inputs$year_range[1])]], risk_vars, data_code_book, MAX_LOC_DIST)
  dep_dat = calculate_score(risk_vars, risk_weights, city_all_spdf_hash[[as.character(inputs$year_range[2])]], data_code_book)
  
  
  #min_max_scaling vars
  ind_vars[,2:ncol(ind_vars)] = sapply(ind_vars[,2:ncol(ind_vars)], min_max_vec)
  dep_dat[,2] = min_max_vec(vec = dep_dat[,2])
  
  if(identical(ind_vars[,1], dep_dat[,1])){
    model_dat = data.frame(score = dep_dat[,2], ind_vars[,-1])
  }else{warning("ids don't match up between dep_dat and ind_vars")}
  
  score.lm = lm(score ~ ., data = model_dat)
  
  predict_dat = get_ind_vars_for_model(city_all_spdf_hash[[as.character(inputs$year_range[2])]],
                                       risk_vars, data_code_book)[,-1] 
  predict_dat[,1:ncol(predict_dat)] = sapply(predict_dat[,1:ncol(predict_dat)], min_max_vec)
  
  pred_score = predict(score.lm, newdata = predict_dat)
  last_real_score = dep_dat[,2]
  
  #to have the overall risk metrics match up with the existing year's risk metrics, we scale the numbers such that they, on average, match up to the existing overall metrics
  division_factor = sum(pred_score, na.rm = TRUE)/sum(last_real_score, na.rm = TRUE)
  pred_score_fixed = pred_score/division_factor
  pred_score_fixed[pred_score_fixed < 0] = 0
  
  pred_score_quantile = get_quantile(pred_score_fixed, quantile_bins = quantile_bins)
  
  pred_score_label = paste0("Predicted overall risk metric 2020: <br/><b>", pred_score_quantile, "%ile</b>")
  
  #checking the absolute error rate. since the scores are between 0 and 1, this shows the %error of the scores
  print(summary(abs(last_real_score[!is.na(last_real_score)] - predict(score.lm, newdata = ind_vars[!is.na(last_real_score),]))))
  
  return(list(raw_score = pred_score_fixed, score_quantile = pred_score_quantile, label = pred_score_label))
}
#given present_spdf with future predictions & labels, past_spdf, inputs, pallette info for tracts, and quantile bins, returns a leaflet map
make_map = function(present_spdf, past_spdf, inputs, TRACT_PAL = 'RdYlGn', TRACT_OPACITY = 0.7, quantile_bins = 10){
  lon_med = mean(present_spdf@bbox[1,])
  lat_med = mean(present_spdf@bbox[2,])
  
  map <- leaflet(options = leafletOptions(minZoom = 10, zoomControl = FALSE)) %>% 
    # add ocean basemap
    # addProviderTiles(providers$Esri.OceanBasemap) %>%
    # add another layer with place names
    addProviderTiles(providers$Hydda.Full) %>%
    # focus map in a certain area / zoom level
    setView(lng = lon_med, lat = lat_med, zoom = 12) 
  
  TRACT_PAL = 'RdYlGn'
  TRACT_OPACITY = .7
  tract_color_vals = get_quantile(present_spdf@data$score, quantile_bins = quantile_bins)
  past_tract_color_vals = get_quantile(past_spdf@data$score, quantile_bins = quantile_bins)
  future_tract_color_vals = get_quantile(present_spdf@data$pred_score, quantile_bins = quantile_bins)
  
  tract_pal = colorFactor(
    palette = TRACT_PAL, 
    domain = tract_color_vals,
    reverse = TRUE
  )
  
  u_tract_color_vals = unique(tract_color_vals[!is.na(tract_color_vals)])
  legend_val = u_tract_color_vals[order(u_tract_color_vals)][c(1,length(u_tract_color_vals))]
  
  map_all = map %>% addMarkers(group = 'Clear', lng = 10, lat = 10) %>% 
    addMapPane('risk_tiles', zIndex = 410) %>%
    addPolygons(data = present_spdf, fillColor = ~tract_pal(tract_color_vals), popup = present_spdf@data$label, stroke = T,
                fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
                highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                    bringToFront = FALSE, dashArray = FALSE),
                group = as.character(inputs$year_range[2]),options = pathOptions(pane = "risk_tiles")) %>% 
    addPolygons(data = past_spdf, fillColor = ~tract_pal(past_tract_color_vals), popup = past_spdf@data$label, stroke = T,
                fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
                highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                    bringToFront = FALSE, dashArray = FALSE),
                group = as.character(inputs$year_range[1]), options = pathOptions(pane = "risk_tiles")) %>% 
    addPolygons(data = present_spdf, fillColor = ~tract_pal(future_tract_color_vals), popup = present_spdf@data$pred_label, stroke = T,
                fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
                highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                    bringToFront = FALSE, dashArray = FALSE),
                group = as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])), options = pathOptions(pane = "risk_tiles")) %>% 
    addLegend(colors = tract_pal(legend_val[length(legend_val):1]), opacity = 0.7, position = 'bottomright',
              title = 'Risk factors level', labels = c('High (90%ile)', 'Low (10%ile)')) %>%
    # addLayersControl(baseGroups = c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]),
    #                                 as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])))) %>%
    showGroup(as.character(inputs$year_range[2])) %>% hideGroup('Clear')
  return(map_all)
}
###### Pass-through parameters #########

#these need to be converted into a list and saved as an rds file to be maintained throughout the journey
#check if file exists
# if(file.exists('inputs_outputs/home_inputs.rds')){
#   inputs = readRDS('inputs_outputs/home_inputs.rds')
# }else{
  inputs = hash()
  inputs[['cities']] <- ''
  inputs[['year_range']] <- YEAR_RANGE
  inputs[['health_factors']] <- ''
  inputs[['economics_factors']] <- ''
  inputs[['at-risk_factors']] <- ''
  inputs[['qol_factors']] <- ''
# }

  
location = inputs[['cities']]
violence_risk_factors = inputs[['at-risk_factors']]
health_risk_factors = inputs[['health_factors']]
economic_factors = inputs[['economics_factors']]
qol_factors = inputs[['qol_factors']]
year_range = inputs[['year_range']]

# print(inputs)

##### UI ##########

output$pageStub <- renderUI(tagList(
  tags$head(tags$script(redirect_jscode)),
  tags$head(rel = "stylesheet", type = 'text/css', href = 'https://fonts.googleapis.com/css?family=Montserrat|Open+Sans|Raleway|Roboto|Roboto+Condensed&display=swap'),
  includeCSS('www/sreen_size.css'),
  useShinyjs(),
########### splash up front ###########
  fluidRow(
    div(class = 'center_wrapper',
    div(class = 'splash_front',
      h1('Understand the health of your neighborhood', class = "splash_text"),
      HTML('<h4 class = "splash_text smaller_header">The needs of a city are different in each neighborhood. Some communities struggle with poverty, others with health problems.',
           'Understanding the specific issues in each neighborhood can improve how a city allocates services.',
         'Study the numbers that reflect issues in your community'),
      HTML('<h5 class = "splash_text smaller_header">Data from the CDC and US Census. All metrics are scored from low-issue (0%ile) to high-issue (90%ile).</h5>',
           '<h5 class = "splash_text smaller_header">For comments, questions, and custom-mapping requests, contact Albert Gehami at 
           <a href = "mailto: gehami@alumni.stanford.edu">gehami@alumni.stanford.edu</a></h5>')

    )
  )
  ),


###### Inputs and descriptions ##########  
  div(class = 'center_wrapper',
  fluidRow(class = 'splash_front',
    div(class = "on_same_row", 
        selectizeInput('state', HTML('Filter by state and search for your city below'), choices = c(states_cdc[order(states_cdc)]), 
                       options = list(
                         onInitialize = I(paste0('function() { this.setValue(""); }'))
                       )),
        uiOutput('select_city')
           # selectizeInput(
           #   'city', HTML('Search for your city </br><small>(Largest 500 US cities only)</small>'), choices = c(cities_cdc), multiple = FALSE,
           #   options = list(
           #     placeholder = 'Enter City name',
           #     onInitialize = I(paste0('function() { this.setValue("',paste(location, collapse = ','),'"); }')),
           #     maxOptions = 10
           #   )
           # )
           )
  ),
  fluidRow(column(12,
                  h3('Select a preset group of metrics'),
                  uiOutput('preset_buttons'),
                  h3('Or customize your metrics of interest'),

                  
                  div(class = "factor_selector", 
                    dropdownButton(
                    checkboxInput('all_health_factors', "Select all"),
                    checkboxGroupInput(
                      'health_factors', 'Community health factors',
                      choices = HEALTH_CHOICES,
                      selected = health_risk_factors
                    ),
                    label = 'Community health factors',
                    circle = FALSE
                  )),
                  div(class = "factor_selector",
                    dropdownButton(
                    checkboxInput('all_economic_factors', "Select all"),
                    checkboxGroupInput(
                      'economic_factors', 'Economic factors',
                      choices = ECONOMIC_CHOICES,
                      selected = economic_factors
                    ),
                    label = 'Economic factors',
                    circle = FALSE
                  )),
                  div(class = "factor_selector",
                      dropdownButton(
                        checkboxInput('all_violence_factors', "Select all", value = FALSE),
                        checkboxGroupInput(
                          'violence_factors', 'At-risk factors',
                          choices = VIOLENCE_CHOICES, selected = violence_risk_factors
                        ),
                        label = 'At-risk factors',
                        circle = FALSE
                  )),
                  div(class = "factor_selector",
                    dropdownButton(
                    checkboxInput('all_qol_factors', "Select all"),
                    checkboxGroupInput(
                      'qol_factors', 'Other quality-of-life factors',
                      choices = QOL_CHOICES, 
                      selected = qol_factors
                    ),
                    label = 'Quality-of-life factors',
                    circle = FALSE
                  )),
                  # sliderInput('year_range', 'Which years should we look at?',
                  #             YEAR_RANGE[1], YEAR_RANGE[2], value = year_range),
                  
                  actionBttn('map_it', 'Map it'),
                  # actionButton('map_it', 'Map it'),
                  uiOutput('input_warning'),
                  uiOutput('loading_sign')

                )
           )
  )
))



###### begin server code #########

# observeEvent(input$year_range,{
#   print(input$year_range)
# })


######### Allowing to filter by state ###########

output$select_city <- renderUI({
  selectizeInput(
    'city', HTML('Search for your city </br><small>(Largest 500 US cities only)</small>'), choices = 
      c(cities_cdc[grep(paste0(input$state, '$'), cities_cdc)])[order(c(cities_cdc[grep(paste0(input$state, '$'), cities_cdc)]))], multiple = FALSE,
    options = list(
      placeholder = 'Enter City name',
      onInitialize = I(paste0('function() { this.setValue("',paste(location, collapse = ','),'"); }')),
      maxOptions = 1000
    )
  )
})






######### Setting up the preset metrics buttons ###########

preset_options = gsub('\\.', ' ', gsub('^Preset_[0-9]+_', '', 
                                       grep('^Preset', colnames(data_code_book), value = TRUE, ignore.case = TRUE)))

output$preset_buttons <- renderUI(lapply(preset_options, function(i){
                                       actionBttn(inputId = i, label = i, size = 'sm')
                                     }))

clicked_preset <- reactiveVal(FALSE)

#if the first preset is clicked
observeEvent(input[[preset_options[1]]], {
  
  clicked_preset(TRUE)
  
  violence_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_1_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                data_code_book$metric_category == 'at-risk']
  health_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_1_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                              data_code_book$metric_category == 'health']
  economic_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_1_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                              data_code_book$metric_category == 'economic']
  qol_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_1_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                              data_code_book$metric_category == 'qol']
  
  
  updateCheckboxGroupInput(session, 'violence_factors', selected = violence_selected)
  updateCheckboxGroupInput(session, 'health_factors', selected = health_selected)
  updateCheckboxGroupInput(session, 'economic_factors', selected = economic_selected)
  updateCheckboxGroupInput(session, 'qol_factors', selected = qol_selected)
  
  shinyjs::click('map_it')
  
  
})
#if the second preset is clicked
observeEvent(input[[preset_options[2]]], {
  
  clicked_preset(TRUE)
  
  
  violence_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_2_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                        data_code_book$metric_category == 'at-risk']
  health_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_2_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                      data_code_book$metric_category == 'health']
  economic_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_2_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                        data_code_book$metric_category == 'economic']
  qol_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_2_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                   data_code_book$metric_category == 'qol']
  
  
  updateCheckboxGroupInput(session, 'violence_factors', selected = violence_selected)
  updateCheckboxGroupInput(session, 'health_factors', selected = health_selected)
  updateCheckboxGroupInput(session, 'economic_factors', selected = economic_selected)
  updateCheckboxGroupInput(session, 'qol_factors', selected = qol_selected)
  
  shinyjs::click('map_it')
  
  
})
#if the third preset is clicked
observeEvent(input[[preset_options[3]]], {
  
  clicked_preset(TRUE)
  
  violence_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_3_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                        data_code_book$metric_category == 'at-risk']
  health_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_3_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                      data_code_book$metric_category == 'health']
  economic_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_3_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                        data_code_book$metric_category == 'economic']
  qol_selected = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_3_', colnames(data_code_book), ignore.case = TRUE)] %in% 1 &
                                                   data_code_book$metric_category == 'qol']
  
  
  updateCheckboxGroupInput(session, 'violence_factors', selected = violence_selected)
  updateCheckboxGroupInput(session, 'health_factors', selected = health_selected)
  updateCheckboxGroupInput(session, 'economic_factors', selected = economic_selected)
  updateCheckboxGroupInput(session, 'qol_factors', selected = qol_selected)
  
  shinyjs::click('map_it')
  
  
})




######### Setting up the select all checkboxes and their server code ##########


#checkbox server code to make sure only one "mental health diagnoses" is checked at a time
#since we have mental health in two places, we are making sure that if you check one then you uncheck the other one

observeEvent(input$violence_factors, {
  if(length(input$health_factors) == 0){}else{
  print(input$health_factors)
  print(input$violence_factors)
  if(any(grepl('Mental health diagnoses', input$health_factors)) & any(grepl('Mental health diagnoses', input$violence_factors))){
    print('chaning')
    updateCheckboxGroupInput(session, 'health_factors', selected = input$health_factors[grep('Mental health diagnoses', input$health_factors, invert = TRUE)])
  }}
})
observeEvent(input$health_factors, {
  if(length(input$violence_factors) == 0){}else{
  print(input$health_factors)
  print(input$violence_factors)
  if(any(grepl('Mental health diagnoses', input$health_factors)) & any(grepl('Mental health diagnoses', input$violence_factors))){
    print('chaning')
    updateCheckboxGroupInput(session, 'violence_factors', selected = input$violence_factors[grep('Mental health diagnoses', input$violence_factors, invert = TRUE)])
  }}
})




#this just tracks if they have actually pressed the "Select all button" at all.
select_all_tracker = reactiveVal(FALSE)


#violence
observeEvent(input$all_violence_factors, {
  if(input$all_violence_factors){
    updateCheckboxGroupInput(session, 'violence_factors', selected = VIOLENCE_CHOICES)
    select_all_tracker(TRUE)
  }
  if(!input$all_violence_factors){
    if(select_all_tracker()){
      updateCheckboxGroupInput(session, 'violence_factors', selected = character(0))
    }
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)

#health
observeEvent(input$all_health_factors, {
  if(input$all_health_factors){
    updateCheckboxGroupInput(session, 'health_factors', selected = HEALTH_CHOICES)
    select_all_tracker(TRUE)
  }
  if(!input$all_health_factors){
    if(select_all_tracker()){
      updateCheckboxGroupInput(session, 'health_factors', selected = character(0))
    }
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)


#economic
observeEvent(input$all_economic_factors, {
  if(input$all_economic_factors){
    updateCheckboxGroupInput(session, 'economic_factors', selected = ECONOMIC_CHOICES)
    select_all_tracker(TRUE)
  }
  if(!input$all_economic_factors){
    if(select_all_tracker()){
      updateCheckboxGroupInput(session, 'economic_factors', selected = character(0))
    }
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)


#qol
observeEvent(input$all_qol_factors, {
  if(input$all_qol_factors){
    updateCheckboxGroupInput(session, 'qol_factors', selected = QOL_CHOICES)
    select_all_tracker(TRUE)
  }
  if(!input$all_qol_factors){
    if(select_all_tracker()){
      updateCheckboxGroupInput(session, 'qol_factors', selected = character(0))
    }
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)









##### Reaction to save inputs and begin building the map ##########

output$loading_sign = NULL
output$input_warning <- renderUI(HTML("<h5>This process may take up to 60 seconds</h5>"))

observeEvent(input$map_it,{
  if(is.null(c(input$violence_factors, input$health_factors, input$economic_factors, input$qol_factors)) & !clicked_preset()){
    print("no factors present")
    output$input_warning <- renderUI(h5("Please select at least 1 risk factor from the 4 drop-down menus above", class = "warning_text"))
  }else if(is.null(input$city) | input$city == ''){
    output$input_warning <- renderUI(h5("Please select a city", class = "warning_text"))
  }else{
    shinyjs::disable('map_it')
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Recording inputs", value = 0)
    
    output$warn = NULL
    inputs = hash()
    inputs[['cities']] <- input$city
    # inputs[['year_range']] <- input$year_range
    inputs[['year_range']] <- YEAR_RANGE
    inputs[['health_factors']] <- input$health_factors
    inputs[['economics_factors']] <- input$economic_factors
    inputs[['at-risk_factors']] <- input$violence_factors
    inputs[['qol_factors']] <- input$qol_factors
    print(inputs)
    saveRDS(inputs, 'inputs_outputs/home_inputs.rds')
    # inputs = readRDS('inputs_outputs/home_inputs.rds')
    
    
    ###### opening files and doing the things ######
    ####### Reading in data ########
    
    #reading in the cdc data
    progress$set(message = "Loading CDC data", value = .05)
    if(!exists("cdc_hash")){cdc_hash = hash()}
    years = seq(inputs$year_range[1], inputs$year_range[2])
    for(year in years){
      if(!(year %in% keys(cdc_hash))){
        cdc_data = readRDS(paste0('data_tables/cdc_', as.character(year), '.rds'))
        colnames(cdc_data)[colnames(cdc_data) == 'tractfips'] = 'GEOID'
        cdc_hash[[as.character(year)]] = cdc_data
      }
    }
    
    
    
    
    #reading in the acs data. Note that the year of the data is actually the year before the key 
    #(i.e. acs_hash[['2018']] actually stores 2017 acs data), becuase the acs data is one year behind the cdc data. 
    progress$set(message = "Loading Census data", value = .1)
    if(!exists("acs_hash")){acs_hash = readRDS('data_tables/acs_dat_hash.rds')}

    #reading in the spatial data
    progress$set(message = "Loading maps", value = .15)
    if(!exists('trimmed_tracts')){trimmed_tracts = readRDS('data_tables/trimmed_tract_data.rds')}
    
    #reading in the tract_city_database
    if(!exists('tract_city_dictionary')){tract_city_dictionary = readRDS('data_tables/tract_city_dictionary.rds')}
    
    progress$set(message = "Loading maps", value = .20)
    
    #reading in codebook to translate the names of the acs vars to their name in the dataset
    # if(!exists('codebook')){
    #   codebook = read.csv('variable_mapping.csv', stringsAsFactors = FALSE)
    # }
    
    print("codebook")
    
    #get just the tracts from the cities that we care about
    city_tracts = tract_city_dictionary[inputs$cities] %>% values() %>% unlist() %>% as.character()
    
    #identifying which tracts to use
    tracts_map = trimmed_tracts[trimmed_tracts$GEOID %in% city_tracts,]
    
    
    

    ####### Contstants #########
    
    INITIAL_WEIGHTS = 1
    #percent of a variable that is allowed to be NA for me to keep it in the predictors dataset
    NA_TOL = .1
    QUANTILE_BINS = 10
    # 1/x_ij where x is number of blocks between block i and j (starting at 1), 0 if more than MAX_BLOCK_DIST away
    MAX_LOC_DIST = 1 #looking at neighbords directly next to tract
    
    TRACT_PAL = 'RdYlBu'
    TRACT_OPACITY = .7
    SLIDER_MIN = 0
    SLIDER_MAX = 10
    INITIAL_SLIDER_VALUE = 1
    MIN_SLIDER_STEP = 0.5
    param_hash = hash::copy(inputs)
    hash::delete(c('cities', 'year_range'), param_hash)
    data_factors = param_hash %>% values() %>% unlist()
    if(length(dim(data_factors)) > 0){
      data_factors = as.character(data_factors)
      names(data_factors) = rep(keys(param_hash), length(data_factors))
    }
    
    
    
    ######## Creating the initial map #########
    
    progress$set(message = "Cleaning data", value = .30)
    
    print('cleaning data')
    
    city_all_dat_hash = hash::hash() 
    for(year in inputs$year_range[1]:inputs$year_range[2]){
      acs_year = acs_hash[[as.character(year)]]
      acs_year = acs_year[acs_year$GEOID %in% city_tracts,]
      cdc_year = cdc_hash[[as.character(year)]]
      cdc_year = cdc_year[cdc_year$GEOID %in% city_tracts,]
      city_all_dat_hash[[as.character(year)]] = merge(cdc_year[!duplicated(cdc_year$GEOID),], acs_year[!duplicated(acs_year$GEOID),], by = 'GEOID')
    }
    
    city_all_spdf_hash = hash::hash()
    for(year in inputs$year_range[1]:inputs$year_range[2]){
      city_data = merge(tracts_map@data, city_all_dat_hash[[as.character(year)]], by = 'GEOID')
      city_spdf = tracts_map[tracts_map$GEOID %in% city_data$GEOID,]
      city_spdf = city_spdf[order(city_spdf$GEOID),]
      city_data = city_data[order(city_data$GEOID),]
      city_spdf@data = city_data
      city_all_spdf_hash[[as.character(year)]] = city_spdf
    }
    
    progress$set(message = "Developing metric scores", value = .35)
    
    #creating the scores
    risk_vars = data_factors[!duplicated(as.character(data_factors))]
    risk_weights = rep(INITIAL_WEIGHTS, length(risk_vars))
    # spdf = city_all_spdf_hash[['2018']]
    # #data_code_book = codebook[!duplicated(codebook$risk_factor_name),]
    quantile_bins = QUANTILE_BINS
    
    progress$set(message = paste0("Designing map of ", inputs$year_range[1]), value = .40)
    past_spdf = make_full_spdf(city_all_spdf_hash[[as.character(inputs$year_range[1])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS)
    
    progress$set(message = paste0("Designing map of ", inputs$year_range[2]), value = .50)
    present_spdf = make_full_spdf(city_all_spdf_hash[[as.character(inputs$year_range[2])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS)
    
    progress$set(message = paste0("Predicting map of ", inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])), value = .60)
    pred_list = get_predicted_scores_and_labels(city_all_spdf_hash, inputs, risk_vars, risk_weights, data_code_book, QUANTILE_BINS, MAX_LOC_DIST)
    present_spdf@data$pred_score = pred_list$raw_score
    present_spdf@data$pred_quantile = pred_list$score_quantile
    present_spdf@data$pred_label = pred_list$label
    
    
    
    progress$set(message = "Rendering maps", value = .70)
    initial_map = make_map(present_spdf, past_spdf, inputs, TRACT_PAL, TRACT_OPACITY, QUANTILE_BINS)
    
    
    ######### Saving the needed files for next page ########
    
    progress$set(message = "Finalizing information", value = .80)
    
    
    saveRDS(city_all_spdf_hash, file = 'inputs_outputs/city_all_spdf_hash.rds')
    saveRDS(data_code_book, file = 'inputs_outputs/data_code_book.rds')
    saveRDS(risk_vars, file = 'inputs_outputs/risk_vars.rds')
    saveRDS(data_factors, file = 'inputs_outputs/data_factors.rds')
    saveRDS(initial_map, file = 'inputs_outputs/initial_map.rds')
    saveRDS(past_spdf[1,], file = 'inputs_outputs/example_past_spdf.rds')
    
    #### Moving to next page #######
    
    progress$set(message = "Moving to results page", value = .90)
    
    shinyjs::enable('map_it')
    
    session$sendCustomMessage("mymessage", "mymessage")
    
    
  }
})





