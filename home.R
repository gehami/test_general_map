# home page
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


city_tract_map = readRDS('data_tables/All tracts in all US cities - state WY.rds')
all_cities = unlist(hash::keys(city_tract_map))
big_city_tract_map = readRDS('data_tables/tract_city_dictionary.rds')
big_cities = unlist(hash::keys(big_city_tract_map))

health_risk_factors = ''
economic_factors = ''
qol_factors = ''
violence_risk_factors = ''
location = ''

#Understanding the year range that should be available in the app
#since cdc data only goes back to 2016, we are cutting the year range off at 2016 minimum
YEAR_RANGE = c(2016,2018)


#loading one cdc data to know what cities we have cdc data on
# cdc_2018 = readRDS('data_tables/cdc_2018.rds')
# cities_cdc = paste0(cdc_2018$placename[!duplicated(cdc_2018$placename)], ' ', cdc_2018$stateabbr[!duplicated(cdc_2018$placename)])
# states_cdc = unique(cdc_2018$stateabbr)
cities_cdc = all_cities
states_cdc = unique(substr(all_cities, nchar(all_cities)-1, nchar(all_cities)))[order(unique(substr(all_cities, nchar(all_cities)-1, nchar(all_cities))))]

##Marking which datasets do not have data for smaller cities at the tract level
NO_SMALL_DATA = c('CDC')



##checking to make sure every city_state in cdc is also in county_tract_map... it does
# length(cities_cdc %in% all_cities) == length(cities_cdc) #confirmed



INITIAL_WEIGHTS = 1
#percent of a variable that is allowed to be NA for me to keep it in the predictors dataset
NA_TOL = .1
QUANTILE_BINS = 10
# 1/x_ij where x is number of blocks between block i and j (starting at 1), 0 if more than MAX_BLOCK_DIST away
MAX_LOC_DIST = 1 #looking at neighbords directly next to tract

TRACT_PAL = 'RdYlBu'
TRACT_OPACITY = .35
SLIDER_MIN = 0
SLIDER_MAX = 10
INITIAL_SLIDER_VALUE = 1
MIN_SLIDER_STEP = 0.5



INFO_POPUP_TEXT = 'The overall score combines all of the metrics you chose into one number for each neighborhood. You can calculate it by taking the average score of all the metrics you chose (shown below in small font).
Typically the highest scoring neighborhoods show the highest needs in the city according to the data and selected metrics. Learn more from the <a href = "?home">FAQ on the bottom of the home page.</a>'

#economic
PRESET_1_DESC_TEXT = 'Income, wealth, and poverty'
#medical
PRESET_2_DESC_TEXT = 'Medical health stats from the CDC'
#high needs
PRESET_3_DESC_TEXT = 'General needs for services across many factors'

######## custom JS ######
# Allows you to move people to a new page via the server
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
  if(all(is.na(vec))) return(NA)
  
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
#Given the risk vars, risk weights, spdf, and codebook, calculates the overall risk factor score
calculate_score = function(risk_vars, risk_weights, spdf, data_code_book, keep_nas = FALSE){
  
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
  if(keep_nas){
    for(n in seq_len(ncol(score_vars))){
      score_vars[is.na(score_vars[,n]),n] = -10000000
    }
  }else{
    #Alternatively, we can deal with NAs by just taking the average score from the other variables. I like this better for now and will do as such
    for(n in seq_len(ncol(score_vars))){
      score_vars[is.na(score_vars[,n]),n] = rowSums(score_vars[is.na(score_vars[,n]),], na.rm = TRUE) / rowSums(!is.na(score_vars[is.na(score_vars[,n]),]))
    }
  }
  
  score_mat = data.matrix(score_vars)
  
  #multiplying those scores by the weights and summing them. works
  score = score_mat %*% risk_dataset$risk_weights
  
  score[score < 0] = NA
  if(is.null(spdf$GEOID)) return(data.frame(score = min_max_vec(score, na.rm = TRUE), stringsAsFactors = FALSE))
  return(data.frame(GEOID = spdf$GEOID, score = min_max_vec(score, na.rm = TRUE), stringsAsFactors = FALSE))
  
}
#making the label for the map from the risk_vars, spdf, codebook, and quantile bins
make_label_for_score = function(risk_vars, spdf, data_code_book, quantile_bins = 10, front_name = FALSE, info_popup_text = ""){
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
      risk_cats_quantiles[,n] = suppressWarnings(get_quantile(risk_cats_scores[,n], quantile_bins = quantile_bins))
    }
  }
  
  for(row_ind in 1:nrow(spdf@data)){
    
    label_string = NULL
    if(length(risk_var_cats) < 2){
      for(n in seq_along(risk_vars)){
        
        if(front_name){
          label_string = c(label_string, paste0('<small class = "phone_popup">',data_code_book$front_name[data_code_book$risk_factor_name == risk_vars[n]][1], ' :', 
                                                round(spdf@data[row_ind,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]]), '% ', 
                                                suppressWarnings(get_quantile(spdf@data[,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]], quantile_bins = quantile_bins))[row_ind], '%ile)</small>'))
        }else{
          label_string = c(label_string, paste0('<small class = "phone_popup">', round(spdf@data[row_ind,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]]), '% ', 
                                                data_code_book$back_name[data_code_book$risk_factor_name == risk_vars[n]][1], ' (',
                                                suppressWarnings(get_quantile(spdf@data[,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]], quantile_bins = quantile_bins))[row_ind], '%ile)</small>'))
        }
        

      }
      full_label = paste0('<div class = "top-line-popup"><b>Neighborhood zipcodes: ', gsub('(^[0-9]+\\, [0-9]+\\, [0-9]+)(\\, [[:print:]]+)', '\\1', spdf$zipcodes[row_ind]), '</b></div>',
                          '<div class = "top-line-popup" onclick = "popupFunction()"><b>Overall ', tolower(risk_var_cats_name_conversion$display_names[1]), " metric: ", suppressWarnings(get_quantile(spdf@data$score[row_ind], quantile_bins = quantile_bins, compare_vec = spdf@data$score)),
                            "%ile</b>",
                            HTML('<div class = "info-popup">',
                                 '<i class="fa fa-info-circle"></i>',
                                 '</div>'), '</div>',
                            '<div class = "info-popuptext" id = "myInfoPopup" onclick = "popupFunction()">', info_popup_text, '</div>', paste(label_string, collapse = '<br>'))
                     label_list = c(label_list, full_label)
                     
      #                HTML('<div class = "info-popup" onclick = "popupFunction()">',
      #                     '<i class="fa fa-info-circle"></i>',
      #                     '<span class = "info-popuptext" id = "myInfoPopup">', info_popup_text, '</span>',
      #                     '</div>')), label_string)
      # label_list = c(label_list, paste(full_label, collapse = '<br>')) 
      
      
    }else{
      for(risk_cat in risk_var_cats){
        cat_score = risk_cats_quantiles[row_ind,risk_cat]
        label_string = c(label_string, paste0('<i>', risk_var_cats_name_conversion$display_names[risk_var_cats_name_conversion$cats == risk_cat],
                                              ': ', as.character(cat_score),
                                              '%ile</i>'
                                              ))
        
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
                             paste0(#'<small class = "no_small_screen">',
                                    '<small class = "phone_popup">',
                                    display_var_names[sub_vars_ind], ': ', 
                                    round(spdf@data[row_ind,interest_var_names[sub_vars_ind]]), '% (', 
                                    suppressWarnings(get_quantile(spdf@data[,interest_var_names[sub_vars_ind]], quantile_bins = quantile_bins))[row_ind], '%ile)', '</small>')
            )
          }else{
            label_string = c(label_string, 
                             paste0(#'<small class = "no_small_screen">',
                                    '<small class = "phone_popup">',
                                    round(spdf@data[row_ind,interest_var_names[sub_vars_ind]]), '% ', display_var_names[sub_vars_ind], ' (',
                                    suppressWarnings(get_quantile(spdf@data[,interest_var_names[sub_vars_ind]], quantile_bins = quantile_bins))[row_ind], '%ile)', '</small>')
            )
          }
        }
      }
      #this should work now
      full_label = paste0('<div class = "top-line-popup"><b>Neighborhood zipcodes: ', gsub('(^[0-9]+\\, [0-9]+\\, [0-9]+)(\\, [[:print:]]+)', '\\1', spdf$zipcodes[row_ind]), '</b></div>',
                          '<div class = "top-line-popup" onclick = "popupFunction()"><b>Overall risk metric: ', 
                            suppressWarnings(get_quantile(spdf@data$score[row_ind], quantile_bins = quantile_bins, compare_vec = spdf@data$score)), "%ile</b>",#, 
                            #'<br class = "no_big_screen">'
                            HTML('<div class = "info-popup">',
                                 '<i class="fa fa-info-circle"></i>',
                                 '</div>'), '</div>',
                            '<div class = "info-popuptext" id = "myInfoPopup" onclick = "popupFunction()">', info_popup_text, '</div>', paste(label_string, collapse = '<br>'))
      label_list = c(label_list, full_label)
    }
  }
  return(label_list)
}
#given an spdf, codebook, risk_vars, risk_weights, and quantiles, returns the updated spdf with the metric score and full label as new vars
make_full_spdf = function(spdf, data_code_book, risk_vars, risk_weights, quantile_bins, info_popup_text = ""){
  #making sure the variables of interest are numeric
  for(n in risk_vars){
    spdf@data[,colnames(spdf@data) == data_code_book$Name[data_code_book$risk_factor_name == n]] = 
      as.numeric(spdf@data[,colnames(spdf@data) == data_code_book$Name[data_code_book$risk_factor_name == n]])
  }
  
  spdf@data = merge(spdf@data, calculate_score(risk_vars, risk_weights, spdf, data_code_book), by = 'GEOID')
  
  spdf@data$label = make_label_for_score(risk_vars, spdf, data_code_book, quantile_bins, front_name = FALSE, info_popup_text = info_popup_text)
  
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
  
  #fail-safe in case a neighborhood 
  if(nrow(spdf@data) >= 2){
    neib_matrix = neib_avg_scores(x_vars, ids, loc_dist_matrix, na.rm = TRUE)
    
    big_ind_dat = merge(ind_vars, neib_matrix, by = 'GEOID')
    
  }else{
    big_ind_dat = ind_vars
  }
  
  return(big_ind_dat)
}
#given the full spdf hash, inputs list, risk_vars, and codebook, returns the 1) raw predicted scores, 2) pred score quantiles, and 3) labels for the pred map
get_predicted_scores_and_labels = function(city_all_spdf_hash, inputs, risk_vars, risk_weights, data_code_book, quantile_bins = 10, MAX_LOC_DIST = 1,
                                           info_popup_text = ""){
  
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
  
  pred_score_quantile = suppressWarnings(get_quantile(pred_score_fixed, quantile_bins = quantile_bins))
  
  pred_score_label = paste0('<div class = "top-line-popup"><b>Neighborhood zipcodes: ', gsub('(^[0-9]+\\, [0-9]+\\, [0-9]+)([[:print:]]+)', '\\1', city_all_spdf_hash[[as.character(inputs$year_range[2])]]$zipcodes), '</b></div>',
                            '<div class = "top-line-popup" onclick = "popupFunction()">',"<b>Predicted overall risk metric 2020: ", pred_score_quantile, "%ile</b>",
                            HTML('<div class = "info-popup">',
                                 '<i class="fa fa-info-circle"></i>',
                                 '</div>'), '</div>',
                            '<div class = "info-popuptext" id = "myInfoPopup" onclick = "popupFunction()">', info_popup_text, '</div>',
  '</div>','<br/>To avoid inaccurate predictions, we only display the predicted overall score from the metrics you chose.',
                            '<span class = "no_small_screen"> This does take into account any weights adjustments you made above.', 
                            'For example, if you adjusted the weights to 0 for all but one metric, then the predicted score will reflect the predicted value for just that one metric.',
                            '</span>')
  
  #checking the absolute error rate. since the scores are between 0 and 1, this shows the %error of the scores
  print(summary(abs(last_real_score[!is.na(last_real_score)] - predict(score.lm, newdata = ind_vars[!is.na(last_real_score),]))))
  
  return(list(raw_score = pred_score_fixed, score_quantile = pred_score_quantile, label = pred_score_label))
}
#given present_spdf with future predictions & labels, past_spdf, inputs, pallette info for tracts, and quantile bins, returns a leaflet map
make_map = function(present_spdf, past_spdf, inputs, TRACT_PAL = 'RdYlGn', TRACT_OPACITY = 0.7, quantile_bins = 10){
  lon_med = mean(present_spdf@bbox[1,])
  lat_med = mean(present_spdf@bbox[2,])
  
  map <- leaflet(options = leafletOptions(minZoom = 8, zoomControl = FALSE)) %>% 
    # add ocean basemap
    # addProviderTiles(providers$Esri.OceanBasemap) %>%
    # add another layer with place names
    # addProviderTiles(leaflet::providers$OpenStreetMap) %>%
    # addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    # addProviderTiles(leaflet::providers$OpenStreetMap.HOT) %>%
    # addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik) %>%
    
    addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
    # focus map in a certain area / zoom level
    setView(lng = lon_med, lat = lat_med, zoom = 12) 
  
  # TRACT_PAL = 'RdYlGn'
  # TRACT_OPACITY = .7
  tract_color_vals = suppressWarnings(get_quantile(present_spdf@data$score, quantile_bins = quantile_bins))
  past_tract_color_vals = suppressWarnings(get_quantile(past_spdf@data$score, quantile_bins = quantile_bins))
  future_tract_color_vals = suppressWarnings(get_quantile(present_spdf@data$pred_score, quantile_bins = quantile_bins))
  
  tract_pal = colorFactor(
    palette = TRACT_PAL, 
    domain = tract_color_vals,
    reverse = TRUE
  )
  
  u_tract_color_vals = unique(tract_color_vals[!is.na(tract_color_vals)])
  legend_val = u_tract_color_vals[order(u_tract_color_vals)][c(1,length(u_tract_color_vals))]
  
  map_all = map %>% addMarkers(group = 'Clear', lng = 10, lat = 10) %>% 
    addMapPane('risk_tiles', zIndex = 200) %>%
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
                group = as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])), options = pathOptions(pane = "risk_tiles")) 
  map_all = map_all %>% 
    addLegend(colors = tract_pal(legend_val[length(legend_val):1]), opacity = 0.7, position = 'bottomright',
              title = 'Need level', labels = c('High (90%ile)', 'Low (0%ile)')) %>%
    # addLayersControl(baseGroups = c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]),
    #                                 as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])))) %>%
    showGroup(as.character(inputs$year_range[2])) %>% hideGroup('Clear') %>% 
    hideGroup(as.character(inputs$year_range[1])) %>% #hiding the first year layer
    hideGroup(as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1]))) #hiding the future year layer

  return(map_all)
}

# ######## Debugging setup ###########

  # ###### Initial set-up #######
  # #libraries
  # library(shiny)
  # library(shinyWidgets)
  # # library(maps)
  # library(tools)
  # # library(tidyverse)
  # # library(tidycensus)
  # library(hash)
  # library(leaflet)
  # library(magrittr)
  # library(shinyBS)
  # library(sp)
  # library(rgeos)
  # library(shinyjs)
  # library(leaflet.minicharts)
  # library(rgdal)
  # #
  # #
  # inputs = readRDS('inputs_outputs/debug_inputs.rds')
  # 
  # 
  # #marking if it is a small city or not. If it's a small city then we need to remove all inputs that are from the CDC, since CDC does not have neighborhood data for small cities
  # small_city = FALSE
  # if(!(inputs$cities %in% big_cities)){
  #   small_city = TRUE
  #   print('small city')
  # }
  # 
  # ##removing any inputs that do not apply to small cities if this is a small city
  # if(small_city){
  #   remove_vars = data_code_book$risk_factor_name[data_code_book$Dataset %in% NO_SMALL_DATA]
  #   for(i in keys(inputs)){
  #     inputs[[i]] = inputs[[i]][which(!(inputs[[i]] %in% remove_vars))]
  #     if(length(inputs[[i]]) == 0) inputs[[i]] = NULL
  #   }
  # }
  # 
  # 
  # #saving inputs for debugging
  # # saveRDS(inputs, 'inputs_outputs/debug_inputs.rds')
  # 
  # ###### opening files and doing the things ######
  # ####### Reading in data ########
  # 
  # #reading in the cdc data if it is a big city
  # if(!small_city){
  #   # progress$set(message = "Loading CDC data", value = .05)
  #   if(!exists("cdc_hash")){cdc_hash = hash()}
  #   years = seq(inputs$year_range[1], inputs$year_range[2])
  #   for(year in years){
  #     if(!(year %in% keys(cdc_hash))){
  #       cdc_data = readRDS(paste0('data_tables/cdc_', as.character(year), '.rds'))
  #       colnames(cdc_data)[colnames(cdc_data) == 'tractfips'] = 'GEOID'
  #       cdc_hash[[as.character(year)]] = cdc_data
  #     }
  #   }
  # }else{
  #   print("Imma smol city so I don't need CDC data")
  # }
  # 
  # 
  # #reading in the acs data.
  # # progress$set(message = "Loading Census data", value = .1)
  # if(!exists("acs_hash")){acs_hash = readRDS('data_tables/acs_dat_hash.rds')}
  # 
  # #reading in the spatial data
  # # progress$set(message = "Loading maps", value = .15)
  # 
  # #First we need to identify the tracts in the city
  # if(!exists('city_tract_map')){city_tract_map = readRDS('data_tables/All tracts in all US cities - state WY.rds')}
  # tracts_in_city = city_tract_map[[inputs$cities]]
  # #then we need to identify which counties are connected to the city so we can pull those counties into the map
  # if(!exists('city_county_map'))city_county_map = readRDS('data_tables/city_to_county_hash.rds')
  # #pull all the counties the city is a part of
  # city_counties = city_county_map[[inputs$cities]]
  # #pulls the first county the city is a part of
  # county_map = readRDS(paste0('data_tables/All county shape data/', city_counties[1]))
  # #pulls additional counties the city is a part of
  # if(length(city_counties) > 1){
  #   for(n in 2 : length(city_counties)){
  #     county_map = rbind(county_map, readRDS(paste0('data_tables/All county shape data/', city_counties[n])))
  #   }
  # }
  # #makes the tract map for all the city
  # tracts_map = county_map[county_map$GEOID %in% tracts_in_city,]
  # 
  # 
  # #checking to make sure that my ACS data holds all tracts, it does. blessings on blessings
  # # all_tracts = hash::values(city_tract_map) %>% unlist() %>% unique()
  # # length(all_tracts %in% acs_year$GEOID) == length(all_tracts)
  # 
  # 
  # 
  # ####### Contstants #########
  # 
  # param_hash = hash::copy(inputs)
  # hash::delete(c('cities', 'year_range'), param_hash)
  # data_factors = param_hash %>% values() %>% unlist()
  # if(length(dim(data_factors)) > 0){
  #   data_factors = as.character(data_factors)
  #   names(data_factors) = rep(keys(param_hash), length(data_factors))
  # }
  # 
  # 
  # 
  # ######## final set-ups before initial map #########
  # 
  # # progress$set(message = "Cleaning data", value = .30)
  # 
  # 
  # city_all_dat_hash = hash::hash()
  # for(year in inputs$year_range[1]:inputs$year_range[2]){
  #   acs_year = acs_hash[[as.character(year)]]
  #   acs_year = acs_year[acs_year$GEOID %in% tracts_in_city,]
  #   if(!small_city){
  #     cdc_year = cdc_hash[[as.character(year)]]
  #     cdc_year = cdc_year[cdc_year$GEOID %in% tracts_in_city,]
  #     city_all_dat_hash[[as.character(year)]] = merge(cdc_year[!duplicated(cdc_year$GEOID),], acs_year[!duplicated(acs_year$GEOID),], by = 'GEOID')
  #   }
  #   else{
  #     city_all_dat_hash[[as.character(year)]] = acs_year[!duplicated(acs_year$GEOID),]
  #   }
  # }
  # 
  # city_all_spdf_hash = hash::hash()
  # for(year in inputs$year_range[1]:inputs$year_range[2]){
  #   city_data = merge(tracts_map@data, city_all_dat_hash[[as.character(year)]], by = 'GEOID')
  #   city_spdf = tracts_map[tracts_map$GEOID %in% city_data$GEOID,]
  #   city_spdf = city_spdf[order(city_spdf$GEOID),]
  #   city_data = city_data[order(city_data$GEOID),]
  #   city_spdf@data = city_data
  #   city_all_spdf_hash[[as.character(year)]] = city_spdf
  # }
  # 
  # # progress$set(message = "Developing metric scores", value = .35)
  # 
  # #creating the scores
  # risk_vars = data_factors[!duplicated(as.character(data_factors))]
  # risk_weights = rep(INITIAL_WEIGHTS, length(risk_vars))
  # # spdf = city_all_spdf_hash[['2018']]
  # # #data_code_book = codebook[!duplicated(codebook$risk_factor_name),]
  # quantile_bins = QUANTILE_BINS
  # 
  # 
  # ####### making initial map ######
  # 
  # 
  # # progress$set(message = paste0("Designing map of ", inputs$year_range[1]), value = .40)
  # past_spdf = make_full_spdf(city_all_spdf_hash[[as.character(inputs$year_range[1])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS, info_popup_text = INFO_POPUP_TEXT)
  # print(head(past_spdf@data[,-ncol(past_spdf@data)]))
  # 
  # # progress$set(message = paste0("Designing map of ", inputs$year_range[2]), value = .50)
  # present_spdf = make_full_spdf(city_all_spdf_hash[[as.character(inputs$year_range[2])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS, info_popup_text = INFO_POPUP_TEXT)
  # print(head(present_spdf@data[,-ncol(present_spdf@data)]))
  # 
  # 
  # # progress$set(message = paste0("Predicting map of ", inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])), value = .60)
  # pred_list = get_predicted_scores_and_labels(city_all_spdf_hash, inputs, risk_vars, risk_weights, data_code_book, QUANTILE_BINS, MAX_LOC_DIST, info_popup_text = INFO_POPUP_TEXT)
  # present_spdf@data$pred_score = pred_list$raw_score
  # present_spdf@data$pred_quantile = pred_list$score_quantile
  # present_spdf@data$pred_label = pred_list$label
  # 
  # 
  # 
  # # progress$set(message = "Rendering maps", value = .70)
  # initial_map = make_map(present_spdf, past_spdf, inputs, TRACT_PAL, TRACT_OPACITY, QUANTILE_BINS)








###### reactive-vals / Pass-through parameters #########

#these need to be converted into a list and saved as an rds file to be maintained throughout the journey
inputs = hash()
inputs[['cities']] <- NULL
inputs[['year_range']] <- YEAR_RANGE
inputs[['medical_factors']] <- NULL
inputs[['economics_factors']] <- NULL
inputs[['at-risk_factors']] <- NULL
inputs[['qol_factors']] <- NULL


#declaring reactive values
inputs_react = reactiveVal(inputs)
city_all_spdf_hash_react <- reactiveVal()
data_code_book_react <- reactiveVal()
risk_vars_react <- reactiveVal()
data_factors_react <- reactiveVal()
initial_map_react <- reactiveVal()
example_past_spdf_react <- reactiveVal()
present_spdf_react <- reactiveVal()
clicked_preset <- reactiveVal(FALSE)




# print(inputs)

##### UI ##########

output$pageStub <- renderUI(tagList(
  tags$head(tags$script(redirect_jscode)),
  tags$head(rel = "stylesheet", type = 'text/css', href = 'https://fonts.googleapis.com/css?family=Montserrat|Open+Sans|Raleway|Roboto|Roboto+Condensed&display=swap'),
  includeCSS('www/sreen_size.css'),
  useShinyjs(),
  uiOutput('current_page')
))

######## home page ui #######
output$current_page <- renderUI({
  fluidPage(
    div(class = 'home-page-div',
    ########### splash up front ###########

    div(class = 'center_wrapper',
        div(class = 'splash_front',
            h1('City Equity Map', class = "splash_text"),
            HTML('<h4 class = "splash_text smaller_header">Inequity across neighborhoods has become top of mind for city planners. Some face higher poverty rates while others struggle more with medical issues.',
                 '<h4 class = "splash_text smaller_header">Use this tool to <strong>map out economic, health and other needs across neighborhoods in your city</strong></h4>',
                 '<h4 class = splash_text smaller_header>City planners can <strong>improve service allocation</strong></h4>',
                 '<h4 class = splash_text smaller_header>Citizens can better <strong>understand the broader community</strong></h4>',
                 '<h4 class = splash_text smaller_header>Get to know the numbers behind your neighborhoods</h4>')
            
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
                   #added as a workaround to prevent the chrome autofill
                   HTML('<input style="display:none" type="text" name="address"/>'),
                   uiOutput('select_city')
               )
               #put code for video tutorial here
               # actionButton()
      ),
      fluidRow(column(12,
                      h3('Select a preset group of metrics'),
                      uiOutput('preset_buttons'),
                      h3('Or customize your metrics of interest'),
                      uiOutput('custom_metrics'),
                      actionBttn('map_it', 'Map custom metrics', size = 'sm'),
                      uiOutput('input_warning'),
                      uiOutput('loading_sign')
                      
      )
      )
  )
    ###### end of home-page-div ########
  ),
    ###### FAQ ###########
    div(class = 'splash_text smaller_header', id = 'FAQ',
        HTML('<h3>Frequently asked questions</h3>',
             '<h4><strong>Q: </strong>How do I navigate this page?</h4>',
             '<h5><strong>A: </strong><ol><li>Select your city from the city drop-down. You may type in your city or select the state and then the city. Due to privacy concerns, only the 500 US cities with the largest population are avaialable. Smaller city maps can be made on request.</li>
             <li>Select the metrics you are interested in studying. These can be economic factors such as unemployment or medical factors such as obesity. For a simple selection of metrics, click on one of the three presets. For a fully-custom map, select your metrics individually below the presets.</li>
             <li>Click the blue button to map the metrics and wait about 20-30 seconds for delivey. If it has finished loading but the screen is blank, wait an additional 20 seconds before refreshing, as the map may take some time to render.</li>
             </ol></h5>',
#             '<h4><strong>Q: </strong>Why does it take so long to load / why is my screen blank for so long?</h4>',
#             '<h5><strong>A: </strong>Running this app on a faster server would be expensive so the mapping process may take 10-20 seconds to show even after loading.</h5>',
             '<h4><strong>Q: </strong>What does this app help me with?</h4>',
             '<h5><strong>A: </strong>You\'ve got resources & services to allocate in your city. Where should you put those resources to hit the populations in most need of those services? This map shows where the data would point those resources. <i>The data can\'t capture everything about a neighborhood,</i> but it can help inform decisions on where to put resources.</h5>',
             '<h4><strong>Q: </strong>What does the map show?</h4>',
             '<h5><strong>A: </strong>It shows each census tract (loosely each neighborhood in a city) ranked from 0%ile to 90%ile based on the metrics you choose. It shows what neighborhoods seem to be better off and which need more assistance. Metrics include unemployment rates, obesity rates, and low education rates (% of adults with no diploma).</h5>',
             '<h4><strong>Q: </strong>What does "0%ile" or "90%ile" mean?</h4>',
             '<h5><strong>A: </strong>"0%ile" means that 90% or more of the other neighborhoods in the city have a higher score. "90%ile" means less than 10% of the other neighborhoods have a higher score.</h5>',
             '<h4><strong>Q: </strong>What is being scored?</h4>',
             '<h5><strong>A: </strong>All the metrics you chose. For example, if you looked at the metric "unemployment rate" then the neighborhoods in the 90%ile would have the highest unemployment rates in the city and the neighborhoods in the 0%ile would have the lowest unemployment rates.</h5>',
             '<h4><strong>Q: </strong>How do I see these "scores"?</h4>',
             '<h5><strong>A: </strong>Go through the first page to make your map, and then click / tap on any of the neighborhoods (each will be their own color) to see its overall score and the score breakdown for each metric you chose. The colors also display the overall score, with the highest scoring neighborhoods (90%ile) in red and the lowest scoring neighborhoods (0%ile) in green.</h5>',
             '<h4><strong>Q: </strong>What is the overall score and how is it calculated?</h4>',
             '<h5><strong>A: </strong>The overall score combines all of the metrics you chose into one number for each neighborhood. You can calculate it by taking the average score for each metric you chose. For example, say you chose three metrics to look at. One neighborhood scores in the 70%ile for unemployment, 80%ile for obesity, and 60%ile in low education, so the neighborhood\'s overall score would be the 70%ile. In general, a higher overall score (90%ile) means the neighborhood has worse conditions (based ont the metrics you chose) than a neighborhood with a lower overall score (0%ile) </h5>',
             '<h4><strong>Q: </strong>So 90%ile = neighborhood needs help and 0%ile = neighborhood is doing well?</h4>',
             '<h5><strong>A: </strong><i>Based on the metrics you chose to study and compared to other neighorhoods in the city, yes.</i> Again, this data cannot capture the full circumstances a neighborhood experiences, but it can act as another piece of useful information in guiding where to put resources.</h5>',
             '<h4><strong>Q: </strong>I don\'t think every metric I choose should be counted equally in the overall score. How can I fix that?</h4>',
             '<h5><strong>A: </strong>If you are on a computer or a large screen you will be able to adjust the metrics to have some count towards the overall score more than others (i.e., re-weight the metrics). See the tutorial on the next page for where you can do this (computers and other large screens only)</h5>',
             '<h4><strong>Q: </strong>Where does the data come from?</h4>',
             '<h5><strong>A: </strong><a href = "https://www.census.gov/programs-surveys/acs">US Census American Community Survey</a> provides information on income, education, and other demographics. <a href = "https://www.cdc.gov/500cities/index.htm">Center for Disease Control</a> provides information on physical and mental health.</h5>',
             '<h4><strong>Q: </strong>Why should I trust this map? Who are you and why do you have this data?</h4>',
             '<h5><strong>A: </strong>All of this data is publicly available and you can find it yourself (see the links in the last question). I worked as a data scientist for the City of San Jose to help answer these questions around where to allocate resources. Most of the data I used was publicly available, so I wanted to enable other cities to study these questions through a data lens.</h5>',
             '<h4><strong>Q: </strong>I like what you\'ve done here. I have some ideas I think you could help with. How do I get in contact?</h4>',
             '<h5><strong>A: </strong>You can reach me at my email address (<a href = "mailto: gehami@alumni.stanford.edu">gehami@alumni.stanford.edu</a>) for any custom mapping requests or for partnership on a city-specific project.</h5>'
             
        ),
HTML('<h5 class = "splash_text smaller_header">Data from the <a href = "https://www.census.gov/programs-surveys/acs"> US Census American Community Survey </a> and the <a href = "https://www.cdc.gov/500cities/index.htm">CDC\'s 500 Cities Project</a>. All metrics are scored from low-issue (0%ile) to high-issue (90%ile).</h5>',
     '<h5 class = "splash_text smaller_header">For comments, questions, and custom-mapping requests, contact Albert Gehami at 
     <a href = "mailto: gehami@alumni.stanford.edu">gehami@alumni.stanford.edu</a></h5>')

        )
  )
  
})


###### begin server code #########

######### Allowing to filter by state ###########

output$select_city <- renderUI({
  selected_cities = ''
  
  if(input$state != ''){
  
  # if(input$state != ''){
    selected_cities = cities_cdc[grep(paste0(input$state, '$'), cities_cdc)][order(c(cities_cdc[grep(paste0(input$state, '$'), cities_cdc)]))]
  # }
  
  selectizeInput(
    'city', HTML(paste0('Search for your city', '</br><small>Cities with small populations will not show health-related data due to privacy concerns</small>')), choices = selected_cities,
      # c(cities_cdc[grep(paste0(input$state, '$'), cities_cdc)])[order(c(cities_cdc[grep(paste0(input$state, '$'), cities_cdc)]))],
    multiple = FALSE,
    options = list(
#      placeholder = 'Enter City name',
      onInitialize = I(paste0('function() { this.setValue("',paste(location, collapse = ','),'"); }')),
      maxOptions = 1000
    )
  )
  }else{NULL}
})






######### Setting up the preset metrics buttons ###########

preset_options = gsub('\\.', ' ', gsub('^Preset_[0-9]+_', '', 
                                       grep('^Preset', colnames(data_code_book), value = TRUE, ignore.case = TRUE)))

metrics_selected_1 = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_1_', colnames(data_code_book), ignore.case = TRUE)] %in% 1]
metrics_selected_2 = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_2_', colnames(data_code_book), ignore.case = TRUE)] %in% 1]
metrics_selected_3 = data_code_book$risk_factor_name[data_code_book[,grep('^Preset_3_', colnames(data_code_book), ignore.case = TRUE)] %in% 1]




preset_options_list = list(list(preset_options[1], metrics_selected_1, PRESET_1_DESC_TEXT, 1),
                           list(preset_options[2], metrics_selected_2, PRESET_2_DESC_TEXT, 2),
                           list(preset_options[3], metrics_selected_3, PRESET_3_DESC_TEXT, 3))

#shout out to the real ones at w3schools for this popup function: https://www.w3schools.com/howto/howto_js_popup.asp

output$preset_buttons <- renderUI(lapply(preset_options_list, function(i){

  div(class = 'preset-buttons-dropdown', 
      actionBttn(inputId = i[[1]], label = i[[1]], size = 'sm'),
      HTML('<p class = "preset-button-desc"><i>', i[[3]], '</i></p>')
      )
}))





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
  
  inputs_local = inputs_react()
  inputs_local[['medical_factors']] <- health_selected
  inputs_local[['economics_factors']] <- economic_selected
  inputs_local[['at-risk_factors']] <- violence_selected
  inputs_local[['qol_factors']] <- qol_selected
  
  inputs_react(inputs_local)
  
  
  
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
  
  inputs_local = inputs_react()
  inputs_local[['medical_factors']] <- health_selected
  inputs_local[['economics_factors']] <- economic_selected
  inputs_local[['at-risk_factors']] <- violence_selected
  inputs_local[['qol_factors']] <- qol_selected
  
  inputs_react(inputs_local)
  

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
  
  inputs_local = inputs_react()
  inputs_local[['medical_factors']] <- health_selected
  inputs_local[['economics_factors']] <- economic_selected
  inputs_local[['at-risk_factors']] <- violence_selected
  inputs_local[['qol_factors']] <- qol_selected
  
  inputs_react(inputs_local)
  


  shinyjs::click('map_it')
  
  
})


########## removing CDC data if small city is chosen #########

observeEvent(input$city, handlerExpr = {
  if(input$city %in% big_cities | input$city == ""){
    preset_options_list = list(list(preset_options[1], metrics_selected_1, PRESET_1_DESC_TEXT, 1),
                               list(preset_options[2], metrics_selected_2, PRESET_2_DESC_TEXT, 2),
                               list(preset_options[3], metrics_selected_3, PRESET_3_DESC_TEXT, 3))
    
    
    health_input_choices = HEALTH_CHOICES
    economic_input_choices = ECONOMIC_CHOICES
    at_risk_input_choices = VIOLENCE_CHOICES
    qol_input_choices = QOL_CHOICES
    
  }else{
    preset_options_list = list(list(preset_options[1], metrics_selected_1, PRESET_1_DESC_TEXT, 1),
                               #list(preset_options[2], metrics_selected_2, PRESET_2_DESC_TEXT, 2),#commenting out medical data
                               list(preset_options[3], metrics_selected_3, PRESET_3_DESC_TEXT, 3))
    
    
    remove_vars = data_code_book$risk_factor_name[data_code_book$Dataset %in% NO_SMALL_DATA]
    health_input_choices = HEALTH_CHOICES[which(!(HEALTH_CHOICES %in% remove_vars))]
    economic_input_choices = ECONOMIC_CHOICES[which(!(ECONOMIC_CHOICES %in% remove_vars))]
    at_risk_input_choices = VIOLENCE_CHOICES[which(!(VIOLENCE_CHOICES %in% remove_vars))]
    qol_input_choices = QOL_CHOICES[which(!(QOL_CHOICES %in% remove_vars))]
    
  }
  output$preset_buttons <- renderUI(lapply(preset_options_list, function(i){
    
    div(class = 'preset-buttons-dropdown', 
        actionBttn(inputId = i[[1]], label = i[[1]], size = 'sm'),
        HTML('<p class = "preset-button-desc"><i>', i[[3]], '</i></p>')
    )
  }))
  
  output$custom_metrics <- renderUI(
    div(id = 'factor_selectors', 
        div(class = "factor_selector",
            dropdownButton(
              checkboxInput('all_health_factors', "Select all"),
              checkboxGroupInput(
                'health_factors', 'Health factors',
                choices = health_input_choices,
                selected = health_risk_factors
              ),
              label = 'Health factors',
              circle = FALSE
            )),
        div(class = "factor_selector",
            dropdownButton(
              checkboxInput('all_economic_factors', "Select all"),
              checkboxGroupInput(
                'economic_factors', 'Economic factors',
                choices = economic_input_choices,
                selected = economic_factors
              ),
              label = 'Economic factors',
              circle = FALSE
            )),
        div(class = "factor_selector",
            dropdownButton(
              checkboxInput('all_violence_factors', "Select all", value = FALSE),
              checkboxGroupInput(
                'violence_factors', 'Social factors',
                choices = at_risk_input_choices, selected = violence_risk_factors
              ),
              label = 'Social factors',
              circle = FALSE
            )),
        div(class = "factor_selector",
            dropdownButton(
              checkboxInput('all_qol_factors', "Select all"),
              checkboxGroupInput(
                'qol_factors', 'Other quality-of-life factors',
                choices = qol_input_choices, 
                selected = qol_factors
              ),
              label = 'Quality-of-life factors',
              circle = FALSE
            ))
    )
  )
  
})




######### Tracking checkboxgroups to update inputs_react() ############



observeEvent(input$health_factors, {
  inputs_local <- inputs_react()
  inputs_local[['medical_factors']] <- input$health_factors
  inputs_react(inputs_local)
})


observeEvent(input$economic_factors, {
  inputs_local <- inputs_react()
  inputs_local[['economics_factors']] <- input$economic_factors
  inputs_react(inputs_local)
})
observeEvent(input$violence_factors, {
  inputs_local <- inputs_react()
  inputs_local[['at-risk_factors']] <- input$violence_factors
  inputs_react(inputs_local)
})
observeEvent(input$qol_factors, {
  inputs_local <- inputs_react()
  inputs_local[['qol_factors']] <- input$qol_factors
  inputs_react(inputs_local)
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
      inputs_local <- inputs_react()
      inputs_local[['at-risk_factors']] <- character(0)
      inputs_react(inputs_local)
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
      inputs_local <- inputs_react()
      inputs_local[['medical_factors']] <- character(0)
      inputs_react(inputs_local)
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
      inputs_local <- inputs_react()
      inputs_local[['economic_factors']] <- character(0)
      inputs_react(inputs_local)
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
      inputs_local <- inputs_react()
      inputs_local[['qol_factors']] <- character(0)
      inputs_react(inputs_local)
    }
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)











##### Reaction to save inputs and begin building the map ##########

output$loading_sign = NULL
output$input_warning <- renderUI(HTML("<h5>This process may take up to 60 seconds</h5>"))

observeEvent(input$map_it,{
  # print(input$state)
  print(is.null(input$city))

if(is.null(c(input$violence_factors, input$health_factors, input$economic_factors, input$qol_factors)) & !clicked_preset()){
    print("no factors present")
    output$input_warning <- renderUI(h4("Please select at least 1 factor from the 4 drop-down menus above or select one of the three presets above", class = "warning_text"))
}else if(is.null(input$city)){
    output$input_warning <- renderUI(h4("Please select a state and a city", class = "warning_text"))
}else if(input$city == ''){
  output$input_warning <- renderUI(h4("Please select a state and a city", class = "warning_text"))
  }
  else{
    shinyjs::disable('map_it')
    
    ###### Initial set-up #######
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Recording inputs", value = 0)
    
    #marking if it is a small city or not. If it's a small city then we need to remove all inputs that are from the CDC, since CDC does not have neighborhood data for small cities
    small_city = FALSE
    if(!(input$city %in% big_cities)){
      small_city = TRUE
      print('small city')
    }
    
    
    # output$warn = NULL
    inputs <- inputs_react()
    inputs[['cities']] <- input$city
    # inputs[['year_range']] <- input$year_range
    inputs[['year_range']] <- YEAR_RANGE
    # inputs[['medical_factors']] <- input$health_factors
    # inputs[['economics_factors']] <- input$economic_factors
    # inputs[['at-risk_factors']] <- input$violence_factors
    # inputs[['qol_factors']] <- input$qol_factors
    # print(inputs)
    
    ##removing any inputs that do not apply to small cities if this is a small city
    if(small_city){
      remove_vars = data_code_book$risk_factor_name[data_code_book$Dataset %in% NO_SMALL_DATA]
      for(i in keys(inputs)){
        inputs[[i]] = inputs[[i]][which(!(inputs[[i]] %in% remove_vars))]
        if(length(inputs[[i]]) == 0) inputs[[i]] = NULL
      }
    }
    

    #saving inputs for debugging
    # saveRDS(inputs, 'inputs_outputs/debug_inputs.rds')
    
    ###### opening files and doing the things ######
    ####### Reading in data ########
    
    #reading in the cdc data if it is a big city
    if(!small_city){
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
    }else{
      print("Imma smol city so I don't need CDC data")
    }
    
    
    
    
    #reading in the acs data. 
    progress$set(message = "Loading Census data", value = .1)
    if(!exists("acs_hash")){acs_hash = readRDS('data_tables/acs_dat_hash.rds')}

    #reading in the spatial data
    progress$set(message = "Loading maps", value = .15)
    
    #First we need to identify the tracts in the city
    if(!exists('city_tract_map')){city_tract_map = readRDS('data_tables/All tracts in all US cities - state WY.rds')}
    tracts_in_city = city_tract_map[[inputs$cities]]
    #then we need to identify which counties are connected to the city so we can pull those counties into the map
    if(!exists('city_county_map'))city_county_map = readRDS('data_tables/city_to_county_hash.rds')
    #pull all the counties the city is a part of
    city_counties = city_county_map[[inputs$cities]]
    #pulls the first county the city is a part of
    county_map = readRDS(paste0('data_tables/All county shape data/', city_counties[1]))
    #pulls additional counties the city is a part of
    if(length(city_counties) > 1){
      for(n in 2 : length(city_counties)){
        county_map = rbind(county_map, readRDS(paste0('data_tables/All county shape data/', city_counties[n])))
      }
    }
    #makes the tract map for all the city
    tracts_map = county_map[county_map$GEOID %in% tracts_in_city,]
    
    
    #checking to make sure that my ACS data holds all tracts, it does. blessings on blessings
    # all_tracts = hash::values(city_tract_map) %>% unlist() %>% unique()
    # length(all_tracts %in% acs_year$GEOID) == length(all_tracts)
    
    # if(!exists('trimmed_tracts')){trimmed_tracts = readRDS('data_tables/trimmed_tract_data.rds')}
    
    #reading in the tract_city_database
    # if(!exists('tract_city_dictionary')){tract_city_dictionary = readRDS('data_tables/tract_city_dictionary.rds')}
    
    progress$set(message = "Loading maps", value = .20)
    

    #get just the tracts from the cities that we care about
    # city_tracts = tract_city_dictionary[inputs$cities] %>% values() %>% unlist() %>% as.character()
    
    #identifying which tracts to use
    # tracts_map = trimmed_tracts[trimmed_tracts$GEOID %in% city_tracts,]
    
    
    

    ####### Contstants #########
    
    param_hash = hash::copy(inputs)
    hash::delete(c('cities', 'year_range'), param_hash)
    data_factors = param_hash %>% values() %>% unlist()
    if(length(dim(data_factors)) > 0){
      data_factors = as.character(data_factors)
      names(data_factors) = rep(keys(param_hash), length(data_factors))
    }
    
    
    
    ######## Creating the initial map #########
    
    progress$set(message = "Cleaning data", value = .30)
    

    city_all_dat_hash = hash::hash() 
    for(year in inputs$year_range[1]:inputs$year_range[2]){
      acs_year = acs_hash[[as.character(year)]]
      acs_year = acs_year[acs_year$GEOID %in% tracts_in_city,]
      if(!small_city){
        cdc_year = cdc_hash[[as.character(year)]]
        cdc_year = cdc_year[cdc_year$GEOID %in% tracts_in_city,]
        city_all_dat_hash[[as.character(year)]] = merge(cdc_year[!duplicated(cdc_year$GEOID),], acs_year[!duplicated(acs_year$GEOID),], by = 'GEOID')
      }
      else{
        city_all_dat_hash[[as.character(year)]] = acs_year[!duplicated(acs_year$GEOID),]
      }
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
    past_spdf = make_full_spdf(city_all_spdf_hash[[as.character(inputs$year_range[1])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS, info_popup_text = INFO_POPUP_TEXT)
    
    progress$set(message = paste0("Designing map of ", inputs$year_range[2]), value = .50)
    present_spdf = make_full_spdf(city_all_spdf_hash[[as.character(inputs$year_range[2])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS, info_popup_text = INFO_POPUP_TEXT)
    
    progress$set(message = paste0("Predicting map of ", inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])), value = .60)
    pred_list = get_predicted_scores_and_labels(city_all_spdf_hash, inputs, risk_vars, risk_weights, data_code_book, QUANTILE_BINS, MAX_LOC_DIST, info_popup_text = INFO_POPUP_TEXT)
    present_spdf@data$pred_score = pred_list$raw_score
    present_spdf@data$pred_quantile = pred_list$score_quantile
    present_spdf@data$pred_label = pred_list$label
    
    
    
    progress$set(message = "Rendering maps", value = .70)
    initial_map = make_map(present_spdf, past_spdf, inputs, TRACT_PAL, TRACT_OPACITY, QUANTILE_BINS)
    
    
    ######### Saving the needed files for next page ########
    
    progress$set(message = "Finalizing information", value = .80)
    
    city_all_spdf_hash_react(city_all_spdf_hash)
    data_code_book_react(data_code_book)
    risk_vars_react(risk_vars)
    data_factors_react(data_factors)
    initial_map_react(initial_map)
    example_past_spdf_react(past_spdf[1,])
    present_spdf_react(present_spdf)
    
    
   
    #### Moving to next page #######
    
    progress$set(message = "Moving to results page", value = .90)
    
    shinyjs::enable('map_it')
    
    output$pageStub <- renderUI(tagList(
      tags$head(tags$script(redirect_jscode)),
      tags$head(rel = "stylesheet", type = 'text/css', href = 'https://fonts.googleapis.com/css?family=Montserrat|Open+Sans|Raleway|Roboto|Roboto+Condensed&display=swap'),
      includeCSS('www/sreen_size.css'),
      useShinyjs(),
      fluidPage(
        div(class = "no_small_screen",
            bsCollapse(id = "sliders",
                       bsCollapsePanel(HTML('<div stlye = "width:100%;">Click here to edit weight of metrics</div>'), value = 'Click here to edit weight of metrics',
                                       fluidRow(
                                         column(10, h4("Increase/decrease the amount each metric goes into the overall need metric. To recalculate overall risk / need, click 'Submit'"),
                                                h5("For example, boosting one metric to 2 will make it twice as important in calculating the overall risk / need")),
                                         column(2, actionBttn('recalculate_weights', 'Submit'),
                                                actionBttn('reset_weight', 'Reset weights'))
                                         # column(2, actionButton('recalculate_weights', 'Submit'))
                                       ),
                                       fluidRow(
                                         column(4, uiOutput('sliders_1')),
                                         column(4, uiOutput('sliders_2')),
                                         column(4, uiOutput('sliders_3'))
                                       ), style = "info"
                       )
            )
        ),
        fluidRow(
          div(id = "map_container",
              leaflet::leafletOutput('map', height = 'auto'),
              div(id = 'initial_popup', class = "popup",
                  HTML('<h3, class = "popup_text">Would you like the tutorial?</h3></br>'),
                  actionLink('close_help', label = HTML('<p class="close">&times;</p>')),
                  actionBttn("walkthrough", HTML("<p>Yes</p>"), style = 'unite', size = 'sm'),
                  actionBttn("no_walkthrough", HTML("<p>No</p>"), style = 'unite', size = 'sm')
                  # actionButton("walkthrough", HTML("<p>Yes</p>")),
                  # actionButton("no_walkthrough", HTML("<p>No</p>"))
              ),
              uiOutput('tutorial'),
              div(id = 'home_button', class = 'no_small_screen', tags$a(href = '?home', icon('home', class = 'fa-3x')),
                  div(id = 'download_button_div', downloadBttn('download_gis_data', label = 'download shapefile', size = 'sm'))),
              div(id = 'select_year_div', class = 'no_small_screen', pickerInput('select_year',
                                                      choices = c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]),
                                                                  as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1]))),
                                                      multiple = FALSE, selected = as.character(inputs$year_range[2]), width = '71px'),
                  div(id = 'race_circles_div', class = 'race_check_box', checkboxInput(inputId = 'race_circles', label = 'Race', value = FALSE))),
              div(id = 'home_and_year', class = 'no_big_screen',
                  # div(id = 'select_year_div',  pickerInput('select_year_small',
                  #                                          choices = c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]),
                  #                                                                                as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1]))),
                  #                                                                    multiple = FALSE, selected = as.character(inputs$year_range[2]), width = '71px')),
                  div(id = 'race_circles_div_small', class = 'race_check_box',checkboxInput(inputId = 'race_circles_small', label = 'Race', value = FALSE)),
                  
                  div(id = 'home_button', tags$a(href = '?home', icon('home', class = 'fa-3x')))
                  
                  )
          )
        ),id = 'results_page'
      )
      
    ))
    
    # session$sendCustomMessage("mymessage", "mymessage")
    # output$current_page <- uiOutput({
      
    # })
  }
})

#########  Server back-end #########
####### Outputing initial map ########
output$map <- renderLeaflet(initial_map_react())


####### Tutorial #########

#General map movement
observeEvent(input$walkthrough,{
  shinyjs::hide(id = 'initial_popup')
  output$tutorial <- renderUI({
    div(class = "popup",
        HTML('<h5, class = "popup_text">Here is the map of your city based on the metrics you chose.</h5></br>',
             '<h5, class = "popup_text">The map is divided into <a href = "https://simplyanalytics.zendesk.com/hc/en-us/articles/204848916-What-is-a-census-tract-" target="_blank">census tracts</a>, which are similar to neighborhoods</h5></br>',
             '<h5, class = "popup_text">Click-and-drag map to move around (or swipe on mobile).</h5></br>',
             '<h5, class = "popup_text">Scroll to zoom in and out (or pinch on mobile)</h5></br>'),
        actionLink('close_help_popups', label = HTML('<p class="close">&times;</p>')),
        actionBttn("walkthrough_map_nav", HTML("<p>Next</p>"), style = 'unite', size = 'sm')
        # actionButton("walkthrough_map_nav", HTML("<p>Next</p>"))
    )
  })
})

#map tiles
observeEvent(input$walkthrough_map_nav,{
  # shinyjs::hide(id = 'initial_popup')
  output$tutorial <- renderUI({
    div(id = 'map_tile_popup', class = "popup",
        HTML('<h5, class = "popup_text">Clicking or tapping on a neighborhood tile (the colored blocks on the map) will display a pop-up with the neighborhood\'s overall "risk / need factor level" score and a breakdown for each metric chosen. A score below the 50%ile means that the factors you chose are less present in this neighborhood than others in the city, while a score above the 50%ile means the factors are more present than in the average neighborhood in the city. Learn more about these scores from the <a href = "?home">FAQ on the bottom of the home page</a></h5></br></br>'),
             # '<h5, class = "popup_text">This is where you will see information such as a neighborhood\'s unemployment rate, obesity rate, or any other metrics you chose to look at. 
             # Each metric is scored relative to the other neighborhoods, with lowest-scoring neighborhoods in the 0%ile and highest-scoring neighborhoods in the 90%ile. For example, if a neighborhood had one of the highest obesity rates in the city, that neighborhood would score in the 90%ile in obesity.</br>
             # The overall score combines all of the metrics you chose into one number for each neighborhood, and reflects the color of the neighborhood\'s tile</br>
             # <i>Additional details for the extra curious: </i>You can calculate the overall score by taking the average score of all the metrics you chose (shown in small font). Typically the highest scoring neighborhoods show the highest needs in the city according to the data and selected metrics. Learn more from the <a href = "?home">FAQ on the bottom of the home page</a></br>
             # If you selected metrics from multiple categories (e.g., both medical and economic factors), the metrics are segmented by category, and each category has an "overall category score" as well. For example you may see an overall score (all metrics selected), an overall economic score (things such as unemployment or poverty rates), and an overall medical score (things such as obesity and diabetes rates).</h5></br>'),
        actionLink('close_help_popups', label = HTML('<p class="close">&times;</p>')),
        actionBttn("walkthrough_map_tile", HTML("<p>Next</p>"), style = 'unite', size = 'sm')
        # actionButton("walkthrough_map_tile", HTML("<p>Next</p>"))
        
    )
  })
  # leafletProxy('map') %>% clearPopups() %>% addPopups(lat = as.numeric(example_past_spdf_react()$INTPTLAT), lng = as.numeric(example_past_spdf_react()$INTPTLON),
  #                                                     popup = HTML(example_past_spdf_react()$label))
  
  # shinyjs::addCssClass(class = 'highlight-border', selector = '.leaflet-popup-content')
  
  
})

#legend
observeEvent(input$walkthrough_map_tile,{
  # shinyjs::hide(id = 'initial_popup')
  # shinyjs::removeCssClass(class = 'highlight-border', selector = '.leaflet-popup-content')
  
  output$tutorial <- renderUI({
    div(id = 'legend_popup', class = "popup",
        HTML('<h5, class = "popup_text">Below is the map\'s legend. Each neighborhood is colored based on its overall 
             score from the metrics you chose. High-issue neighborhoods will be shown in red (90%ile) while low-issue 
             neighborhoods will be shown in blue (0%ile)</h5></br>'),
        actionLink('close_help_popups', label = HTML('<p class="close">&times;</p>')),
        div(class = 'no_big_screen',actionBttn("walkthrough_race", HTML("<p no_big_screen>Next</p>"), style = 'unite', size = 'sm')),
        div(class = 'no_small_screen',actionBttn("walkthrough_legend", HTML("<p no_small_screen>Next</p>"), style = 'unite', size = 'sm'))
        #actionBttn("walkthrough_legend", HTML("<p>Next</p>"), style = 'unite', size = 'sm')
        # actionButton("walkthrough_legend", HTML("<p>Next</p>"))
        
        )
  })
  leafletProxy('map') %>% clearPopups()
  shinyjs::addCssClass(class = 'highlight-border', selector = '.legend')
  
})

#layer controls
observeEvent(input$walkthrough_legend,{
  # shinyjs::hide(id = 'initial_popup')
  shinyjs::removeCssClass(class = 'highlight-border', selector = '.legend')
  
  output$tutorial <- renderUI({
    div(id = 'layer_and_metrics_popup', class = "popup",
        HTML('<h5, class = "popup_text"></h5>Change the year by clicking the drop-down <span class = "no_small_screen">to the right</span>. 
             You can see the metrics for the past and present, as well as the predict overall metric for the future.</br>'),
        actionLink('close_help_popups', label = HTML('<p class="close">&times;</p>')),
        actionBttn("walkthrough_race", HTML("<p>Next</p>"), style = 'unite', size = 'sm')
        # div(class = 'no_big_screen', actionButton("walkthrough_to_home", HTML("<p no_big_screen>Explore map</p>"))),
        # div(class = 'no_small_screen', actionButton("walkthrough_layers", HTML("<p no_small_screen>Next</p>")))
        
        )
  })
  shinyjs::addCssClass(id = 'select_year_div', class = 'highlight-border')
  shinyjs::addCssClass(id = 'home_and_year', class = 'highlight-border')
  
})
#race
observeEvent(input$walkthrough_race,{
  # shinyjs::hide(id = 'initial_popup')
  shinyjs::removeCssClass(class = 'highlight-border', selector = '.legend')
  shinyjs::removeCssClass(id = 'select_year_div', class = 'highlight-border')
  shinyjs::removeCssClass(id = 'home_and_year', class = 'highlight-border')
  
  output$tutorial <- renderUI({
    div(id = 'layer_and_metrics_popup', class = "popup",
        HTML('<h5, class = "popup_text"></h5>Toggle race & ethnicity demographics using the "Race" check box<span class = "no_small_screen"> to the right</span>. 
             Clicking the box will display a pie chart for each community\'s demographics. The pie chart is located at the <i>population center</i> for each neighborhood, rather than the geographic center. If the pie charts seem to clog up the map, try zooming in. Like the map tiles, clicking on a pie chart will show its population count and racial breakdown.<span class = "no_big_screen"> NOTE: On a bigger screen, you can change change the year of the data. On mobile you can see the most recent published data - 2018.</span></br>'),
        actionLink('close_help_popups', label = HTML('<p class="close">&times;</p>')),
        div(class = 'no_big_screen',actionBttn("walkthrough_to_home", HTML("<p no_big_screen>Next</p>"), style = 'unite', size = 'sm')),
        div(class = 'no_small_screen',actionBttn("walkthrough_weights", HTML("<p no_small_screen>Next</p>"), style = 'unite', size = 'sm'))
        )
  })
  shinyjs::addCssClass(id = 'race_circles_div', class = 'highlight-border')
  shinyjs::addCssClass(id = 'race_cirlces_div_small', class = 'highlight-border')
  updateCheckboxInput(session, inputId = 'race_circles_small', value = TRUE)
  shinyjs::addCssClass(id = 'home_and_year', class = 'highlight-border')
  
  
})



#weight adjustment
observeEvent(input$walkthrough_weights,{
  # shinyjs::hide(id = 'initial_popup')
  shinyjs::removeCssClass(id = 'race_circles_div', class = 'highlight-border')
  shinyjs::removeCssClass(id = 'race_circles_div_small', class = 'highlight-border')
  
  updateCheckboxInput(session, inputId = 'race_circles_small', value = FALSE)
  
  output$tutorial <- renderUI({
    div(id = 'layer_and_metrics_popup', class = "popup",
        HTML('<h5, class = "popup_text">Currently, all of the metrics you chose to look at are weighted equally. 
             If you feel like some should be more important than others in determining your overall metric,
             you can adjust their weights above.</br></h5>'),
        actionLink('close_help_popups', label = HTML('<p class="close">&times;</p>')),
        div(actionBttn("walkthrough_to_home", HTML("<p>Next</p>"), style = 'unite', size = 'sm'))
        # div(actionButton("walkthrough_to_home", HTML("<p>Next</p>")))
        
        )
  })
  
  updateCollapse(session, "sliders", open = 'Click here to edit weight of metrics')
  shinyjs::addCssClass(class = 'highlight-border', selector = '.panel.panel-info')
  
})

#home button and final mentions
observeEvent(input$walkthrough_to_home,{
  shinyjs::removeCssClass(class = 'highlight-border', selector = '.panel.panel-info')
  updateCollapse(session, "sliders", close = 'Click here to edit weight of metrics')
  updateCheckboxInput(session, inputId = 'race_circles_small', value = FALSE)
  
  shinyjs::removeCssClass(id = 'select_year_div', class = 'highlight-border')
  shinyjs::removeCssClass(id = 'race_circles_div', class = 'highlight-border')
  shinyjs::removeCssClass(id = 'race_circles_div_small', class = 'highlight-border')
  
  
  output$tutorial <- renderUI({
    div(id = 'home_popup', class = "popup",
        HTML('<h5, class = "popup_text">Return to the home screen by clicking on the home icon.',
             '<span class = "no_small_screen">If you like the map, you can download the raw data in shapefile format (with excel table) by clicking the download button. </span>',
             '<span class = "no_big_screen">For additional features, including <strong> downloading the map\'s raw data </strong>, access this site on a larger screen.</span>',
             'For comments/questions, custom mapping requests and advice on other projects, contact Albert at <a href = "mailto: gehami@alumni.stanford.edu">gehami@alumni.stanford.edu</a>',
             '</h5></br>'),
        actionLink('close_help_popups', label = HTML('<p class="close">&times;</p>')),
        div(actionBttn("end_walkthrough", HTML("<p>Explore map</p>"), style = 'unite', size = 'sm'))
        # div(actionButton("end_walkthrough", HTML("<p>Explore map</p>")))
    )     
  })
  
  shinyjs::addCssClass(id = 'home_button', class = 'highlight-border')
  shinyjs::addCssClass(id = 'home_and_year', class = 'highlight-border')
  
  
})

#close "x" button from first screen
observeEvent(input$close_help,{
  output$tutorial <- renderUI({
    div(id = 'return_help_popup', class = 'help_popup', 
        actionLink('open_help', HTML('<p class = "re_open">&quest;</p>'))
    )
  })
  shinyjs::hide(id = 'initial_popup')
  
  
})

#close "x" button from non-first screen
observeEvent(input$close_help_popups,{
  print("This should close the help popup")
  leafletProxy('map') %>% clearPopups()
  updateCheckboxInput(session, inputId = 'race_circles_small', value = FALSE)
  output$tutorial <- renderUI({
    div(id = 'return_help_popup', class = 'help_popup', 
        actionLink('open_help', HTML('<p class = "re_open">&quest;</p>'))
    )
  })
  shinyBS::updateCollapse(session, "sliders", close = 'Click here to edit weight of metrics')
  shinyjs::removeCssClass(class = 'highlight-border', selector = '.panel.panel-info')
  shinyjs::removeCssClass(id = 'select_year_div', class = 'highlight-border')
  shinyjs::removeCssClass(class = 'highlight-border', selector = '.legend')
  shinyjs::removeCssClass(id = 'home_button', class = 'highlight-border')
  shinyjs::removeCssClass(id = 'home_and_year', class = 'highlight-border')
  shinyjs::removeCssClass(id = 'race_circles_div', class = 'highlight-border')
  shinyjs::removeCssClass(id = 'race_circles_div_small', class = 'highlight-border')
  
  
  
})

#end walkthrough button
observeEvent(input$end_walkthrough,{
  
  shinyjs::removeCssClass(id = 'home_button', class = 'highlight-border')
  shinyjs::removeCssClass(id = 'home_and_year', class = 'highlight-border')
  
  
  output$tutorial <- renderUI({
    div(id = 'return_help_popup', class = 'help_popup', 
        actionLink('open_help', HTML('<p class = "re_open">&quest;</p>'))
    )
  })
  
})

#no walkthrough button
observeEvent(input$no_walkthrough,{
  shinyjs::hide(id = 'initial_popup')
  output$tutorial <- renderUI({
    div(id = 'return_help_popup', class = 'help_popup', 
        actionLink('open_help', HTML('<p class = "re_open">&quest;</p>'))
    )
  })
})

#re-open help
observeEvent(input$open_help,{
  output$tutorial <- renderUI({
    div(class = "popup",
        HTML('<h5, class = "popup_text">Here is the map of your city based on the metrics you chose.</h5></br>',
             '<h5, class = "popup_text">The map is divided into <a href = "https://simplyanalytics.zendesk.com/hc/en-us/articles/204848916-What-is-a-census-tract-" target="_blank">census tracts</a>, which are similar to neighborhoods</h5></br>',
             '<h5, class = "popup_text">Click-and-drag map to move around (or swipe on mobile).</h5></br>',
             '<h5, class = "popup_text">Scroll to zoom in and out (or pinch on mobile)</h5></br>'),
        actionLink('close_help_popups', label = HTML('<p class="close">&times;</p>')),
        actionBttn("walkthrough_map_nav", HTML("<p>Next</p>"), style = 'unite', size = 'sm')
        # actionButton("walkthrough_map_nav", HTML("<p>Next</p>"))
        
    )
  })
})



######### Sliders/numeric inputs ##########
output$sliders_1 <- renderUI({
  lapply(data_factors_react()[seq(1, length(data_factors_react()), by = 3)], function(i){
      numericInput(inputId = i, label = i, value = INITIAL_SLIDER_VALUE)
      # sliderInput(inputId = i, label = i, min = SLIDER_MIN, max = SLIDER_MAX, value = INITIAL_SLIDER_VALUE, step = MIN_SLIDER_STEP)
  })
})
output$sliders_2 <- renderUI({
  if(length(data_factors_react()) > 1){
    lapply(data_factors_react()[seq(2, length(data_factors_react()), by = 3)], function(i){
      numericInput(inputId = i, label = i, value = INITIAL_SLIDER_VALUE)
      # sliderInput(inputId = i, label = i, min = SLIDER_MIN, max = SLIDER_MAX, value = INITIAL_SLIDER_VALUE, step = MIN_SLIDER_STEP)
    })
  }
})
output$sliders_3 <- renderUI({
  if(length(data_factors_react()) > 2){
    lapply(data_factors_react()[seq(3, length(data_factors_react()), by = 3)], function(i){
      numericInput(inputId = i, label = i, value = INITIAL_SLIDER_VALUE)
      # sliderInput(inputId = i, label = i, min = SLIDER_MIN, max = SLIDER_MAX, value = INITIAL_SLIDER_VALUE, step = MIN_SLIDER_STEP)
    })
  }
})

  # if(length(data_factors) > 1){
  #   output$sliders_2 <- renderUI({
  #     lapply(data_factors[seq(2, length(data_factors), by = 3)], function(i){
  #       numericInput(inputId = i, label = i, value = INITIAL_SLIDER_VALUE)
  #       # sliderInput(inputId = i, label = i, min = SLIDER_MIN, max = SLIDER_MAX, value = INITIAL_SLIDER_VALUE, step = MIN_SLIDER_STEP)
  #     })
  #   })
  # }else{output$sliders_2 = NULL}
  # 
  # if(length(data_factors) > 2){
  #   output$sliders_3 <- renderUI({
  #     lapply(data_factors[seq(3, length(data_factors), by = 3)], function(i){
  #       numericInput(inputId = i, label = i, value = INITIAL_SLIDER_VALUE)
  #       # sliderInput(inputId = i, label = i, min = SLIDER_MIN, max = SLIDER_MAX, value = INITIAL_SLIDER_VALUE, step = MIN_SLIDER_STEP)
  #     })
  #   })
  # }else{output$sliders_3 = NULL}
  # 




######### Updating map year layer #######

observeEvent(input$select_year,{
  leafletProxy('map') %>% hideGroup(c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]),
                                      as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])))) %>%
    showGroup(as.character(input$select_year)) 
})
observeEvent(input$select_year_small,{
  leafletProxy('map') %>% hideGroup(c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]),
                                      as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])))) %>%
    showGroup(as.character(input$select_year_small)) 
})


####### adding race circles ######

observeEvent(input$race_circles, {
  if(input$race_circles){
    #adding in the racial demographics in the area
    present_spdf = present_spdf_react()
    race_pop = rowSums(present_spdf@data[,c("white", "black", "amiaknh", "asian", "hispanic")], na.rm = TRUE)
    #race popup is now working at the moment. Will need to figure this out later. 
    race_popup = paste0('<table><tbody><tr><td class="key">Population   :</td><td class="value">',race_pop,
                        '</td></tr><tr><td class="key">white</td><td class="value">',round(present_spdf@data$white/race_pop, digits = 3)*100, '%',
                        '</td></tr><tr><td class="key">black</td><td class="value">',round(present_spdf@data$black/race_pop, digits = 3)*100, '%',
                        '</td></tr><tr><td class="key">amiaknh</td><td class="value">',round(present_spdf@data$amiaknh/race_pop, digits = 3)*100, '%',
                        '</td></tr><tr><td class="key">asian</td><td class="value">',round(present_spdf@data$asian/race_pop, digits = 3)*100, '%',
                        # '</td></tr><tr><td class="key">mixed</td><td class="value">',round(present_spdf@data$mixed/race_pop, digits = 3)*100, '%',
                        '</td></tr><tr><td class="key">hispanic</td><td class="value">',round(present_spdf@data$hispanic/race_pop, digits = 3)*100, '%',
                        '</td></tr></tbody></table>')
    # print(head(present_spdf@data))
    leafletProxy('map') %>% leaflet.minicharts::addMinicharts(lng = present_spdf$center_lon, lat = present_spdf$center_lat, type = 'pie',
                                                            chartdata = present_spdf@data[,c("white", "black", "amiaknh", "asian",
                                                                                             # "mixed", 
                                                                                             "hispanic")],
                                                            height = 25, width = 25, showLabels = FALSE, legend = TRUE,
                                                            legendPosition = 'bottomright',
                                                            popup = popupArgs(html = race_popup))
    
  }else{
    leafletProxy('map') %>% leaflet.minicharts::clearMinicharts() 
  }  
})
#since the small phone and the big input have to have different names, this centralizes the update to impact the big input function
observeEvent(input$race_circles_small,{
  if(input$race_circles_small){
    if(!input$race_circles){
      updateCheckboxInput(session, inputId = 'race_circles', value = TRUE)
    }
  }else{
    if(input$race_circles){
      updateCheckboxInput(session, inputId = 'race_circles', value = FALSE)
    }
  }
})

######## Downloading shapefile #########


output$download_gis_data <- downloadHandler(
  filename = function(){
    paste0(input$city, '_', Sys.Date(), '.zip')
  },
  content = function(file){
    spdf = present_spdf_react()
    data_excel = spdf@data[,-grep('label', colnames(spdf@data))]
    #parses each unique word in the city-state name
    city_chunks = unlist(strsplit(input$city, split = ' '))
    #extracts the state abbreviation
    state_abb = city_chunks[length(city_chunks)]
    city_words = city_chunks[-length(city_chunks)]
    #if the city is made up of 2 or more words, take the first letter of the two longest words to make the abbreviation. 
    #Defaults to the words closest to the front in the case of ties
    if(length(city_words) > 1){
      city_words_df = data.frame(word = city_words, spot = 1:length(city_words), size = nchar(city_words), stringsAsFactors = FALSE)
      city_words_df = city_words_df[order(-city_words_df$size, city_words_df$spot)[1:2],]
      city_abbs = city_words_df$word[order(city_words_df$spot)] %>% substr(1,1) %>% paste(collapse = '', sep = '')
    }else{
      city_abbs = substr(city_words, 1, 2)
    }
    city_state_abb = paste(toupper(city_abbs), toupper(state_abb), collapse = '//_', sep = '_')
    #create a temp folder for the shape and excel files
    temp_shp = tempdir()
    #writing the shape files and excel files
    writeOGR(obj = spdf, dsn = temp_shp, layer = paste0(city_state_abb, '_SF'), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
    write.csv(spdf@data, file = paste0(temp_shp, '/', city_state_abb, '_XL.csv'))
    #zip all the files
    zip_file <- file.path(temp_shp, paste0(city_state_abb,'.zip'))
    save_files <- list.files(temp_shp, pattern = city_state_abb, full.names = TRUE)
    
    # R.utils::gzip(save_files)
    
    utils::zip(zipfile = zip_file, files = save_files)
    
    # the following zip method works for me in linux but substitute with whatever method working in your OS 
    # zip_command <- paste("zip ", 
    #                      zip_file, 
    #                      paste(save_files, collapse = " "))
    # system(zip_command)
    
    # copy the zip file to the file argument
    file.copy(zip_file, file)
    # remove all the files created
    file.remove(zip_file, save_files)
    print('downloading')
  },
  contentType = 'application/zip'
)


############# Updating map with updated metrics and reseting weights ##############

observeEvent(input$recalculate_weights,{
  
  shinyjs::disable("recalculate_weights")
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  
  progress$set(message = "Recording inputs", value = 0)
  
  #getting the new weights
  new_weights = rep(0, length(data_factors_react()))
  for(n in seq_along(data_factors_react())) new_weights[n] = input[[data_factors_react()[n]]]
  #creating the scores
  risk_vars = data_factors_react()
  risk_weights = new_weights
  print(risk_vars)
  print(risk_weights)
  # spdf = city_all_spdf_hash[['2018']]
  data_code_book = data_code_book_react()
  quantile_bins = QUANTILE_BINS
  
  progress$set(message = "Redefining 2016 metrics", value = .10)
  
  past_spdf = make_full_spdf(city_all_spdf_hash_react()[[as.character(inputs$year_range[1])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS, info_popup_text = INFO_POPUP_TEXT)
  
  progress$set(message = "Redefining 2018 metrics", value = .20)
  
  present_spdf = make_full_spdf(city_all_spdf_hash_react()[[as.character(inputs$year_range[2])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS, info_popup_text = INFO_POPUP_TEXT)
  
  progress$set(message = "Building predictive model", value = .30)
  
  pred_list = get_predicted_scores_and_labels(city_all_spdf_hash_react(), inputs, risk_vars, risk_weights, data_code_book, QUANTILE_BINS, MAX_LOC_DIST, info_popup_text = INFO_POPUP_TEXT)
  present_spdf@data$pred_score = pred_list$raw_score
  present_spdf@data$pred_quantile = pred_list$score_quantile
  present_spdf@data$pred_label = pred_list$label
  
  progress$set(message = "Updating map", value = .60)
  
  present_spdf_react(present_spdf)
  
  new_map = make_map(present_spdf, past_spdf, inputs, TRACT_PAL, TRACT_OPACITY, QUANTILE_BINS)
  
  progress$set(message = "Rendering map", value = .90)
  
  output$map = renderLeaflet(new_map)
  
  updatePickerInput(session, 'select_year',
              choices = c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]),
                          as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1]))),
              selected = as.character(inputs$year_range[2]))
  updatePickerInput(session, 'select_year_small',
                    choices = c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]),
                                as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1]))),
                    selected = as.character(inputs$year_range[2]))
  
  shinyBS::updateCollapse(session, "sliders", close = 'Click here to edit weight of metrics')
  # progress$close()
  
  shinyjs::enable("recalculate_weights")
  
  
})

observeEvent(input$reset_weight,{
  shinyjs::disable("reset_weight")
  
  
  output$sliders_1 <- renderUI({
    lapply(data_factors_react()[seq(1, length(data_factors_react()), by = 3)], function(i){
      numericInput(inputId = i, label = i, value = INITIAL_SLIDER_VALUE)
      # sliderInput(inputId = i, label = i, min = SLIDER_MIN, max = SLIDER_MAX, value = INITIAL_SLIDER_VALUE, step = MIN_SLIDER_STEP)
    })
  })
  if(length(data_factors_react()) > 1){
    output$sliders_2 <- renderUI({
      lapply(data_factors_react()[seq(2, length(data_factors_react()), by = 3)], function(i){
        numericInput(inputId = i, label = i, value = INITIAL_SLIDER_VALUE)
        # sliderInput(inputId = i, label = i, min = SLIDER_MIN, max = SLIDER_MAX, value = INITIAL_SLIDER_VALUE, step = MIN_SLIDER_STEP)
      })
    })
  }else{output$sliders_2 = NULL}
  if(length(data_factors_react()) > 2){
    output$sliders_3 <- renderUI({
      lapply(data_factors_react()[seq(3, length(data_factors_react()), by = 3)], function(i){
        numericInput(inputId = i, label = i, value = INITIAL_SLIDER_VALUE)
        # sliderInput(inputId = i, label = i, min = SLIDER_MIN, max = SLIDER_MAX, value = INITIAL_SLIDER_VALUE, step = MIN_SLIDER_STEP)
      })
    })
  }else{output$sliders_3 = NULL}
  
  shinyjs::enable("reset_weight")
  
  
})











