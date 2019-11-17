####### Experimenting with CDC data pulling ###########
#install.packages("RSocrata")
library(RSocrata)
#accesses the 2018 cdc data by census tract. Please note that you can filter the data by a value by 
#adding things like "?stateabbr=CA" after the ".json". any variable can be used to filter. 
#pulls data from these sources:
#2016: https://chronicdata.cdc.gov/500-Cities/500-Cities-City-level-Data-GIS-Friendly-Format-201/k56w-7tny
#2017: https://chronicdata.cdc.gov/500-Cities/500-Cities-City-level-Data-GIS-Friendly-Format-201/djk3-k3zs
#2018: https://chronicdata.cdc.gov/500-Cities/500-Cities-Census-Tract-level-Data-GIS-Friendly-Fo/k86t-wghb
#API urls:
#2016: https://chronicdata.cdc.gov/resource/a3kh-5fhs.json
#2017: https://chronicdata.cdc.gov/resource/kucs-wizg.json
#2018: https://chronicdata.cdc.gov/resource/k86t-wghb.json


cdc_2016 <- read.socrata(
  "https://chronicdata.cdc.gov/resource/a3kh-5fhs.json",
  app_token = "2TJ9miraJvdQBDfA9fTD4QNZ6",
  email     = "gehami@alumni.stanford.edu",
  password  = "albutt69!socrata"
)
cdc_2017 <- read.socrata(
  "https://chronicdata.cdc.gov/resource/kucs-wizg.json",
  app_token = "2TJ9miraJvdQBDfA9fTD4QNZ6",
  email     = "gehami@alumni.stanford.edu",
  password  = "albutt69!socrata"
)
cdc_2018 <- read.socrata(
  "https://chronicdata.cdc.gov/resource/k86t-wghb.json",
  app_token = "2TJ9miraJvdQBDfA9fTD4QNZ6",
  email     = "gehami@alumni.stanford.edu",
  password  = "albutt69!socrata"
)

cdc_2016 = cdc_2016[,grep('95ci', colnames(cdc_2016), invert = TRUE)]
cdc_2017 = cdc_2017[,grep('95ci', colnames(cdc_2017), invert = TRUE)]
cdc_2018 = cdc_2018[,grep('95ci', colnames(cdc_2018), invert = TRUE)]


#fixing the ventura name and any others with parantheses
cdc_2016$placename = gsub('[[:space:]]*\\([[:print:]]*\\)', '', cdc_2016$placename)
cdc_2017$placename = gsub('[[:space:]]*\\([[:print:]]*\\)', '', cdc_2017$placename)
cdc_2018$placename = gsub('[[:space:]]*\\([[:print:]]*\\)', '', cdc_2018$placename)

#fixing Boise City to just Boise
cdc_2016$placename = gsub('Boise City', 'Boise', cdc_2016$placename)
cdc_2017$placename = gsub('Boise City', 'Boise', cdc_2017$placename)
cdc_2018$placename = gsub('Boise City', 'Boise', cdc_2018$placename)


saveRDS(cdc_2016, 'data_tables/cdc_2016.rds')
saveRDS(cdc_2017, 'data_tables/cdc_2017.rds')
saveRDS(cdc_2018, 'data_tables/cdc_2018.rds')
