library(rgdal)
library(jsonlite)
library(readr)

preprocess_au_service_list <- function(filepath_geoj, filepath_postcodes){
  # read files
  df_geoj <- read.csv(filepath_geoj)
  df_postcodes <- read.csv(filepath_postcodes)

  # change column names
  colnames = c('service_name',
               'physical_address_line_1',
               'physical_address_2',
               'suburb',
               'state',
               'postcode',
               'aged_care_planning_region_acpr_2015',
               'care_type',
               'residential_places',
               'home_care_places',
               'restorative_care_places',
               'provider_name',
               'organisation_type',
               'abs_remoteness',
               'geoj_lat',
               'geoj_lon',
               'australian_government_funding_2018_19')
  names(df_geoj) <- colnames
  colnames <- c('postcode',
                'suburb',
                'state',
                'dc',
                'type',
                'postcode_lat',
                'postcode_lon')
  names(df_postcodes) <- colnames

  # preprocess df_geoj
  df_geoj = df_geoj[, c('service_name',
                        'postcode',
                        'suburb',
                        'state',
                        'geoj_lat',
                        'geoj_lon',
                        'home_care_places')]
  df_geoj$home_care_places[is.na(df_geoj$home_care_places)] <- 1
  return (df_geoj)
}

add_popn_to_geoj <- function(filepath_geoj, layer, filepath_popn, new_filepath_geoj){
  df_popn <- read.csv(filepath_popn)

  # change column names
  colnames <- c('state_id',
                'state_name',
                'lga_code',
                'lga_name',
                'ages00to04', # 5
                'ages05to09',
                'ages10to14',
                'ages15to19',
                'ages20to24',
                'ages25to29', # 10
                'ages30to34',
                'ages35to39',
                'ages40to44',
                'ages45to49',
                'ages50to54', # 15
                'ages55to59',
                'ages60to64',
                'ages65to69',
                'ages70to74',
                'ages75to79', # 20
                'ages80to84',
                'ages85andOver',
                'total_persons')
  names(df_popn) <- colnames
  new_df_popn = df_popn[, c('state_id',
                            'state_name',
                            'lga_code',
                            'lga_name')]
  new_df_popn$ages35andUp = get_total_from_index(df_popn, 12)
  new_df_popn$ages40andUp = get_total_from_index(df_popn, 13)
  new_df_popn$ages45andUp = get_total_from_index(df_popn, 14)
  new_df_popn$ages50andUp = get_total_from_index(df_popn, 15)
  new_df_popn$ages55andUp = get_total_from_index(df_popn, 16)
  new_df_popn$ages60andUp = get_total_from_index(df_popn, 17)
  new_df_popn$ages65andUp = get_total_from_index(df_popn, 18)
  new_df_popn$ages70andUp = get_total_from_index(df_popn, 19)
  new_df_popn$ages75andUp = get_total_from_index(df_popn, 20)
  new_df_popn$ages80andUp = get_total_from_index(df_popn, 21)
  new_df_popn$ages85andUp = get_total_from_index(df_popn, 22)
  print(head(new_df_popn))
  
  ogr_geoj <- readOGR(filepath_geoj, layer)
  ogr_geoj@data <- sp::merge(ogr_geoj@data, df_popn , by.x = "LGA_CODE11", by.y = "lga_code", all.x = TRUE)
  writeOGR(ogr_geoj, new_filepath_geoj,'spDf', driver = 'GeoJSON', check_exists = FALSE)
}

get_total_from_index <- function(df, index){
  total_cols = ncol(df) - 1
  
  df$newcol = 0
  for (i in index:total_cols){
    df$newcol = df$newcol + df[, i]
  }

  return(df$newcol)
}

wd <- '/Users/vivi/Documents/Git/annalect_probset/aged_care_viz/aged_care_viz/au_aged_care'
# df_main <- preprocess_au_service_list(file.path(wd, 'Australia-30-June-2019-v2-1.csv'),
#                                       file.path(wd, 'Australian_Post_Codes_Lat_Lon.csv'))
# write.csv(df_main, file.path(wd, 'output.csv'), quote=FALSE)

# df_main <- add_popn_to_geoj(file.path(wd, 'lga11aAust_0.1.geojson'),
#                             'lga11aAust_0.1',
#                             file.path(wd, 'population_by_age.csv'),
#                             file.path(wd, 'lga11aAust_final.geojson'))
