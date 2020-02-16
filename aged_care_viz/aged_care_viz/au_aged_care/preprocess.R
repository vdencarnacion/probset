library(rgdal)

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
               'australian_government_funding_2016_17')
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

wd <- '/Users/vivi/Documents/Git/annalect_probset/aged_care_viz/aged_care_viz/au_aged_care'
# df_main <- preprocess_au_service_list(file.path(wd,
#                                                 'Australia-30-June-2019-v2-1.csv'),
#                                       file.path(wd,
#                                                 'Australian_Post_Codes_Lat_Lon.csv'))
# write.csv(df_main, file.path(wd, 'output.csv'), quote=FALSE)

# age_density <- tbl(src_sqlite(file.path(wd,
#                                         "./SA2 ERP by Age and Sex GeoPackage 2018.gpkg")), "age_density")
# print(age_density, n = 5)
