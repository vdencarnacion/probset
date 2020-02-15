
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
  # print(head(df_geoj))
  df_geoj = df_geoj[, c('service_name',
                        'postcode',
                        'suburb',
                        'state',
                        'geoj_lat',
                        'geoj_lon',
                        'home_care_places')]
  df_geoj$home_care_places[is.na(df_geoj$home_care_places)] <- 1
  return (df_geoj)
  
  # join df_geoj with df_postcodes
  # df_merged <- merge(x=df_geoj, y=df_postcodes, by=c('postcode',
  #                                                    'suburb',
  #                                                    'state'), all.x=TRUE)
  # df_merged <- df_merged[which(!is.null(df_merged$postcode_lat) &
  #                                !is.null(df_merged$postcode_lon) &
  #                                !is.null(df_merged$geoj_lat) &
  #                                !is.null(df_merged$geoj_lon)
  #                              )]

  # print(df_merged[which(is.null(df_merged$postcode_lat | df_merged$postcode_lon))])
  # print('df_merged', str(nrow(df_merged)))
  # print('df_geoj', str(nrow(df_geoj)))
  # print('df_geoj', str(nrow(df_postcodes)))
  
  # print(df_merged[which(is.na(df_merged$postcode_lon)),])
  # empty <- is.na(df_merged$postcode_lon)
  # print(empty)
  # df_merged$postcode_lon[empty] <- df_merged$geoj_lon[empty]
  # df_merged$postcode_lat[empty] <- df_merged$geoj_lat[empty]
  
  # df_merged <- df_merged[!(is.null(df_merged$postcode_lat)),]
  # df_merged <- df_merged[!(is.null(df_merged$postcode_lon)),]
  # print(head(df_merged))
  # return (df_merged)
}

wd <- '/Users/vivi/Documents/Git/annalect_probset/aged_care_viz/aged_care_viz/au_aged_care'
df_main <- preprocess_au_service_list(file.path(wd,
                                                'Australia-30-June-2019-v2-1.csv'),
                                      file.path(wd,
                                                'Australian_Post_Codes_Lat_Lon.csv'))
# print(head(df_main))
# print(nrow(df_main))
# print(df_main[which(is.na(df_main$postcode_lon)),])
write.csv(df_main, file.path(wd, 'output.csv'), quote=FALSE)
