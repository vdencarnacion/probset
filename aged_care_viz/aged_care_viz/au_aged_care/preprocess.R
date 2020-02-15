
preprocess_au_service_list <- function(filename){
  # read file
  df_main <- read.csv(filename)
  
  # change column names
  
  colnames = c('service_name',
               'physical_address_line_1',
               'physical_address_2',
               'physical_address_suburb',
               'physical_address_state',
               'physical_address_post_code',
               'aged_care_planning_region_acpr_2015',
               'care_type',
               'residential_places',
               'home_care_places',
               'restorative_care_places',
               'provider_name',
               'organisation_type',
               'abs_remoteness',
               'lat',
               'long',
               'australian_government_funding_2016_17')
  names(df_main) <- colnames
  
  # preprocess
  df_main = df_main[, c('service_name',
                        'lat',
                        'long',
                        'home_care_places')]
  df_main$home_care_places[is.na(df_main$home_care_places)] <- 1
  
  print(head(df_main))
  return (df_main)
}
