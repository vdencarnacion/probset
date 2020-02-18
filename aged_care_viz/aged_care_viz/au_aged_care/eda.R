library(tidyverse)
library(dplyr)
library(ggplot2)

get_total_from_index <- function(df, index, max_index){
  df$newcol = 0
  for (i in index:max_index){
    df$newcol = df$newcol + df[, i]
  }
  # print(df$newcol)
  return(df$newcol)
}

# USER INPUT
wd <- '/Users/vivi/Documents/Git/annalect_probset/aged_care_viz/aged_care_viz/au_aged_care'
wd <- '~/vivi/annalect_probset/aged_care_viz/aged_care_viz/au_aged_care'
filepath_popn = file.path(wd, 'population_by_age.csv')
filepath_acu = file.path(wd, 'Australia-30-June-2019-v2-1.csv')

# READ DATA
df_popn = read.csv(filepath_popn, stringsAsFactors = FALSE)
df_acu = read.csv(filepath_acu, stringsAsFactors = FALSE)

# CHANGE COLUMN NAMES
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
names(df_acu) <- colnames

# ADD ABV FOR STATE NAMES
df_popn$abv_state = df_popn$state_name
df_popn$abv_state[df_popn$state_name=='New South Wales'] <- 'NSW'
df_popn$abv_state[df_popn$state_name=='South Australia'] <- 'SA'
df_popn$abv_state[df_popn$state_name=='Victoria'] <- 'VIC'
df_popn$abv_state[df_popn$state_name=='Western Australia'] <- 'WA'
df_popn$abv_state[df_popn$state_name=='Northern Territory'] <- 'NT'
df_popn$abv_state[df_popn$state_name=='Tasmania'] <- 'TAS'
df_popn$abv_state[df_popn$state_name=='Australian Capital Territory'] <- 'ACT'
df_popn$abv_state[df_popn$state_name=='Queensland'] <- 'QLD'
df_popn$abv_state[df_popn$state_name=='Other Territories'] <- 'OT'
# print(unique(df_popn$abv_state))
print(unique(df_acu$state))
# print(df_popn[df_popn$state_name=='Other Territories',])

# PROCESS LGA NAME
df_popn$lga_new = toupper(sub(' \\(.*\\)', '', df_popn$lga_name))

# GET TOTAL POPULATION - STATE-LEVEL
df_popn_by_state = 
  df_popn %>%
  group_by(abv_state) %>%
  summarise_at(c('ages00to04',
                 'ages05to09',
                 'ages10to14',
                 'ages15to19',
                 'ages20to24',
                 'ages25to29',
                 'ages30to34',
                 'ages35to39',
                 'ages40to44',
                 'ages45to49',
                 'ages50to54',
                 'ages55to59',
                 'ages60to64',
                 'ages65to69',
                 'ages70to74',
                 'ages75to79',
                 'ages80to84',
                 'ages85andOver'), sum) %>%
  data.frame()

# COMPUTE FOR MORE AGGREGATED AGES
df_popn_by_state$ages35andUp = get_total_from_index(df_popn_by_state, 9, 19)
df_popn_by_state$ages40andUp = get_total_from_index(df_popn_by_state, 10, 19)
df_popn_by_state$ages45andUp = get_total_from_index(df_popn_by_state, 11, 19)
df_popn_by_state$ages50andUp = get_total_from_index(df_popn_by_state, 12, 19)
df_popn_by_state$ages55andUp = get_total_from_index(df_popn_by_state, 13, 19)
df_popn_by_state$ages60andUp = get_total_from_index(df_popn_by_state, 14, 19)
df_popn_by_state$ages65andUp = get_total_from_index(df_popn_by_state, 15, 19)
df_popn_by_state$ages70andUp = get_total_from_index(df_popn_by_state, 16, 19)
df_popn_by_state$ages75andUp = get_total_from_index(df_popn_by_state, 17, 19)
df_popn_by_state$ages80andUp = get_total_from_index(df_popn_by_state, 18, 19)
df_popn_by_state$ages85andUp = get_total_from_index(df_popn_by_state, 19, 19)
df_popn_by_state = df_popn_by_state[, c('abv_state',
                                        # 'ages35andUp',
                                        # 'ages40andUp',
                                        # 'ages45andUp',
                                        # 'ages50andUp',
                                        # 'ages55andUp',
                                        'ages60andUp',
                                        'ages65andUp',
                                        'ages70andUp',
                                        'ages75andUp',
                                        'ages80andUp',
                                        'ages85andUp')]
write_csv(df_popn_by_state, file.path(wd, 'df_popn_by_state_aggregated.csv'), quote=FALSE)

# GET TOTAL POPULATION - LGA LEVEL
df_popn_by_lga =
  df_popn %>%
  group_by(lga_new) %>%
  summarise_at(c('ages00to04',
                 'ages05to09',
                 'ages10to14',
                 'ages15to19',
                 'ages20to24',
                 'ages25to29',
                 'ages30to34',
                 'ages35to39',
                 'ages40to44',
                 'ages45to49',
                 'ages50to54',
                 'ages55to59',
                 'ages60to64',
                 'ages65to69',
                 'ages70to74',
                 'ages75to79',
                 'ages80to84',
                 'ages85andOver'), sum) %>%
  data.frame()
# print(head(df_popn_by_lga))

# COMPUTE FOR MORE AGGREGATED AGES
df_popn_by_lga$ages35andUp = get_total_from_index(df_popn_by_lga, 9, 19)
df_popn_by_lga$ages40andUp = get_total_from_index(df_popn_by_lga, 10, 19)
df_popn_by_lga$ages45andUp = get_total_from_index(df_popn_by_lga, 11, 19)
df_popn_by_lga$ages50andUp = get_total_from_index(df_popn_by_lga, 12, 19)
df_popn_by_lga$ages55andUp = get_total_from_index(df_popn_by_lga, 13, 19)
df_popn_by_lga$ages60andUp = get_total_from_index(df_popn_by_lga, 14, 19)
df_popn_by_lga$ages65andUp = get_total_from_index(df_popn_by_lga, 15, 19)
df_popn_by_lga$ages70andUp = get_total_from_index(df_popn_by_lga, 16, 19)
df_popn_by_lga$ages75andUp = get_total_from_index(df_popn_by_lga, 17, 19)
df_popn_by_lga$ages80andUp = get_total_from_index(df_popn_by_lga, 18, 19)
df_popn_by_lga$ages85andUp = get_total_from_index(df_popn_by_lga, 19, 19)
df_popn_by_lga = df_popn_by_lga[, c('lga_new',
                                    # 'ages35andUp',
                                    # 'ages40andUp',
                                    # 'ages45andUp',
                                    # 'ages50andUp',
                                    # 'ages55andUp',
                                    'ages60andUp',
                                    'ages65andUp',
                                    'ages70andUp',
                                    'ages75andUp',
                                    'ages80andUp',
                                    'ages85andUp')]
# print(head(df_popn_by_lga))
write_csv(df_popn_by_lga, file.path(wd, 'df_popn_by_lga_aggregated.csv'), quote=FALSE)

# AGGREGATE ACU BY STATE
df_acu$home_care_places[is.na(df_acu$home_care_places)] <- 0
df_acu$residential_places[is.na(df_acu$residential_places)] <- 0
df_acu$restorative_care_places[is.na(df_acu$restorative_care_places)] <- 0
df_acu$total = df_acu$home_care_places + df_acu$residential_places + df_acu$restorative_care_places
df_acu$count = 1

df_acu_by_state = 
  df_acu %>%
  group_by(state) %>%
  summarise_at(c('count',
                 'home_care_places',
                 'residential_places',
                 'restorative_care_places',
                 'total'),
               sum) %>%
  data.frame()
df_acu_by_state$hcp_perc = df_acu_by_state$home_care_places/sum(df_acu_by_state$home_care_places)
df_acu_by_state$rp_perc = df_acu_by_state$residential_places/sum(df_acu_by_state$residential_places)
df_acu_by_state$rcp_perc = df_acu_by_state$restorative_care_places/sum(df_acu_by_state$restorative_care_places)
df_acu_by_state$total_perc = df_acu_by_state$total/sum(df_acu_by_state$total)
write_csv(df_acu_by_state, file.path(wd, 'df_acu_by_state_aggregated.csv'), quote=FALSE)

# print(unique(df_acu$residential_places )).
# print(head(df_popn))
print(head(df_acu))
# print(df_popn_by_state)
# print(df_acu_by_state)

# AGGREGATE ACU BY SUBURB (LGA?)
df_acu_by_suburb = 
  df_acu %>%
  group_by(state, suburb) %>%
  summarise_at(c('count',
                 'home_care_places',
                 'residential_places',
                 'restorative_care_places',
                 'total'),
               sum) %>%
  data.frame()
df_acu_by_suburb$hcp_perc = df_acu_by_suburb$home_care_places/sum(df_acu_by_suburb$home_care_places)
df_acu_by_suburb$rp_perc = df_acu_by_suburb$residential_places/sum(df_acu_by_suburb$residential_places)
df_acu_by_suburb$rcp_perc = df_acu_by_suburb$restorative_care_places/sum(df_acu_by_suburb$restorative_care_places)
df_acu_by_suburb$total_perc = df_acu_by_suburb$total/sum(df_acu_by_suburb$total)
write_csv(df_acu_by_suburb, file.path(wd, 'df_acu_by_suburb.csv'), quote=FALSE)
# print(head(df_acu_by_suburb))
