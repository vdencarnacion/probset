library(rgdal)      # Spatial data processing
library(jsonlite)   # Read json files
library(readr)      # Fast I/O

# Postcode shape json
gdal.postcodes <- readOGR(dsn="/Users/vivi/Documents/Git/annalect_probset/aged_care_viz/aged_care_viz/au_aged_care/au-postcodes-Visvalingam-0.1.geojson", layer="au-postcodes-Visvalingam-0.1")

# Read data you want to join to the shape file
# df <- read_csv("your_data.csv")

# Change to character for leading 0s in NT postcodes
# df$cleaned_postcode <- sprintf("%04d", df$postcode)

# Join agg values onto postcode (returns)
# gdal.postcodes@data <- sp::merge(gdal.postcodes@data, df , by.x = "POA_NAME", by.y = "cleaned_postcode", all.x = TRUE)

# Write to a new file
writeOGR(gdal.postcodes, '/Users/vivi/Documents/Git/annalect_probset/aged_care_viz/aged_care_viz/au_aged_care/au-postcodes-0.2.geojson','spDf', driver = 'GeoJSON', check_exists = FALSE)
